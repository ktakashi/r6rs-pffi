;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/struct/helper.sls - Foreign structure helper
;;;
;;;   Copyright (c) 2015-2025  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
#!r6rs
(library (pffi struct helper)
    (export ->ctr&pred ->sizeof ->sizeofs
	    make-process-clauses make->type-name
	    alignment struct 

	    default-protocol ;; because of Guile...
	    make-constructor
	    make-union-constructor)
    (import (for (rnrs) run expand (meta -1))
	    (pffi ffi-type-descriptor)
	    (only (pffi misc) take drop split-at))

;; keyword to distinguish type (for Chez)
(define-syntax struct (syntax-rules ()))
(define-syntax alignment (syntax-rules ()))

;; Guile doesn't understand (meta -1) so passing the required free identifiers
;; explicitly here (i.e. fields, parent, protocol and alignment)
(define-syntax make-process-clauses
  (syntax-rules ()
    ((_  x who (fields parent protocol alginment))
     (lambda (k name clauses)
       (define (process-fields k type-name ofields)
	 (let loop ((fields ofields) (r '()))
	   (syntax-case fields ()
             (() (reverse r))
             (((type name) . rest)
	      (with-syntax (((ref set) (->ref&set! k type-name #'name)))
		(loop #'rest (cons #'(type name ref set) r))))
             (((type name ref) . rest)
	      (with-syntax (((ignore set) (->ref&set! k type-name #'name)))
		(loop #'rest (cons #'(type name ref set) r))))
             (((type name ref set) . rest)
	      (loop #'rest (cons #'(type name ref set) r)))
	     (_ (syntax-violation who "Invalid field declaration"
				  x (car fields))))))
       (let loop ((clauses clauses) (fs #f) (par #f) (proto #f) (align #f))
	 (syntax-case clauses (fields parent protocol alginment)
	   (() (list (or fs '()) par proto align))
	   (((fields defs (... ...)) . rest)
	    (or (not fs)
		(syntax-violation who "only one fields clause allowed" x
				  (car clauses)))
	    (loop #'rest
		  (process-fields k name #'(defs (... ...))) par proto align))
	   (((parent p) . rest)
	    (or (not par)
		(syntax-violation who "only one parent clause allowed" x
				  (car clauses)))
	    (loop #'rest fs #'p proto align))
	   (((protocol p) . rest)
	    (or (not proto)
		(syntax-violation who "only one protocol clause allowed" x
				  (car clauses)))
	    (loop #'rest fs par #'p align))
	   (((alignment a) . rest)
	    (or (not align)
		(syntax-violation who "only one alignment clause allowed" x
				  (car clauses)))
	    (if (identifier? #'a)
		(loop #'rest fs par proto #'a)
		;; Apparently, PLT R6RS creates syntax object against
		;; number / string
		(loop #'rest fs par proto (syntax->datum #'a))))
	   (_ (syntax-violation who "invalid clause" x (car clauses)))))))))

(define-syntax make->type-name
  (syntax-rules ()
    ((_ who (struct))
     (lambda (names)
       (let loop ((names names) (r '()))
	 (syntax-case names (struct)
	   (() (reverse r))
	   ((type rest (... ...))
	    (identifier? #'type)
	    (loop #'(rest (... ...)) (cons #'type r)))
	   (((struct type) rest (... ...))
	    (identifier? #'type)
	    (loop #'(rest (... ...)) (cons #'type r)))
	   (name (syntax-violation who "invalid type name" names #'name))))))))

(define (->ctr&pred k name)
  (datum->syntax k (list (->name "make-" name "") (->name "" name "?"))))

(define (->sizeof k name) (datum->syntax k (->name "size-of-" name "")))

(define (->sizeofs k types)
  (let loop ((types types) (r '()))
    (syntax-case types ()
      (() (datum->syntax k (reverse r)))
      ((a . d)
       (loop #'d
	     (cons (list (syntax->datum #'a) (->name "size-of-" #'a "")) r))))))

(define (->ref&set! k name field)
  (define s (symbol->string (syntax->datum field)))
  (datum->syntax k (list (->name "" name (string-append "-" s))
			 (->name "" name (string-append "-" s "-set!")))))

(define (->name prefix name suffix)
  (let ((base (symbol->string (syntax->datum name))))
    (string->symbol (string-append prefix base suffix))))

;; the same thing as r6rs defines...

(define (total-field-count desc)
  (let loop ((desc desc) (r 0))
    (if desc
        (loop (foreign-struct-descriptor-parent desc)
              (+ (length (foreign-struct-descriptor-fields desc)) r))
        r)))

(define (make-struct desc field-values)
  (define (set-parent-fields desc bv field-values)
    (define (->ordered-paretns desc)
      (let loop ((p (foreign-struct-descriptor-parent desc)) (r '()))
        (if p
            (loop (foreign-struct-descriptor-parent p) (cons p r))
            (reverse r))))
    (let loop ((parents (->ordered-paretns desc))
               (field-values field-values))
      (if (null? parents)
          field-values
          (let* ((setters (foreign-struct-descriptor-setters (car parents)))
                 (len (length setters)))
            (for-each (lambda (set arg) (set bv arg))
                      setters (take field-values len))
            (loop (cdr parents) (drop field-values len))))))
  (let ((setters (foreign-struct-descriptor-setters desc))
        (bv (make-bytevector (foreign-struct-descriptor-size desc) 0)))
    (let ((field-values (set-parent-fields desc bv field-values)))
      (for-each (lambda (set arg) (set bv arg)) setters field-values)
      bv)))

(define (make-simple-conser protocol desc argc)
  (protocol
   (lambda field-values
     (if (= (length field-values) argc)
         (make-struct desc field-values)
         (assertion-violation "struct constructor"
                              "wrong number of arguments"
                              field-values)))))

(define (make-nested-conser protocol odesc argc)
  (protocol
   ((let loop ((desc odesc))
      (cond ((foreign-struct-descriptor-parent desc)
             => (lambda (parent)
                  (lambda extra-field-values
                    (lambda protocol-args
                      (lambda this-field-values
                        (apply ((foreign-struct-descriptor-protocol parent)
                                (apply (loop parent)
                                       (append this-field-values
                                               extra-field-values)))
                               protocol-args))))))
            (else
             (lambda extra-field-values
               (lambda this-field-values
                 (let ((field-values (append this-field-values
                                             extra-field-values)))
                   (if (= (length field-values) argc)
                       (make-struct odesc field-values)
                       (assertion-violation "struct constructor"
                                            "wrong number of arguments"
                                            field-values)))))))))))

(define (default-protocol desc)
  (let ((parent (foreign-struct-descriptor-parent desc)))
    (if parent
        (let ((parent-field-count (total-field-count parent)))
          (lambda (p)
            (lambda field-values
              (let-values (((parent-field-values this-field-values)
                            (split-at field-values parent-field-count)))
                (let ((n (apply p parent-field-values)))
                  (apply n this-field-values))))))
        (lambda (p)
          (lambda field-values
            (apply p field-values))))))

;; TODO implement it properly...
(define (make-constructor desc protocol)
  (let ((parent? (foreign-struct-descriptor-parent desc))
        (protocol (or protocol (default-protocol desc))))
    (if parent?
        ;; check parent protocol
        (begin
          (when (and (foreign-struct-descriptor-has-protocol? parent?)
                     (not (foreign-struct-descriptor-has-protocol? desc)))
            (assertion-violation 'make-constructor
                                 "parent has custom protocol" desc))
          (make-nested-conser protocol desc
                              (total-field-count desc)))
        (make-simple-conser protocol desc
                            (length (foreign-struct-descriptor-fields desc))))))

(define (make-union-constructor desc protocol)
  (define fields (map car (foreign-struct-descriptor-fields desc)))
  (define sizeof (foreign-struct-descriptor-size desc))
  (define (custom-ctr . field&value)
    (define f (and (not (null? field&value)) (car field&value)))
    (define v (and (not (null? field&value))
                   (not (null? (cdr field&value)))
                   (cadr field&value)))
    (define setters (foreign-struct-descriptor-setters desc))
    (let ((r (make-bytevector sizeof 0)))
      ;; a bit inefficient...
      (when (and f v)
        (do ((i 0 (+ i 1)) (f* fields (cdr f*)))
            ((or (null? f*) (eq? (car f*) f))
             (unless (null? f*)
               (let ((s (list-ref setters i)))
                 (s r v))))
	  ))
      r))
  (if protocol
      (protocol custom-ctr)
      (lambda () (make-bytevector sizeof 0))))

)
