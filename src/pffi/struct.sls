;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/struct.sls - Foreign structure
;;;
;;;   Copyright (c) 2015-2019  Takashi Kato  <ktakashi@ymail.com>
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

;; Foreign structure is basically chunk of memory and in this
;; library we represent it as bytevector. (this is because
;; not all implementation have GCable memory allocation.)
;; To pass it to foreign procedures, it needs to be converted
;; to a pointer by bytevector->pointer.

(library (pffi struct)
    (export define-foreign-struct
            define-foreign-union
	    fields parent protocol alignment struct)
    (import (rnrs)
            (pffi compat)
	    (pffi ffi-type-descriptor)
	    (for (pffi struct helper) run expand)
            (for (only (pffi misc) take drop split-at) run expand))

;; use fields, protocol and parent from (rnrs)
;; e.g.
;; (define-foreign-struct foreign-vector
;;   (fields (int size)
;;           (pointer elements))
;;   (protocol
;;     (lambda (p)
;;       (lambda (size)
;;         (p size (bytevector->pointer (make-bytevector size)))))))
(define-syntax define-foreign-struct
  (lambda (x)
    (define process-clauses
      (make-process-clauses x 'define-foreign-struct
			    (fields parent protocol alginment)))
    (define ->type-name (make->type-name 'define-foreign-struct (struct)))
    (syntax-case x ()
      ((k (name ctr pred) specs ...)
       (and (identifier? #'name) (identifier? #'ctr) (identifier? #'pred))
       ;; collect fields, parent and protocol
       (with-syntax (((((type field ref set) ...) parent protocol align)
                      (process-clauses #'k #'name #'(specs ...)))
                     (sizeof (->sizeof #'k #'name)))
	 (with-syntax (((type ...) (->type-name  #'(type ...))))
           #'(begin
	       (define alignment-check
                 (unless (or (not align) (memv align '(1 2 4 8 16)))
                   (assertion-violation 'define-foreign-struct
                                        "alignment must be  1, 2, 4, 8, or 16"
                                        align)))
	       (define sizeof (compute-size parent (list type ...) align))
	       (define this-protocol protocol)
	       (define (pred o)
                 (and (bytevector? o)
		      (>= (bytevector-length o) sizeof)))
	       (define name
                 (make-generic-foreign-struct-descriptor
                  'name
                  sizeof
                  (struct-alignment (list type ...))
                  (list (cons 'field type) ...)
                  parent
                  this-protocol
                  ;; type-ref
                  (lambda (o offset)
                    (let ((bv (make-bytevector sizeof)))
		      (bytevector-copy! o offset bv 0 sizeof)
		      bv))
                  ;; type-set!
                  (lambda (o offset v)
                    (bytevector-copy! v 0 o offset sizeof))))
	       (define ctr (make-constructor name this-protocol))
	       (define ref
                 (let ((acc (type->ref type))
		       (offset (compute-offset name 'field align)))
                   (lambda (o) (acc o offset))))
	       ...
	       (define set
                 (let ((acc (type->set! type))
		       (offset (compute-offset name 'field align)))
                   (lambda (o v) (acc o offset v))))
	       ...
	       (define dummy
		 (begin
		   (foreign-struct-descriptor-ctr-set! name ctr)
		   (foreign-struct-descriptor-getters-set! name (list ref ...))
		   (foreign-struct-descriptor-setters-set! name (list set ...))
		   (foreign-struct-descriptor-protocol-set! name
		    (or this-protocol (default-protocol name)))
		   name))))))
      ((k name specs ...)
       (identifier? #'name)
       (with-syntax (((ctr pred) (->ctr&pred #'k #'name)))
         #'(k (name ctr pred) specs ...))))))

(define-syntax define-foreign-union
  (lambda (x)
    (define process-clauses
      (make-process-clauses x 'define-foreign-union
			    (fields parent protocol alginment)))
    (define ->type-name (make->type-name 'define-foreign-union (struct)))
    (syntax-case x ()
      ((k (name ctr pred) specs ...)
       (and (identifier? #'name) (identifier? #'ctr) (identifier? #'pred))
       ;; collect fields, parent and protocol
       (with-syntax (((((type field ref set) ...) parent protocol alignment)
		      (process-clauses #'k #'name #'(specs ...)))
                     (sizeof (->sizeof #'k #'name)))
	 (when #'parent
	   (syntax-violation 'define-foreign-union "Union can't have parent" x))
	 (when #'alignment
	   (syntax-violation 'define-foreign-union "Union can't have alignment" x))
	 (with-syntax (((type ...) (->type-name  #'(type ...))))
           #'(begin
	       ;; the same as alignment :)
	       (define sizeof (struct-alignment (list type ...)))
	       (define this-protocol protocol)
	       ;; sub struct
	       (define (pred o)
                 (and (bytevector? o)
		      (>= (bytevector-length o) sizeof)))
	       (define name
                 (make-generic-foreign-struct-descriptor
                  'name
                  sizeof
                  (struct-alignment (list type ...))
                  (list (cons 'field type) ...)
                  parent
                  this-protocol
                  ;; type-ref
                  (lambda (o offset)
                    (let ((bv (make-bytevector sizeof)))
		      (bytevector-copy! o offset bv 0 sizeof)
		      bv))
                  ;; type-set!
                  (lambda (o offset v)
                    (bytevector-copy! v 0 o offset sizeof))))
	       (define ctr (make-union-constructor name this-protocol))
	       (define ref
                 (let ((acc (type->ref type)))
                   (lambda (o) (acc o 0))))
	       ...
	       (define set
                 (let ((acc (type->set! type)))
                   (lambda (o v) (acc o 0 v))))
	       ...
	       (define dummy
		 (begin
		   (foreign-struct-descriptor-ctr-set! name ctr)
		   (foreign-struct-descriptor-getters-set! name (list ref ...))
		   (foreign-struct-descriptor-setters-set! name (list set ...))
		   (foreign-struct-descriptor-protocol-set! name
		    (or this-protocol (default-protocol name)))
		   name))))))
      ((k name specs ...)
       (identifier? #'name)
       (with-syntax (((ctr pred) (->ctr&pred #'k #'name)))
         #'(k (name ctr pred) specs ...))))))

(define (check-primitive t) 
  (if (ffi-type-descriptor? t)
      (let ((a (ffi-type-descriptor-alias t)))
	(if (ffi-type-descriptor? a)
	    (check-primitive a)
	    (ffi-type-descriptor-name t)))
      t))

(define (struct-alignment lis)
  (if (null? lis)
      0
      (apply max (map (lambda (l)
			(if (foreign-struct-descriptor? l)
                            (generic-foreign-struct-descriptor-alignment l)
                            (type-descriptor-size l))) lis))))

;; ref
;;  http://en.wikipedia.org/wiki/Data_structure_alignment
(define (compute-size parent types alignment)
  (define list-of-sizeofs
    (map (lambda (t) (cons t (type-descriptor-size t))) types))
  (define-syntax padding
    (syntax-rules ()
      ((_ o a) ;; offset align
       (if (and alignment (zero? (mod o alignment)))
           0
           (bitwise-and (- o) (- a 1))))))

  (let ((parent-size (if parent (foreign-struct-descriptor-size parent) 0))
        (align (if parent (generic-foreign-struct-descriptor-alignment parent) 0)))
    ;; assume primitive types have the same alignment as its size
    ;; (according to the Wikipedia page, linix may have different value
    ;;  for double but never seen it)
    ;; struct manages own alignment
    (let loop ((sizes list-of-sizeofs)
               (max-size align)
               (size parent-size))
      (if (null? sizes)
          ;; fixup
          (let ((m (if alignment (- alignment 1) (- max-size 1))))
            (bitwise-and (+ size m) (bitwise-not m)))
          (let ((s (car sizes)))
            (if (foreign-struct-descriptor? (car s))
                (let* ((sa (generic-foreign-struct-descriptor-alignment (car s)))
                       (ss (cdr s))
                       (size (+ size ss))
                       (pad (padding size sa)))
                  (loop (cdr sizes)
                        (if (> sa max-size) sa max-size)
                        (+ size pad)))
                ;; primitive
                (let* ((ps (cdr s))
                       (size (+ size ps))
                       (pad (padding size ps)))
                  (loop (cdr sizes)
                        (if (> ps max-size) ps max-size)
                        (+ size pad)))))))))

;; FIXME I'm not sure if I'm doing correctly here but works
;;       (tests are passing...)
(define (compute-offset descriptor field align)
  (define-syntax offset
    (syntax-rules ()
      ((_ o a) ;; offset align
       (let* ((off o)
              (ali (if align align a))
              (n (+ off ali)))
         (bitwise-and (- n 1) (bitwise-not (- ali 1)))))))
  (define-syntax padding
    (syntax-rules ()
      ((_ o a) ;; offset align
       (let ((ali a) (off o))
         (if (and align (zero? (mod off align)))
             0
             (bitwise-and (- off) (- a 1)))))))
  (define (compute-next-size size f)
    (let* ((s (type-descriptor-size (cdr f)))
           (size (+ size s))
           (a (if (foreign-struct-descriptor? (cdr f))
                  (generic-foreign-struct-descriptor-alignment (cdr f))
                  s))
           (pad (padding size a)))
      (values (+ s pad) (+ size pad))))
  (define parent-align (if (foreign-struct-descriptor-parent descriptor)
                           (generic-foreign-struct-descriptor-alignment
                            (foreign-struct-descriptor-parent descriptor))
                           0))
  (define (alignment field)
    (if (foreign-struct-descriptor? (cdr field))
        (generic-foreign-struct-descriptor-alignment (cdr field))
        (type-descriptor-size (cdr field))))

  (let loop ((fields (foreign-struct-descriptor-fields descriptor))
             ;; well...
             (size (if (foreign-struct-descriptor-parent descriptor)
                       (foreign-struct-descriptor-size
                        (foreign-struct-descriptor-parent descriptor))
                       0))
             ;; rough next offset
             (off (offset 0 parent-align)))
    (if (null? fields)
        (assertion-violation 'compute-offset "invalid field name" field)
        (let ((f (car fields)))
          (if (eq? field (car f))
              (if (foreign-struct-descriptor? (cdr f))
                  (offset off (generic-foreign-struct-descriptor-alignment
			       (cdr f)))
                  (let-values (((ignore next-size) (compute-next-size size f)))
                    (- next-size (alignment f))))
              (let-values (((s next-size) (compute-next-size size f)))
                ;; rough next offset = current offset + s
                (loop (cdr fields) next-size (+ off s))))))))

(define char-ref           bytevector-s8-ref)
(define unsigned-char-ref  bytevector-u8-ref)
(define short-ref          bytevector-s16-native-ref)
(define unsigned-short-ref bytevector-u16-native-ref)
(define int-ref            bytevector-s32-native-ref)
(define unsigned-int-ref   bytevector-u32-native-ref)
(define long-ref
  (if (= size-of-long 4) bytevector-s32-native-ref bytevector-s64-native-ref))
(define unsigned-long-ref
  (if (= size-of-long 4) bytevector-u32-native-ref bytevector-u64-native-ref))
(define float-ref          bytevector-ieee-single-native-ref)
(define double-ref         bytevector-ieee-double-native-ref)
(define int8_t-ref         bytevector-s8-ref)
(define uint8_t-ref        bytevector-u8-ref)
(define int16_t-ref        bytevector-s16-native-ref)
(define uint16_t-ref       bytevector-u16-native-ref)
(define int32_t-ref        bytevector-s32-native-ref)
(define uint32_t-ref       bytevector-u32-native-ref)
(define int64_t-ref        bytevector-s64-native-ref)
(define uint64_t-ref       bytevector-u64-native-ref)
(define (pointer-ref o offset)
  (integer->pointer
   (if (= size-of-pointer 4)
       (bytevector-u32-ref o offset (native-endianness))
       (bytevector-u64-ref o offset (native-endianness)))))

(define char-set!           bytevector-s8-set!)
(define unsigned-char-set!  bytevector-u8-set!)
(define short-set!          bytevector-s16-native-set!)
(define unsigned-short-set! bytevector-u16-native-set!)
(define int-set!            bytevector-s32-native-set!)
(define unsigned-int-set!   bytevector-u32-native-set!)
(define long-set!
  (if (= size-of-long 4) bytevector-s32-native-set! bytevector-s64-native-set!))
(define unsigned-long-set!
  (if (= size-of-long 4) bytevector-u32-native-set! bytevector-u64-native-set!))
(define float-set!          bytevector-ieee-single-native-set!)
(define double-set!         bytevector-ieee-double-native-set!)
(define int8_t-set!         bytevector-s8-set!)
(define uint8_t-set!        bytevector-u8-set!)
(define int16_t-set!        bytevector-s16-native-set!)
(define uint16_t-set!       bytevector-u16-native-set!)
(define int32_t-set!        bytevector-s32-native-set!)
(define uint32_t-set!       bytevector-u32-native-set!)
(define int64_t-set!        bytevector-s64-native-set!)
(define uint64_t-set!       bytevector-u64-native-set!)
(define (pointer-set! o offset v)
  (let ((bv (uint-list->bytevector
             (list (pointer->integer v))
             (native-endianness)
             size-of-pointer)))
    (bytevector-copy! bv 0 o offset size-of-pointer)))

(define *accessors*
  `((char           ,char-ref           ,char-set!)
    (unsigned-char  ,unsigned-char-ref  ,unsigned-char-set!)
    (short          ,short-ref          ,short-set!)
    (unsigned-short ,unsigned-short-ref ,unsigned-short-set!)
    (int            ,int-ref            ,int-set!)
    (unsigned-int   ,unsigned-int-ref   ,unsigned-int-set!)
    (long           ,long-ref           ,long-set!)
    (unsigned-long  ,unsigned-long-ref  ,unsigned-long-set!)
    (float          ,float-ref          ,float-set!)
    (double         ,double-ref         ,double-set!)
    (int8_t         ,int8_t-ref         ,int8_t-set!)
    (uint8_t        ,uint8_t-ref        ,uint8_t-set!)
    (int16_t        ,int16_t-ref        ,int16_t-set!)
    (uint16_t       ,uint16_t-ref       ,uint16_t-set!)
    (int32_t        ,int32_t-ref        ,int32_t-set!)
    (uint32_t       ,uint32_t-ref       ,uint32_t-set!)
    (int64_t        ,int64_t-ref        ,int64_t-set!)
    (uint64_t       ,uint64_t-ref       ,uint64_t-set!)
    (pointer        ,pointer-ref        ,pointer-set!)))


(define (type->set! type)
  (let ((n (check-primitive type)))
    (cond ((assq n *accessors*) => caddr)
          (else (generic-foreign-struct-descriptor-type-set! type)))))

(define (type->ref type)
  (let ((n (check-primitive type)))
    (cond ((assq n *accessors*) => cadr)
          (else (generic-foreign-struct-descriptor-type-ref type)))))

)
