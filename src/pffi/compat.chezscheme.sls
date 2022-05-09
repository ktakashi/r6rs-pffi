;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/compat.chezscheme.sls - Compatible layer for Chez Scheme
;;;
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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

;; this file provides compatible layer for (pffi procedure)
;; if implementations can't make this layer, then make
;; pffi/procedure.$name.sls file so that (pffi) library can
;; look it up.

#!r6rs
(library (pffi compat)
    (export open-shared-object
            lookup-shared-object
            make-c-function
            make-c-callback
            free-c-callback

            ;; primitive types
            char  unsigned-char
            short unsigned-short
            int   unsigned-int
            long  unsigned-long
            float double
            int8_t  uint8_t
            int16_t uint16_t
            int32_t uint32_t
            int64_t uint64_t
            pointer callback
            void
	    ___

            ;; pointer ref
            pointer-ref-c-uint8
            pointer-ref-c-int8
            pointer-ref-c-uint16
            pointer-ref-c-int16
            pointer-ref-c-uint32
            pointer-ref-c-int32
            pointer-ref-c-uint64
            pointer-ref-c-int64
            pointer-ref-c-unsigned-char
            pointer-ref-c-char
            pointer-ref-c-unsigned-short
            pointer-ref-c-short
            pointer-ref-c-unsigned-int
            pointer-ref-c-int
            pointer-ref-c-unsigned-long
            pointer-ref-c-long
            pointer-ref-c-float
            pointer-ref-c-double
            pointer-ref-c-pointer

            ;; pointer set
            pointer-set-c-uint8!
            pointer-set-c-int8!
            pointer-set-c-uint16!
            pointer-set-c-int16!
            pointer-set-c-uint32!
            pointer-set-c-int32!
            pointer-set-c-uint64!
            pointer-set-c-int64!
            pointer-set-c-unsigned-char!
            pointer-set-c-char!
            pointer-set-c-unsigned-short!
            pointer-set-c-short!
            pointer-set-c-unsigned-int!
            pointer-set-c-int!
            pointer-set-c-unsigned-long!
            pointer-set-c-long!
            pointer-set-c-float!
            pointer-set-c-double!
            pointer-set-c-pointer!

            ;; sizeof
            size-of-char
            size-of-short
            size-of-int
            size-of-long
            size-of-float
            size-of-double
            size-of-pointer
            (rename (size-of-int8 size-of-int8_t)
                    (size-of-int16 size-of-int16_t)
                    (size-of-int32 size-of-int32_t)
                    (size-of-int64 size-of-int64_t))

            pointer?
            bytevector->pointer
            pointer->bytevector
            pointer->integer
            integer->pointer

	    ;; Chez-specific exports
            pointer-tracker
            cleanup-bytevector-locks!
            )
    (import (rnrs)
	    (rnrs eval)
            (rename (pffi bv-pointer)
                    (bytevector->pointer bytevector->address))
	    (pffi helper)
	    (pffi misc)
            (only (chezscheme)
                  load-shared-object
                  lock-object foreign-callable-entry-point
                  foreign-callable unlock-object
                  foreign-procedure
                  ftype-pointer-address
                  foreign-entry foreign-sizeof foreign-ref foreign-set!
                  make-parameter
		  collect
		  make-guardian collect-request-handler))

;; dummy value
(define-record-type shared-object)
(define-record-type (<pointer> dummy pointer?))
;; general pointer.
(define-record-type integer-pointer
  (parent <pointer>)
  (fields address))
;; bytevector pointer, we need this as Chez's GC is generational GC,
;; means it moves its objects.
;; NOTE: this doesn't prevent GC during the FFI call, so a bit of
;;       half baked solution, though better than nothing
;; FIXME: if I have better idea
(define-record-type bytevector-pointer
  (parent <pointer>)
  (fields value))
(define integer->pointer make-integer-pointer)
(define (pointer->integer p)
  (cond ((integer-pointer? p) (integer-pointer-address p))
	((bytevector-pointer? p)
	 (bytevector->address (bytevector-pointer-value p)))
	(else (assertion-violation 'pointer->integer "pointer required" p))))

(define (pointer->bytevector pointer len . maybe-offset)
  (cond ((integer-pointer? pointer)
	 ;; FIXME one way copy
	 (let ((bv (make-bytevector len)))
	   (do ((i 0 (+ i 1)))
	       ((= i len) bv)
	     (bytevector-u8-set! bv i (pointer-ref-c-uint8 pointer i)))))
	;; This pass should be very rare as callback or returning pointer of
	;; FFI call is constructed from integer. But for the sake of
	;; completeness
	;; TODO Should we check length?
	((bytevector-pointer? pointer) (bytevector-pointer-value pointer))
	(else (assertion-violation 'pointer->bytevector "pointer required"
				   pointer))))

;; Parameter to allow tracking pointer creation in `bytevector->pointer`.
(define pointer-tracker
  (let ()
    (define (pointer-tracker-check v)
      (unless (procedure? v)
        (assertion-violation 'pointer-tracker "Given value is not a pointer-tracker" v))
      v)
    (define (default bv ptr)
      #f)
    (make-parameter default pointer-tracker-check)))

;; finalizer emulator
(define garbage-pool (make-guardian))

(define (bytevector->pointer bv)
  ;; We lock for each bytevector->pointer, unlocking happens when
  ;; ptr is collected. This way we have to unlock as many times as
  ;; there are pointers created.
  ;; Lock needs to be taken before we take the pointer.
  (lock-object bv)
  (guard (err [else (unlock-object bv) (raise-continuable err)])
    (let ([ptr (make-bytevector-pointer bv)])
      ;; bv is registered as representative since we want to access it for unlocking.
      (garbage-pool ptr bv)
      ((pointer-tracker) bv ptr)
      ptr)))

(define-syntax callback
  (syntax-rules ()
    ((_ ignore ...) 'callback)))
;; we don't use char defined in (chezscheme) due to the compatiliby
(define void           'void)
(define char           'integer-8)
(define unsigned-char  'unsigned-8)
(define short          'short)
(define unsigned-short 'unsigned-short)
(define int            'int)
(define unsigned-int   'unsigned-int)
(define long           'long)
(define unsigned-long  'unsigned-long)
(define int8_t         'integer-8)
(define uint8_t        'unsigned-8)
(define int16_t        'integer-16)
(define uint16_t       'unsigned-16)
(define int32_t        'integer-32)
(define uint32_t       'unsigned-32)
(define int64_t        'integer-64)
(define uint64_t       'unsigned-64)
(define double         'double)
(define float          'float)
(define pointer        'void*)

(define (open-shared-object path)
  (load-shared-object path)
  (make-shared-object))
(define (lookup-shared-object lib name) 
(make-integer-pointer (foreign-entry name)))

(define (free-c-callback proc) (unlock-object proc))

(define-syntax make-c-function
  (lambda (x)
    (syntax-case x (quote list)
      ((k lib ret (quote name) (list args ...))
       (identifier? #'name)
       (with-syntax ((name-str (symbol->string (syntax->datum #'name)))
                     (((types ...) varargs?)
		      (adjust-argument-types #'k #'(args ...)))
                     (chez-ret (datum->syntax #'k
                                (pffi-type->foreign-type
				 (syntax->datum #'ret)))))
	 ;; Minor optimisation not to have big code
	 (if (syntax->datum #'varargs?)
	     #'(let ()
		 (define (b->p b) (pointer->integer (bytevector->pointer b)))
		 (define (s->p s)
		   (b->p (string->utf8 (string-append s "\x0;"))))
		 (define (convert-arg type arg)
		   (case type
		     ((void*)
		      (cond ((string? arg) (s->p arg))
			    ((bytevector? arg) (b->p arg))
			    (else (pointer->integer arg))))
		     (else arg)))
		 (define (object->foreign-type arg)
		   (cond ((number? arg)
			  (cond ((and (exact? arg) (integer? arg))
				 (cond ((fixnum? arg) int32_t)
				       ((<= (bitwise-length arg) 64) int64_t)
				       (else (assertion-violation 'name
					      "Too big integer" arg))))
				;; sorry we don't know if this is
				;; float or double...
				((real? arg) double)
				(else
				 (assertion-violation 'name
				  "Unsuported number" arg))))
			 ((or (string? arg) (bytevector? arg) (pointer? arg))
			  pointer)
			 (else
			  (assertion-violation 'name
			   "Unsuported Scheme object" arg))))
		 (let ((required-types (drop-right (list args ...) 1)))
		   (lambda arg*
		     (let* ((rest (drop arg* (length required-types)))
			    (rest-types (map object->foreign-type rest))
			    ;; FIXME: We don't want to use `eval` here
			    ;;        to reduce compiled code size.
			    ;; This may not be an issue as this requires a
			    ;; special marker for variadic argument.
			    ;; See:
			    ;;  https://github.com/ktakashi/r6rs-pffi/issues/5
			    (fp (eval `(foreign-procedure name-str
					(,@required-types . ,rest-types)
					chez-ret)
				      (environment '(chezscheme))))
			    (r (apply fp (map convert-arg
					      (append required-types rest-types)
					      arg*))))
		       (case ret
			 ((void*) (integer->pointer r))
			 (else r))))))
	     #'(let ()
		 (define (b->p b) (pointer->integer (bytevector->pointer b)))
		 (define (s->p s)
		   (b->p (string->utf8 (string-append s "\x0;"))))
		 (define (convert-arg type arg)
		   (case type
		     ((void*)
		      (cond ((string? arg) (s->p arg))
			    ((bytevector? arg) (b->p arg))
			    (else (pointer->integer arg))))
		     (else arg)))
		 (let ((fp (foreign-procedure name-str (types ...) chez-ret))
		       (arg-types (list args ...)))
		   (lambda arg*
		     (let ((r (apply fp (map convert-arg arg-types arg*))))
		       (case ret
			 ((void*) (integer->pointer r))
			 (else r))))))))))))
(define-syntax make-c-callback
  (lambda (x)
    (syntax-case x (list)
      ((k ret (list arg* ...) body)
       (with-syntax ((((types ...) varargs?)
		      (adjust-argument-types #'k #'(arg* ...)))
                     (chez-ret (datum->syntax #'k
				(pffi-type->foreign-type
				 (syntax->datum #'ret)))))
         #'(let ((args '(types ...)))
             (define (wrap proc)
               (lambda vals
                 (let ((r (apply proc (map (lambda (type arg)
                                             (case type
                                               ((void*) (integer->pointer arg))
                                               (else arg)))
                                           args vals))))
                   (if (pointer? r)
                       (pointer->integer r)
                       r))))
             (let ((p (wrap body)))
               (define code (foreign-callable p (types ...) chez-ret))
               (lock-object code)
               (foreign-callable-entry-point code))))))))

(define-syntax define-deref
  (lambda (x)
    (define (gen-name t)
      (let ((s (symbol->string (syntax->datum t))))
        (list (string->symbol (string-append "size-of-" s))
              (string->symbol (string-append "pointer-ref-c-" s))
              (string->symbol (string-append "pointer-set-c-" s "!")))))
    (syntax-case x ()
      ((k type)
       (with-syntax (((size ref set!) (datum->syntax #'k (gen-name #'type))))
         #'(begin
             (define size (foreign-sizeof type))
             (define (ref ptr offset)
               (foreign-ref type (pointer->integer ptr) offset))
             (define (set! ptr offset value)
               (foreign-set! type (pointer->integer ptr) offset value)))))
      ((k type conv unwrap)
       (with-syntax (((size ref set!) (datum->syntax #'k (gen-name #'type))))
         #'(begin
             (define size (foreign-sizeof type))
             (define (ref ptr offset)
               (conv (foreign-ref type (pointer->integer ptr) offset)))
             (define (set! ptr offset value)
               (foreign-set! type (pointer->integer ptr) offset
                             (unwrap value)))))))))

(define int8    int8_t)
(define uint8  uint8_t)
(define int16   int16_t)
(define uint16 uint16_t)
(define int32   int32_t)
(define uint32 uint32_t)
(define int64   int64_t)
(define uint64 uint64_t)

(define-deref char)
(define-deref unsigned-char)
(define-deref short)
(define-deref unsigned-short)
(define-deref int)
(define-deref unsigned-int)
(define-deref long)
(define-deref unsigned-long)
(define-deref float)
(define-deref double)
(define-deref int8)
(define-deref uint8)
(define-deref int16)
(define-deref uint16)
(define-deref int32)
(define-deref uint32)
(define-deref int64)
(define-deref uint64)
(define-deref pointer make-integer-pointer pointer->integer)

;; Unlock objects that are referenced by the pool.
(define (cleanup-bytevector-locks!)
  (do ((x (garbage-pool) (garbage-pool)))
      ((not x))
    (unlock-object x)))

;; This has to be the last
(let ((saved (collect-request-handler)))
  (collect-request-handler
   (lambda ()
     (saved)
     (cleanup-bytevector-locks!))))
)
