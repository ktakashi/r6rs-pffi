;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/compat.sagittarius.sls - Compatible layer for Sagittarius
;;;
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
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
    (export (rename (open-shared-library open-shared-object))
            (rename (lookup-shared-library lookup-shared-object))
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
            void boolean
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
            ;; should we define them?
            ;; pointer-ref-c-uint8_t
            ;; pointer-ref-c-int8_t
            ;; pointer-ref-c-uint16_t
            ;; pointer-ref-c-int16_t
            ;; pointer-ref-c-uint32_t
            ;; pointer-ref-c-int32_t
            ;; pointer-ref-c-uint64_t
            ;; pointer-ref-c-int64_t
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
            (rename (size-of-void* size-of-pointer)
		    (size-of-bool size-of-boolean))
            size-of-int8_t
            size-of-int16_t
            size-of-int32_t
            size-of-int64_t

            pointer?
            bytevector->pointer
            pointer->bytevector
            pointer->integer
            (rename (uinteger->pointer integer->pointer))
            )
    (import (rnrs)
            (rename (sagittarius ffi)
		    (callback %callback)
		    (make-c-function %make-c-function)
		    (make-c-callback %make-c-callback)
		    (char           ffi:char)
		    (unsigned-char  ffi:unsigned-char)
		    (short          ffi:short)
		    (unsigned-short ffi:unsigned-short)
		    (int            ffi:int)
		    (unsigned-int   ffi:unsigned-int)
		    (long           ffi:long)
		    (unsigned-long  ffi:unsigned-long)
		    (float          ffi:float)
		    (double         ffi:double)
		    (int8_t         ffi:int8_t)
		    (uint8_t        ffi:uint8_t)
		    (int16_t        ffi:int16_t)
		    (uint16_t       ffi:uint16_t)
		    (int32_t        ffi:int32_t)
		    (uint32_t       ffi:uint32_t)
		    (int64_t        ffi:int64_t)
		    (uint64_t       ffi:uint64_t))
            (pffi ffi-type-descriptor))

(define (->native-type type)
  (cond ((ffi-type-descriptor? type) (ffi-type-descriptor-alias type))
	(else type)))

(define (make-c-function lib conv ret name args)
  (%make-c-function lib (->native-type ret) name (map ->native-type args)))

(define (make-c-callback ret args proc)
  (%make-c-callback (->native-type ret) (map ->native-type args) proc))

(define-syntax callback
  (syntax-rules ()
    ((_ ignore ...) %callback)))

(define-syntax define-ftype
  (lambda (x)
    (define (->type&acc k name)
      (define base (symbol->string (syntax->datum name)))
      (datum->syntax k
       (list (string->symbol (string-append "ffi:" base))
	     (string->symbol (string-append "pointer-ref-c-" base))
	     (string->symbol (string-append "pointer-set-c-" base "!")))))
    (define (->sizeof k name)
      (define base (symbol->string (syntax->datum name)))
      (datum->syntax k (string->symbol (string-append "size-of-" base))))
    (syntax-case x ()
      ((k name)
       (with-syntax ((sizeof (->sizeof #'k #'name)))
	 #'(define-ftype name sizeof)))
      ((k name sizeof)
       (with-syntax (((type p-ref p-set) (->type&acc #'k #'name)))
	 #'(define name
	     (make-pointer-accesible-ffi-type-descriptor
	      'name type sizeof p-ref p-set)))))))

(define-ftype char)
(define-ftype unsigned-char size-of-char)
(define-ftype short)
(define-ftype unsigned-short size-of-short)
(define-ftype int)
(define-ftype unsigned-int size-of-int)
(define-ftype long)
(define-ftype unsigned-long size-of-long)
(define-ftype float)
(define-ftype double)
(define-ftype int8_t)
(define-ftype uint8_t size-of-int8_t)
(define-ftype int16_t)
(define-ftype uint16_t size-of-int16_t)
(define-ftype int32_t)
(define-ftype uint32_t size-of-int32_t)
(define-ftype int64_t)
(define-ftype uint64_t size-of-int64_t)
(define pointer (make-pointer-accesible-ffi-type-descriptor
		 'pointer void* size-of-void*
		 pointer-ref-c-pointer pointer-set-c-pointer!))
(define boolean (make-ffi-type-descriptor 'boolean bool size-of-bool))

)
