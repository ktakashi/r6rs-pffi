;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/compat.mosh.sls - Compatible layer for Mosh
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
	    void

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
	    (rename (pointer-ref-c-signed-char pointer-ref-c-char))
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
	    ;; Mosh doesn't have pointer-set for unsigned types
	    ;; well we don't make effort
	    (rename (pointer-set-c-char! pointer-set-c-unsigned-char!))
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

	    size-of-char
	    size-of-short
	    size-of-int
	    size-of-long
	    size-of-float
	    size-of-double
	    size-of-pointer
	    size-of-int8_t
	    size-of-int16_t
	    size-of-int32_t
	    size-of-int64_t

	    pointer?
	    bytevector->pointer
	    pointer->bytevector
	    pointer->integer
	    integer->pointer
	    )
    (import (rnrs)
	    (rename (except (mosh ffi) 
			    ;; Seems mosh computes offset as multiple of
			    ;; sizeof(type). this I think inconvenient.
			    pointer-ref-c-uint16
			    pointer-ref-c-int16
			    pointer-ref-c-uint32
			    pointer-ref-c-int32
			    pointer-ref-c-uint64
			    pointer-ref-c-int64
			    pointer-ref-c-unsigned-short
			    pointer-ref-c-unsigned-int
			    pointer-ref-c-unsigned-long
			    pointer-ref-c-float
			    pointer-ref-c-double
			    pointer-ref-c-pointer
			    pointer-set-c-uint16!
			    pointer-set-c-int16!
			    pointer-set-c-uint32!
			    pointer-set-c-int32!
			    pointer-set-c-uint64!
			    pointer-set-c-int64!
			    pointer-set-c-short!
			    pointer-set-c-int!
			    pointer-set-c-long!
			    ;;pointer-set-c-unsigned-int!
			    ;;pointer-set-c-unsigned-long!
			    pointer-set-c-float!
			    pointer-set-c-double!
			    pointer-set-c-pointer!)
		    (lookup-shared-library %lookup-shared-library))
	    (rename (pffi bv-pointer) 
		    (bytevector->pointer %bytevector->pointer)))

(define char           'char)
(define unsigned-char  'unsigned-char)
(define short          'short)
(define unsigned-short 'unsigned-short)
(define int            'int)
(define unsigned-int   'unsigned-int)
(define long           'long)
(define unsigned-long  'unsigned-long)
(define float          'float)
(define double         'double)
(define int8_t         'int8_t)
(define uint8_t        'uint8_t)
(define int16_t        'int16_t)
(define uint16_t       'uint16_t)
(define int32_t        'int32_t)
(define uint32_t       'uint32_t)
(define int64_t        'int64_t)
(define uint64_t       'uint64_t)
(define-syntax callback
  (syntax-rules ()
    ((_ ignore ...) 'callback)))
(define void           'void)
(define pointer        'void*)

(define (lookup-shared-library lib name)
  (%lookup-shared-library lib (string->symbol name)))

(define size-of-char    1)
(define size-of-int8_t  1)
(define size-of-int16_t 2)
(define size-of-int32_t 4)
(define size-of-int64_t 8)

(define size-of-int16           size-of-int16_t)
(define size-of-uint16          size-of-int16_t)
(define size-of-int32           size-of-int32_t)
(define size-of-uint32          size-of-int32_t)
(define size-of-int64           size-of-int64_t)
(define size-of-uint64          size-of-int64_t)

(define (bytevector->pointer bv . maybe-offset)
  ;; offset will always be ignored.
  (%bytevector->pointer bv))

(define (pointer->bytevector p len . maybe-offset)
  ;; this is not what I want but no way to do on Mosh
  ;; we want shared bytevector
  (let ((offset (if (null? maybe-offset) 0 (car maybe-offset))))
    (do ((bv (make-bytevector len)) (i 0 (+ i 1)))
	((= i len) bv)
      (bytevector-u8-set! bv i (pointer-ref-c-uint8 p (+ i offset))))))

(define-syntax define-deref
  (lambda (x)
    (define (gen-name t)
      (let ((s (symbol->string (syntax->datum t))))
	(list (string->symbol (string-append "size-of-" s))
	      (string->symbol (string-append "pointer-ref-c-" s))
	      (string->symbol (string-append "pointer-set-c-" s "!")))))
    (syntax-case x ()
      ((k type bv-ref ->bv)
       (with-syntax (((size ref set!) (datum->syntax #'k (gen-name #'type))))
	 #'(begin
	     (define (ref ptr offset)
	       (let ((bv (make-bytevector size)))
		 (do ((i 0 (+ i 1))) 
		     ((= i size) (bv-ref bv 0 (native-endianness)))
		   (bytevector-u8-set! bv i 
		       (pointer-ref-c-uint8 ptr (+ i offset))))))
	     (define (set! ptr offset value)
	       (let ((bv (->bv value)))
		 (do ((len (bytevector-length bv))
		      (i 0 (+ i 1)))
		     ((= i len))
		   (pointer-set-c-uint8! ptr (+ i offset)
					 (bytevector-u8-ref bv i)))))))))))

;; kinda tricky
(define (bytevector-long-ref bv index endian)
  (if (= size-of-long 4)
      (bytevector-s32-ref bv index endian)
      (bytevector-s64-ref bv index endian)))
(define (bytevector-ulong-ref bv index endian)
  (if (= size-of-long 4)
      (bytevector-u32-ref bv index endian)
      (bytevector-u64-ref bv index endian)))
(define (bytevector-pointer-ref bv index endian)
  (integer->pointer
   (if (= size-of-pointer 4)
       (bytevector-u32-ref bv index endian)
       (bytevector-u64-ref bv index endian))))

(define-syntax define-uint->bv
  (syntax-rules ()
    ((_ name size)
     (define (name u)
       (uint-list->bytevector (list u) (native-endianness) size)))))
(define-syntax define-sint->bv
  (syntax-rules ()
    ((_ name size)
     (define (name s)
       (sint-list->bytevector (list s) (native-endianness) size)))))
(define-uint->bv u16->bv 2)
(define-uint->bv u32->bv 4)
(define-uint->bv u64->bv 8)
(define-sint->bv s16->bv 2)
(define-sint->bv s32->bv 4)
(define-sint->bv s64->bv 8)
(define-sint->bv long->bv size-of-long)
(define-sint->bv ulong->bv size-of-long)

(define (float->bv f)
  (let ((bv (make-bytevector 4)))
    (bytevector-ieee-single-native-set! bv 0 f)
    bv))
(define (double->bv f)
  (let ((bv (make-bytevector 8)))
    (bytevector-ieee-double-native-set! bv 0 f)
    bv))

(define-deref short bytevector-s16-ref s16->bv)
(define-deref unsigned-short bytevector-u16-ref u16->bv)
(define-deref int bytevector-s32-ref s32->bv)
(define-deref unsigned-int bytevector-u32-ref u32->bv)
(define-deref long bytevector-long-ref long->bv)
(define-deref unsigned-long bytevector-ulong-ref ulong->bv)
(define-deref float bytevector-ieee-single-ref float->bv)
(define-deref double bytevector-ieee-double-ref double->bv)
(define-deref int16 bytevector-s16-ref s16->bv)
(define-deref uint16 bytevector-u16-ref u16->bv)
(define-deref int32 bytevector-s32-ref s32->bv)
(define-deref uint32 bytevector-u32-ref u32->bv)
(define-deref int64 bytevector-s64-ref s64->bv)
(define-deref uint64 bytevector-u64-ref u64->bv)

(define (pointer->bv p) 
  (uint-list->bytevector (pointer->integer p) 
			 (native-endianness) size-of-pointer))
(define-deref pointer bytevector-pointer-ref pointer->bv)


)
