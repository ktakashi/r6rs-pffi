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
	    (rename (pointer-ref-c-signed-short pointer-ref-c-short))
	    pointer-ref-c-unsigned-int
	    (rename (pointer-ref-c-signed-int pointer-ref-c-int))
	    pointer-ref-c-unsigned-long
	    (rename (pointer-ref-c-signed-long pointer-ref-c-long))
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
	    (rename (pointer-set-c-short! pointer-set-c-unsigned-short!))
	    pointer-set-c-short!
	    (rename (pointer-set-c-int! pointer-set-c-unsigned-int!))
	    pointer-set-c-int!
	    (rename (pointer-set-c-long! pointer-set-c-unsigned-long!))
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

	    bytevector->pointer
	    pointer->bytevector
	    pointer->integer
	    integer->pointer
	    )
    (import (rnrs)
	    (rename (mosh ffi) (lookup-shared-library %lookup-shared-library))
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

(define (bytevector->pointer bv . maybe-offset)
  ;; offset will always be ignored.
  (%bytevector->pointer bv))

(define (pointer->bytevector p len . maybe-offset)
  ;; this is not what I want but no way to do on Mosh
  ;; we want shared bytevector
  (let ((offset (if (null? maybe-offset) 0 (car maybe-offset))))
    (do ((bv (make-bytevector len)) (i 0 (+ i 1)))
	((= i len) bv)
      (bytevector-u8-set! bv (+ i 1) (pointer-ref-c-uint8 p (+ i offset))))))

)
