;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/compat.vicare.sls - Compatible layer for Vicare
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
    (export open-shared-object	 ;; form (vicare ffi)
	    lookup-shared-object ;; ditto
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

	    pointer-ref-c-uint8
	    (rename (pointer-ref-c-sint8 pointer-ref-c-int8))
	    pointer-ref-c-uint16
	    (rename (pointer-ref-c-sint16 pointer-ref-c-int16))
	    pointer-ref-c-uint32
	    (rename (pointer-ref-c-sint32 pointer-ref-c-int32))
	    pointer-ref-c-uint64
	    (rename (pointer-ref-c-sint64 pointer-ref-c-int64))
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
	    (rename (pointer-set-c-sint8! pointer-set-c-int8!))
	    pointer-set-c-uint16!
	    (rename (pointer-set-c-sint16! pointer-set-c-int16!))
	    pointer-set-c-uint32!
	    (rename (pointer-set-c-sint32! pointer-set-c-int32!))
	    pointer-set-c-uint64!
	    (rename (pointer-set-c-sint64! pointer-set-c-int64!))
	    pointer-set-c-unsigned-char!
	    (rename (pointer-set-c-signed-char! pointer-set-c-char!))
	    pointer-set-c-unsigned-short!
	    (rename (pointer-set-c-signed-short! pointer-set-c-short!))
	    pointer-set-c-unsigned-int!
	    (rename (pointer-set-c-signed-int! pointer-set-c-int!))
	    pointer-set-c-unsigned-long!
	    (rename (pointer-set-c-signed-long! pointer-set-c-long!))
	    pointer-set-c-float!
	    pointer-set-c-double!
	    pointer-set-c-pointer!
	    
	    )
    (import (rnrs)
	    (vicare ffi))


(define char           'signed-char)
(define unsigned-char  'unsigned-char)
(define short          'signed-short)
(define unsigned-short 'unsigned-short)
(define int            'signed-int)
(define unsigned-int   'unsigned-int)
(define long           'signed-long)
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
(define pointer        'pointer)
(define-syntax callback
  (syntax-rules ()
    ((_ ignore ...) 'callback)))
;; seems it's not documented but works
(define void           'void)

(define (make-c-function lib ret name arg-type)
  (let ((func (lookup-shared-object lib (symbol->string name)))
	(m (make-c-callout-maker ret arg-type)))
    (m func)))

(define (make-c-callback ret args proc)
  (let ((m (make-c-callback-maker ret args)))
    (m proc)))

)
