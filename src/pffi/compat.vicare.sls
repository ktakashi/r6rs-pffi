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
	    (rename (%lookup-shared-object lookup-shared-object))
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
	    size-of-int8_t
	    size-of-int16_t
	    size-of-int32_t
	    size-of-int64_t

	    bytevector->pointer
	    pointer->bytevector
	    (rename (%pointer->integer pointer->integer)
		    (%integer->pointer integer->pointer))
	    )
    (import (rnrs)
	    (rename (except (vicare ffi)
			    pointer-ref-c-sint8
			    pointer-ref-c-uint16
			    pointer-ref-c-sint16
			    pointer-ref-c-uint32
			    pointer-ref-c-sint32
			    pointer-ref-c-uint64
			    pointer-ref-c-sint64
			    pointer-ref-c-unsigned-char
			    pointer-ref-c-unsigned-short
			    pointer-ref-c-unsigned-int
			    pointer-ref-c-unsigned-long
			    pointer-ref-c-float
			    pointer-ref-c-double
			    pointer-ref-c-pointer
			    pointer-set-c-sint8!
			    pointer-set-c-uint16!
			    pointer-set-c-sint16!
			    pointer-set-c-uint32!
			    pointer-set-c-sint32!
			    pointer-set-c-uint64!
			    pointer-set-c-sint64!
			    pointer-set-c-short!
			    pointer-set-c-sint!
			    pointer-set-c-unsigned-char!
			    pointer-set-c-unsigned-short!
			    pointer-set-c-unsigned-int!
			    pointer-set-c-unsigned-long!
			    pointer-set-c-float!
			    pointer-set-c-double!
			    pointer-set-c-pointer!)
		    (pointer-ref-c-uint8 %pointer-ref-c-uint8)
		    (pointer-set-c-uint8! %pointer-set-c-uint8!))
	    (rename (vicare platform words)
		    (SIZEOF_CHAR    size-of-char)
		    (SIZEOF_SHORT   size-of-short)
		    (SIZEOF_INT     size-of-int)	     
		    (SIZEOF_LONG    size-of-long)
		    (SIZEOF_FLOAT   size-of-float)
		    (SIZEOF_DOUBLE  size-of-double)
		    (SIZEOF_POINTER size-of-pointer)))


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

;; this is needed
(define (sync-pointer arg)
  (when (pointer-wrapper? arg)
    (let* ((dst (pointer-bytevector arg))
	   (len (bytevector-length dst))
	   (src (memory->bytevector (pointer-memory arg) len)))
      (bytevector-copy! src 0 dst 0 len))))

(define (make-c-function lib ret name arg-type)
  (define (pointer-handler f)
    (define (pointer/value arg)
      (if (pointer-wrapper? arg)
	  (pointer-memory arg)
	  arg))
    (lambda args
      (let-values ((results (apply f (map pointer/value args))))
	(for-each sync-pointer args)
	;; do foreign-procedures return multiple values?
	(apply values results))))
  (let ((func (lookup-shared-object lib (symbol->string name)))
	(m (make-c-callout-maker ret arg-type)))
    (pointer-handler (m func))))

(define (%lookup-shared-object lib name)
  (let ((raw-ptr (lookup-shared-object lib name)))
    (make-pointer-wrapper (memory->bytevector raw-ptr size-of-pointer)
			  raw-ptr)))

;; FIXME, this probably doesn't work
(define (make-c-callback ret args proc)
  (define (pointer-handler f)
    (define (pointer/value arg)
      (if (pointer? arg)
	  ;; TODO is this true?
	  (make-pointer-wrapper (memory->bytevector arg size-of-pointer) arg)
	  arg))
    (lambda args
      (let-values ((results (apply f (map pointer/value args))))
	;; (for-each sync-pointer results)
	;; argument is passed from foreign world
	(apply values results))))
  (let ((m (make-c-callback-maker ret args)))
    (m (pointer-handler proc))))


(define size-of-int8_t  1)
(define size-of-int16_t 2)
(define size-of-int32_t 4)
(define size-of-int64_t 8)
(define size-of-uint8_t  1)
(define size-of-uint16_t 2)
(define size-of-uint32_t 4)
(define size-of-uint64_t 8)

(define size-of-int8  1)
(define size-of-int16 2)
(define size-of-int32 4)
(define size-of-int64 8)
(define size-of-uint8  1)
(define size-of-uint16 2)
(define size-of-uint32 4)
(define size-of-uint64 8)


(define size-of-unsigned-char  size-of-char)
(define size-of-unsigned-short size-of-short)
(define size-of-unsigned-int   size-of-int)
(define size-of-unsigned-long  size-of-long)


(define-record-type pointer-wrapper
  (fields (immutable bytevector pointer-bytevector)
	  (immutable memory pointer-memory)))

(define (bytevector->pointer bv . maybe-offset)
  ;; unfortunately, there is no procedure which can make a pointer
  ;; whose value is shared by the given bytevector on Vicare.
  ;; so emulate it.
  (let-values (((p size) (bytevector->memory bv)))
    (make-pointer-wrapper bv p)))

(define (pointer->bytevector p len . maybe-offset)
  ;; limitation, returning bytevector is not shared with pointer.
  (let ((bv (make-bytevector len)))
    (bytevector-copy! (pointer-bytevector p) 0 bv 0 len)
    bv))

;; do the same trick as Mosh
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
	       (let ((bv (make-bytevector size))
		     (p (pointer-memory ptr)))
		 (do ((i 0 (+ i 1))) 
		     ((= i size) (bv-ref bv 0 (native-endianness)))
		   (bytevector-u8-set! bv i 
		       (%pointer-ref-c-uint8 p (+ i offset))))))
	     (define (set! ptr offset value)
	       (let ((bv (->bv value))
		     (p (pointer-memory ptr)))
		 (do ((len (bytevector-length bv))
		      (i 0 (+ i 1)))
		     ((= i len) (sync-pointer ptr))
		   (%pointer-set-c-uint8! p (+ i offset)
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
  (%integer->pointer
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
(define (u8->bv u) (make-bytevector 1 u))
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

(define (bytevector-u8-ref/endian bv index endian)
  (bytevector-u8-ref bv index))
(define (bytevector-s8-ref/endian bv index endian)
  (bytevector-s8-ref bv index))

(define-deref char bytevector-s8-ref/endian u8->bv)
(define-deref unsigned-char bytevector-u8-ref/endian u8->bv)
(define-deref short bytevector-s16-ref s16->bv)
(define-deref unsigned-short bytevector-u16-ref u16->bv)
(define-deref int bytevector-s32-ref s32->bv)
(define-deref unsigned-int bytevector-u32-ref u32->bv)
(define-deref long bytevector-long-ref long->bv)
(define-deref unsigned-long bytevector-ulong-ref ulong->bv)
(define-deref float bytevector-ieee-single-ref float->bv)
(define-deref double bytevector-ieee-double-ref double->bv)
(define-deref int8 bytevector-s8-ref/endian u8->bv)
(define-deref uint8 bytevector-u8-ref/endian u8->bv)
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

(define (%pointer->integer ptr)
  (pointer->integer (pointer-memory ptr)))
(define (%integer->pointer i)
  (let ((p (integer->pointer i)))
    (make-pointer-wrapper (memory->bytevector p size-of-pointer) p)))

)
