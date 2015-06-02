;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/compat.mzscheme.sls - Compatible layer for Racket
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

	    (rename (cpointer? pointer?))
	    bytevector->pointer
	    pointer->bytevector
	    pointer->integer
	    integer->pointer
	    )
    (import (rnrs)
	    (ffi unsafe)
	    (ffi vector)
	    (rename (only (racket base) cons) (cons icons))
	    (only (srfi :13) string-index-right))
 
(define char           _sint8)
(define unsigned-char  _uint8)
(define short          _sshort)
(define unsigned-short _ushort)
(define int            _sint)
(define unsigned-int   _uint)
(define long           _slong)
(define unsigned-long  _ulong)
(define float          _float)
(define double         _double)
(define int8_t         _int8)
(define uint8_t        _uint8)
(define int16_t        _int16)
(define uint16_t       _uint16)
(define int32_t        _int32)
(define uint32_t       _uint32)
(define int64_t        _int64)
(define uint64_t       _uint64)
(define pointer        _pointer)

;; for convenience
(define int8         _int8)
(define uint8        _uint8)
(define int16        _int16)
(define uint16       _uint16)
(define int32        _int32)
(define uint32       _uint32)
(define int64        _int64)
(define uint64       _uint64)

(define-syntax callback
  (syntax-rules ()
    ((_ ret (args ...))
     (_cprocedure (->immutable-list (list args ...)) ret))))
(define void           _void)

(define (open-shared-object path)
  (let* ((index (string-index-right path #\.))
	 (file (if index
		   (substring path 0 index)
		   path)))
    (ffi-lib file)))

(define (lookup-shared-object lib name)
  ;; this gets address of specified object which is exactly what
  ;; we want to.
  (ffi-obj-ref name lib))

;; Fxxk!!!
(define (->immutable-list p)
  (let loop ((p p))
    (cond ((null? p) p)
	  ((pair? p) (icons (car p) (loop (cdr p))))
	  (else p))))
	

(define (make-c-function lib ret name arg-type)
  ;; TODO failure thunk, what should we do when it couldn't be found
  (get-ffi-obj (symbol->string name) lib 
	       ;; DAMN YOU MORON!!!
	       ;; seems this doesn't accept mutable pairs
	       ;; so convert it.
	       (_cprocedure (->immutable-list arg-type) ret)
	       (lambda () (error 'make-c-function "not found" name))))

(define (make-c-callback ret args proc) proc)

;; dummy
(define (free-c-callback ignore) #t)


(define-syntax define-deref
  (lambda (x)
    (define (gen-name t)
      (let ((s (symbol->string (syntax->datum t))))
	(list (string->symbol (string-append "size-of-" s))
	      (string->symbol (string-append "pointer-ref-c-" s))
	      (string->symbol (string-append "pointer-set-c-" s "!")))))
    (syntax-case x ()
      ((k type ->bv)
       (with-syntax (((size ref set!) (datum->syntax #'k (gen-name #'type))))
	 #'(begin
	     ;; ref/set as a byte so that we can specify detailed offset
	     ;; e.g. 
	     ;;  (pointer-ref-c-uint64 p 1)
	     ;;  will refer a value from one byte offset
	     (define (ref ptr offset)
	       (let ((bv (make-bytevector size)))
		 (do ((i 0 (+ i 1)))
		     ((= i size) (ptr-ref bv type 0))
		   (bytevector-u8-set! bv i
		       (ptr-ref ptr unsigned-char (+ i offset))))))
	     (define (set! ptr offset value)
	       (let ((bv (->bv value)))
		 (do ((len (bytevector-length bv)) (i 0 (+ i 1)))
		     ((= i len))
		   (ptr-set! ptr unsigned-char (+ offset i)
			     (bytevector-u8-ref bv i)))))))))))

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
(define (s8->bv s) (make-bytevector 1 s))
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

(define-deref char s8->bv)
(define-deref unsigned-char u8->bv)
(define-deref short s16->bv)
(define-deref unsigned-short u16->bv)
(define-deref int s32->bv)
(define-deref unsigned-int u32->bv)
(define-deref long long->bv)
(define-deref unsigned-long ulong->bv)
(define-deref float float->bv)
(define-deref double double->bv)
(define-deref int8 s8->bv)
(define-deref uint8 u8->bv)
(define-deref int16 s16->bv)
(define-deref uint16 u16->bv)
(define-deref int32 s32->bv)
(define-deref uint32 u32->bv)
(define-deref int64 s64->bv)
(define-deref uint64 u64->bv)

(define (pointer-ref-c-pointer p offset) 
  (let ((bv (make-bytevector size-of-pointer)))
    (do ((i 0 (+ i 1)))
	((= i size-of-pointer) (ptr-ref bv pointer 0))
      (bytevector-u8-set! bv i (pointer-ref-c-uint8 p (+ i offset))))))
(define (pointer-set-c-pointer! p offset v) 
  (let* ((pv (pointer->integer v))
	 (bv (uint-list->bytevector (list pv) 
				    (native-endianness) size-of-pointer)))
    (do ((i 0 (+ i 1)))
	((= i size-of-pointer))
      (pointer-set-c-uint8! p (+ i offset) (bytevector-u8-ref bv i)))))

(define-syntax define-sizeof
  (lambda (x)
    (define (gen-name t)
      (let ((s (symbol->string (syntax->datum t))))
	(string->symbol (string-append "size-of-" s))))
    (syntax-case x ()
      ((k type)
       (with-syntax ((name (datum->syntax #'k (gen-name #'type))))
	 #'(define name
	     (ctype-sizeof type)))))))
(define-sizeof char)
(define-sizeof short)
(define-sizeof int)
(define-sizeof long)
(define-sizeof pointer)
(define-sizeof float)
(define-sizeof double)
(define-sizeof int8_t)
(define-sizeof int16_t)
(define-sizeof int32_t)
(define-sizeof int64_t)

(define size-of-unsigned-char  size-of-char)
(define size-of-unsigned-short size-of-short)
(define size-of-unsigned-int   size-of-int)
(define size-of-unsigned-long  size-of-long)
(define size-of-int8           size-of-int8_t)
(define size-of-uint8          size-of-int8_t)
(define size-of-int16          size-of-int16_t)
(define size-of-uint16         size-of-int16_t)
(define size-of-int32          size-of-int32_t)
(define size-of-uint32         size-of-int32_t)
(define size-of-int64          size-of-int64_t)
(define size-of-uint64         size-of-int64_t)


(define (bytevector->pointer bv . maybe-offset)
  ;; seems not offset is possible
  (u8vector->cpointer bv))

(define (pointer->bytevector p len . maybe-offset)
  ;; ignore offset
  (make-sized-byte-string p len))

;; assume it's _gcpointer...
(define (pointer->integer p)
  (cast p _gcpointer (if (= size-of-pointer 8) _uint64 _uint32)))

(define (integer->pointer i)
  (cast i (if (= size-of-pointer 8) _uint64 _uint32) _gcpointer))


)
