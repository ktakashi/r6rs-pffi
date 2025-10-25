;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/compat.guile.sls - Compatible layer for Guile
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
            void boolean wchar
	    ___

            ;; pointer ref
            (rename (pointer-ref-c-uint8_t  pointer-ref-c-uint8 )
		    (pointer-ref-c-int8_t   pointer-ref-c-int8  )
		    (pointer-ref-c-uint16_t pointer-ref-c-uint16)
		    (pointer-ref-c-int16_t  pointer-ref-c-int16 )
		    (pointer-ref-c-uint32_t pointer-ref-c-uint32)
		    (pointer-ref-c-int32_t  pointer-ref-c-int32 )
		    (pointer-ref-c-uint64_t pointer-ref-c-uint64)
		    (pointer-ref-c-int64_t  pointer-ref-c-int64 ))
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
	    pointer-ref-c-wchar

            ;; pointer set
            (rename (pointer-set-c-uint8_t!  pointer-set-c-uint8! )
		    (pointer-set-c-int8_t!   pointer-set-c-int8!  )
		    (pointer-set-c-uint16_t! pointer-set-c-uint16!)
		    (pointer-set-c-int16_t!  pointer-set-c-int16! )
		    (pointer-set-c-uint32_t! pointer-set-c-uint32!)
		    (pointer-set-c-int32_t!  pointer-set-c-int32! )
		    (pointer-set-c-uint64_t! pointer-set-c-uint64!)
		    (pointer-set-c-int64_t!  pointer-set-c-int64! ))
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
	    pointer-set-c-wchar!

            ;; sizeof
            size-of-char
            size-of-short
            size-of-int
            size-of-long
            size-of-float
            size-of-double
            size-of-pointer
	    size-of-boolean
            size-of-int8_t
            size-of-int16_t
            size-of-int32_t
            size-of-int64_t
	    size-of-wchar

            pointer?
            bytevector->pointer
            pointer->bytevector
            (rename (pointer-address pointer->integer)
                    (make-pointer integer->pointer))
            )
    (import (rnrs)
            (only (guile) dynamic-link dynamic-pointer uname)
            (rename (system foreign)
		    (short ffi:short)
		    (unsigned-short ffi:unsigned-short)
		    (int ffi:int)
		    (unsigned-int ffi:unsigned-int)
		    (long ffi:long)
		    (unsigned-long ffi:unsigned-long)
		    (float ffi:float)
		    (double ffi:double))
	    (pffi ffi-type-descriptor)
	    (only (srfi :1) drop-right split-at)
            (only (srfi :13) string-index-right))

(define size-of-wchar
  ;; Very unfortunately, Guile doesn't support wchar_t so we need to
  ;; dispatch like this
  (case (string->symbol (vector-ref (uname) 0))
    ((Windows) 2)
    (else 4)))

(define (pointer-ref-c-wchar p off)
  (let ((bv (pointer->bytevector p (+ size-of-wchar off))))
    (integer->char
     (case size-of-wchar
       ((2) (bytevector-u16-ref bv off (native-endianness)))
       ((4) (bytevector-u32-ref bv off (native-endianness)))))))
(define (pointer-set-c-wchar! p off wc)
  (let ((bv (pointer->bytevector p (+ size-of-wchar off)))
	(u (char->integer wc)))
    (case size-of-wchar
       ((2) (bytevector-u16-set! bv off u (native-endianness)))
       ((4) (bytevector-u32-set! bv off u (native-endianness))))))

(define-syntax callback
  (syntax-rules ()
    ((_ ignore ...) pointer)))
(define-syntax define-ftype
  (syntax-rules ()
    ((_ name type)
     (define name
       (make-ffi-type-descriptor 'name type (sizeof type))))
    ((_ name type bv-ref bv-set!)
     (define name
       (make-pointer-accesible-ffi-type-descriptor
	'name type (sizeof type)
	(let ((size (sizeof type)))
	  (lambda (ptr offset)
            (let ((bv (pointer->bytevector ptr (+ size offset))))
              (bv-ref bv offset (native-endianness)))))
	(let ((size (sizeof type)))
	  (lambda (ptr offset value)
            (let ((bv (pointer->bytevector ptr (+ size offset))))
              (bv-set! bv offset value (native-endianness))))))))))

(define-ftype char           int8
  bytevector-s8-ref/endian bytevector-s8-set/endian!)
(define-ftype unsigned-char  uint8
  bytevector-u8-ref/endian bytevector-u8-set/endian!)
(define-ftype short          ffi:short
  bytevector-s16-ref bytevector-s16-set!)
(define-ftype unsigned-short ffi:unsigned-short
  bytevector-u16-ref bytevector-u16-set!)
(define-ftype int            ffi:int
  bytevector-s32-ref bytevector-s32-set!)
(define-ftype unsigned-int   ffi:unsigned-int
  bytevector-u32-ref bytevector-u32-set!)
(define-ftype long           ffi:long
  bytevector-long-ref bytevector-long-set!)
(define-ftype unsigned-long  ffi:unsigned-long
  bytevector-ulong-ref bytevector-ulong-set!)
(define-ftype float          ffi:float
  bytevector-ieee-single-ref bytevector-ieee-single-set!)
(define-ftype double         ffi:double
  bytevector-ieee-double-ref bytevector-ieee-double-set!)
(define-ftype int8_t         int8
  bytevector-s8-ref/endian bytevector-s8-set/endian!)
(define-ftype uint8_t        uint8
  bytevector-u8-ref/endian bytevector-u8-set/endian!)
(define-ftype int16_t        int16
  bytevector-s16-ref bytevector-s16-set!)
(define-ftype uint16_t       uint16
  bytevector-u16-ref bytevector-u16-set!)
(define-ftype int32_t        int32
  bytevector-s32-ref bytevector-s32-set!)
(define-ftype uint32_t       uint32
  bytevector-u32-ref bytevector-u32-set!)
(define-ftype int64_t        int64
  bytevector-s64-ref bytevector-s64-set!)
(define-ftype uint64_t       uint64
  bytevector-u64-ref bytevector-u64-set!)
(define-ftype pointer        '*
  bytevector-pointer-ref bytevector-pointer-set!)
(define-ftype boolean        int8) ;; use int8 to make the size = 1
(define wchar (make-pointer-accesible-ffi-type-descriptor 
	       'wchar (case size-of-wchar ((2) uint16) ((4) uint32))
	       size-of-wchar pointer-ref-c-wchar pointer-set-c-wchar!))

(define ___            '___) ;; dummy

(define (open-shared-object path)
  (let* ((index (string-index-right path #\.))
         (file (if index
                   (substring path 0 index)
                   path)))
    (dynamic-link file)))
(define (lookup-shared-object lib name)
  (dynamic-pointer name lib))

(define (free-c-callback proc) #t) ;; for now.

(define (->native-type type)
  (cond ((ffi-type-descriptor? type) (ffi-type-descriptor-alias type))
	(else type)))

(define (convert-arg type arg)
  (define (s->p s) (b->p (string->utf8 (string-append s "\x0;"))))
  (define (b->p bv) (bytevector->pointer bv))

  (cond ((eq? type pointer)
	 (cond ((string? arg)     (s->p arg))
	       ((bytevector? arg) (b->p arg))
	       ;; Let Guile complain, if not the proper
	       ;; one
	       (else arg)))
	((eq? type boolean)
	 (unless (boolean? arg)
	   (assertion-violation name "Boolean is required" arg))
	 (if arg 1 0))
	((eq? type wchar) (char->integer arg))
	(else arg)))

(define (convert-ret type r)
  (cond ((eq? type boolean) (eqv? r 1))
	((eq? type wchar) (integer->char r))
	(else r)))
  
(define (make-c-function lib conv ffi:ret name arg-types)
  (define ret (->native-type ffi:ret))
  (define ptr (lookup-shared-object lib (symbol->string name)))
  
  (define (arg->type arg)
    ;; it's a bit awkward but no other way
    (cond ((number? arg)
	   (cond ((and (exact? arg) (integer? arg))
		  (let ((n (bitwise-length arg)))
		    (cond ((<= n 32) int32_t)
			  ((<= n 64) int64_t)
			  (else (assertion-violation name "Too big integer"
						     arg)))))
		 ;; sorry we don't know if this is float or double...
		 ((real? arg) double)
		 (else (assertion-violation name "Unsuported number" arg))))
	  ((or (string? arg) (bytevector? arg) (pointer? arg)) pointer)
	  ((boolean? arg) boolean)
	  ((char? arg) wchar) ;; naive assumption
	  (else (assertion-violation name "Unsuported type" arg))))

  (cond ((memq ___ arg-types) =>
	 (lambda (l)
	   (unless (null? (cdr l))
	     (assertion-violation 'make-c-function
	      "___ must be the last of argument type" arg-types))
	   (let ((required-args (remove (lambda (e) (eq? ___ e)) arg-types)))
	     (lambda args*
	       (let-values (((required rest)
			     (split-at args* (- (length required-args) 1))))
		 (let* ((real-arg-types (append (drop-right arg-types 1)
						(map arg->type rest)))
			(fp (pointer->procedure ret ptr
			     (map ->native-type real-arg-types))))
		   (convert-ret ffi:ret
		    (apply fp (map convert-arg real-arg-types args*)))))))))
	(else
	 (let ((fp (pointer->procedure ret ptr (map ->native-type arg-types))))
	   (lambda args*
	     (convert-ret ffi:ret
			  (apply fp (map convert-arg arg-types args*))))))))

(define (make-c-callback ret args proc)
  (define (wrapped . args*)
    (convert-arg ret (apply proc (map convert-ret args args*))))
  (procedure->pointer (->native-type ret) wrapped (map ->native-type args)))

(define-syntax define-deref
  (lambda (x)
    (define (gen-name t)
      (let ((s (symbol->string (syntax->datum t))))
        (list (string->symbol (string-append "pointer-ref-c-" s))
              (string->symbol (string-append "pointer-set-c-" s "!")))))
    (syntax-case x ()
      ((k type)
       (with-syntax (((ref set!) (datum->syntax #'k (gen-name #'type))))
         #'(begin
             (define ref
	       (pointer-accesible-ffi-type-descriptor-pointer-ref type))
             (define set!
	       (pointer-accesible-ffi-type-descriptor-pointer-set! type))))))))
(define (bytevector-u8-ref/endian bv index endian)
  (bytevector-u8-ref bv index))
(define (bytevector-u8-set/endian! bv index v endian)
  (bytevector-u8-set! bv index v))
(define (bytevector-s8-ref/endian bv index endian)
  (bytevector-s8-ref bv index))
(define (bytevector-s8-set/endian! bv index v endian)
  (bytevector-s8-set! bv index v))

;; kinda tricky
(define (bytevector-long-ref bv index endian)
  (if (= size-of-long 4)
      (bytevector-s32-ref bv index endian)
      (bytevector-s64-ref bv index endian)))
(define (bytevector-long-set! bv index v endian)
  (if (= size-of-long 4)
      (bytevector-s32-set! bv index v endian)
      (bytevector-s64-set! bv index v endian)))
(define (bytevector-ulong-ref bv index endian)
  (if (= size-of-long 4)
      (bytevector-u32-ref bv index endian)
      (bytevector-u64-ref bv index endian)))
(define (bytevector-ulong-set! bv index v endian)
  (if (= size-of-long 4)
      (bytevector-u32-set! bv index v endian)
      (bytevector-u64-set! bv index v endian)))


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
(define-deref int8_t)
(define-deref uint8_t)
(define-deref int16_t)
(define-deref uint16_t)
(define-deref int32_t)
(define-deref uint32_t)
(define-deref int64_t)
(define-deref uint64_t)

(define (bytevector-pointer-ref bv index endian)
  (make-pointer
   (if (= size-of-pointer 4)
       (bytevector-u32-ref bv index endian)
       (bytevector-u64-ref bv index endian))))
(define (bytevector-pointer-set! bv index v endian)
  (let ((i (pointer-address v)))
    (if (= size-of-pointer 4)
        (bytevector-u32-set! bv index i endian)
        (bytevector-u64-set! bv index i endian))))
(define-deref pointer)


(define-syntax define-sizeof
  (lambda (x)
    (define (gen-name t)
      (let ((s (symbol->string (syntax->datum t))))
        (string->symbol (string-append "size-of-" s))))
    (syntax-case x ()
      ((k type)
       (with-syntax ((name (datum->syntax #'k (gen-name #'type))))
         #'(define name (ffi-type-descriptor-size type)))))))

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
(define-sizeof boolean)
;; for define-deref
(define size-of-unsigned-char   size-of-char)
(define size-of-unsigned-short  size-of-short)
(define size-of-unsigned-int    size-of-int)
(define size-of-unsigned-long   size-of-long)
(define size-of-int8            size-of-int8_t)
(define size-of-uint8           size-of-int8_t)
(define size-of-int16           size-of-int16_t)
(define size-of-uint16          size-of-int16_t)
(define size-of-int32           size-of-int32_t)
(define size-of-uint32          size-of-int32_t)
(define size-of-int64           size-of-int64_t)
(define size-of-uint64          size-of-int64_t)

)
