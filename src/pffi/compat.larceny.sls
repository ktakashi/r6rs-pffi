;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/compat.larceny.sls - Compatible layer for Larceny
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

	    pointer?
	    bytevector->pointer
	    pointer->bytevector
	    pointer->integer
	    integer->pointer

	    )
    (import (rnrs)
	    (primitives ffi/dlopen ffi/dlsym
			ffi-attribute-core-entry
			ffi/ret-converter
			ffi/arg-converter
			ffi/convert-arg-descriptor
			ffi/convert-ret-descriptor
			ffi/make-callout
			ffi/make-callback
			ffi/apply
			;; in lib/Base/std-ffi.sch
			ffi-get-abi
			ffi/rename-ret-type
			ffi/rename-arg-type
			void*-rt
			void*?
			void*->address
			sizeof:long
			sizeof:pointer
			%peek8 %peek8u
			%peek16 %peek16u
			%peek32 %peek32u
			%peek-long %peek-ulong
			peek-bytes
			poke-bytes
			void*-float-ref
			void*-double-ref
			void*-void*-ref
			void*-void*-set!
			ffi/handle->address
			make-nonrelocatable-bytevector
			))

;; it might be better not to show handle (integer) itself
(define-record-type shared-object
  (fields handle))

(define (open-shared-object path) 
  (let ((handle (ffi/dlopen path)))
    (and handle
	 (make-shared-object handle))))

(define-record-type (<pointer> make-pointer pointer?)
  (fields (immutable src pointer-src)
	  ;; nonrelocatable bytevector 
	  dummy
	  ;; element pointer of above
	  (immutable ptr pointer-ptr)))
(define (void*->pointer v*)
  (let ((bv (make-bytevector size-of-pointer)))
    (if (= size-of-pointer 4)
	(bytevector-u32-native-set! bv 0 (void*-ptr v*))
	(bytevector-u64-native-set! bv 0 (void*-ptr v*)))
    (make-pointer bv #f v*)))

;; we can't use unsigned->void*, this converts null pointer
;; to #f...
(define make-void* (record-constructor void*-rt))
(define address->pointer
  (lambda (addr)
    (void*->pointer (make-void* addr))))
;; ffi/dlsym returns integer (address) directly so
;; we need to get converter
(define (lookup-shared-object lib name) 
  (let ((address (ffi/dlsym (shared-object-handle lib) name)))
    (address->pointer address)))

;; we want to manage foreign procedure per shared object
;; so implement this here as well
(define (pointer->void* o)
  (if (pointer? o)
      (pointer-ptr o)
      o))
(define (%void*->pointer o)
  (if (void*? o)
      (void*->pointer o)
      o))

(define (sync-pointer arg)
  (if (pointer? arg)
      (let* ((dst (pointer-src arg))
	     (len (bytevector-length dst)))
	(do ((i 0 (+ i 1)))
	    ((= i len) arg)
	  (bytevector-u8-set! dst i (pointer-ref-c-uint8 arg i))))
      arg))

(define (make-foreign-invoker tramp args ret ret-conv arg-conv name)
  (lambda actual
    ;; (display name) (newline) (display actual) (newline)
    (let-values (((error? value) 
		  (ffi/apply tramp args ret 
			     (map (lambda (c v) (c v (symbol->string name)))
				  arg-conv (map pointer->void* actual)))))
      (for-each sync-pointer actual)
      (if error?
	  (error name "Failed to call foreign procedure" name actual)
	  (sync-pointer (%void*->pointer (ret-conv value (symbol->string name))))))))

(define make-c-function 
  ;; for some reason ffi-get-abi requires something for type
  ;; and if we pass null, we can get cdecl
  (let ((abi (ffi-get-abi 'callout '())))
    (lambda (lib ret name arg-type)
      (let* ((rconv (ffi/ret-converter ret))
	     (argconv (map ffi/arg-converter arg-type))
	     (addr (ffi/dlsym (shared-object-handle lib) (symbol->string name)))
	     (renamed-args (map ffi/rename-arg-type arg-type))
	     (renamed-ret (ffi/rename-ret-type ret))
	     (tramp (ffi/make-callout abi addr renamed-args renamed-ret))
	     (args (ffi/convert-arg-descriptor abi renamed-args))
	     (ret (ffi/convert-ret-descriptor abi renamed-ret)))
	(make-foreign-invoker tramp args ret rconv argconv name)))))

;; maybe we should make this GC protected
(define make-c-callback
  (let ((abi (ffi-get-abi 'callback '())))
    (lambda (ret types proc)
      (ffi/make-callback 
       abi
       (lambda args
	 (let ((v (apply proc (map (lambda (t v)
				     (sync-pointer
				      (%void*->pointer ((ffi/ret-converter t) v t))))
				   types args)))
	       (r-conv (ffi/arg-converter ret)))
	   ;; sync returning value 
	   (pointer->void*
	    (sync-pointer
	     (if r-conv
		 (r-conv v ret)
		 v)))))
       (map ffi/rename-arg-type types)
       (ffi/rename-ret-type ret)))))
;; dummy
(define (free-c-callback ignore) #t)

(define void*-ptr (record-accessor void*-rt 'ptr))
(define pointer-pointer
  (lambda (p)
    (void*-ptr (pointer-ptr p))))
(define-syntax define-pointer-ref
  (syntax-rules ()
    ((_ name peek)
     (define (name p offset)
       (peek (+ (pointer-pointer p) offset))))))

(define-pointer-ref pointer-ref-c-uint8 %peek8u)
(define-pointer-ref pointer-ref-c-int8 %peek8)
(define-pointer-ref pointer-ref-c-uint16 %peek16u)
(define-pointer-ref pointer-ref-c-int16 %peek16)
(define-pointer-ref pointer-ref-c-uint32 %peek32u)
(define-pointer-ref pointer-ref-c-int32 %peek32)
(define (pointer-ref-c-uint64 p offset)
  (let* ((addr (pointer-pointer p))
	 (bv (make-bytevector 8)))
    (peek-bytes (+ addr offset) bv 8)
    (bytevector-u64-native-ref bv 0)))
(define (pointer-ref-c-int64 p offset)
  (let* ((addr (pointer-pointer p))
	 (bv (make-bytevector 8)))
    (peek-bytes (+ addr offset) bv 8)
    (bytevector-s64-native-ref bv 0)))

(define pointer-ref-c-unsigned-char pointer-ref-c-uint8)
(define pointer-ref-c-char pointer-ref-c-int8)
(define pointer-ref-c-unsigned-short pointer-ref-c-uint16)
(define pointer-ref-c-short pointer-ref-c-int16)
(define pointer-ref-c-unsigned-int pointer-ref-c-uint32)
(define pointer-ref-c-int pointer-ref-c-int32)
(define-pointer-ref pointer-ref-c-unsigned-long %peek-long)
(define-pointer-ref pointer-ref-c-long %peek-ulong)
;; use predefined ones
(define (pointer-ref-c-float p offset)
  (let ((p (pointer-ptr p)))
    (void*-float-ref p offset)))
(define (pointer-ref-c-double p offset)
  (let ((p (pointer-ptr p)))
    (void*-double-ref p offset)))
(define (pointer-ref-c-pointer p offset)
  (let ((p (pointer-ptr p)))
    (void*->pointer (void*-void*-ref p offset))))

;; pointer set
(define-syntax define-pointer-set
  (syntax-rules ()
    ((_ name size bv-set)
     (define (name p offset val)
       (let ((bv (make-bytevector size)))
	 (bv-set bv 0 val)
	 (poke-bytes (+ (pointer-pointer p) offset) bv size))))))

(define (bytevector-long-native-set! bv index val)
  (if (= size-of-long 4)
      (bytevector-s32-ref bv index val (native-endianness))
      (bytevector-s64-ref bv index val (native-endianness))))
(define (bytevector-ulong-native-set! bv index val)
  (if (= size-of-long 4)
      (bytevector-u32-ref bv index val (native-endianness))
      (bytevector-u64-ref bv index val (native-endianness))))

(define-pointer-set pointer-set-c-uint8! 1 bytevector-u8-set!)
(define-pointer-set pointer-set-c-int8! 1 bytevector-s8-set!)
(define-pointer-set pointer-set-c-uint16! 2 bytevector-u16-native-set!)
(define-pointer-set pointer-set-c-int16! 2 bytevector-s16-native-set!)
(define-pointer-set pointer-set-c-uint32! 4 bytevector-u32-native-set!)
(define-pointer-set pointer-set-c-int32! 4 bytevector-s32-native-set!)
(define-pointer-set pointer-set-c-uint64! 8 bytevector-u64-native-set!)
(define-pointer-set pointer-set-c-int64! 8 bytevector-s64-native-set!)
(define pointer-set-c-unsigned-char! pointer-set-c-uint8!)
(define pointer-set-c-char! pointer-set-c-int8!)
(define pointer-set-c-unsigned-short! pointer-set-c-uint16!)
(define pointer-set-c-short! pointer-set-c-int16!)
(define pointer-set-c-unsigned-int! pointer-set-c-uint32!)
(define pointer-set-c-int! pointer-set-c-int32!)
(define-pointer-set pointer-set-c-unsigned-long! 
  size-of-long bytevector-ulong-native-set!)
(define-pointer-set pointer-set-c-long! 
  size-of-long bytevector-long-native-set!)
(define-pointer-set pointer-set-c-float! 4 bytevector-ieee-single-native-set!)
(define-pointer-set pointer-set-c-double! 8 bytevector-ieee-double-native-set!)
(define (pointer-set-c-pointer! p offset v)
  (let ((p (pointer-ptr p))
	(v (pointer-ptr v)))
    (void*-void*-set! p offset v)))


;; types
(define char 'char) ;; should we use byte?
(define unsigned-char 'uchar) ;; should we use unsigned?
(define short 'short)
(define unsigned-short 'ushort)
(define int 'int)
(define unsigned-int 'uint)
(define long 'long)
(define unsigned-long 'ulong)
(define float 'float)
(define double 'double)
(define int8_t 'byte)
(define uint8_t 'unsigned)
(define int16_t 'short)
(define uint16_t 'ushort)
(define int32_t 'int)
(define uint32_t 'uint)
(define int64_t 'longlong)
(define uint64_t 'ulonglong)
(define pointer 'void*)
(define-syntax callback
  (syntax-rules ()
    ((_ ignore ...) 'tramp)))
(define void 'void)

(define size-of-char 1)
(define size-of-short 2)
(define size-of-int 4)
(define size-of-long sizeof:long)
(define size-of-float 4)
(define size-of-double 8)
(define size-of-pointer sizeof:pointer)
(define size-of-int8_t 1)
(define size-of-int16_t 2)
(define size-of-int32_t 4)
(define size-of-int64_t 8)

(define (bytevector->pointer bv . maybe-offset)
  ;; this is absolutely not documented anywhere and may not work
  ;; in future. in include/Sys/macros.h, bytevector-ref is
  ;; defined like this (*((byte*)(ptrof(x)+1)+i))
  ;; thus, after 1 word, it will be the content of the bytevector.
  ;; so what we need to do here is adding offset of word.
  ;; we can actually calculate offset, but we don't do it for now
  (let ((dummy (make-nonrelocatable-bytevector (bytevector-length bv))))
    (bytevector-copy! bv 0 dummy 0 (bytevector-length bv))
    (make-pointer bv dummy (make-void* (+ (ffi/handle->address dummy) size-of-pointer)))))
(define (pointer->bytevector p len . maybe-offset)
  ;; Unfortunately, we only have one way, copy
  (let ((bv (make-bytevector len)))
    (do ((i 0 (+ i 1)))
	((= i len) bv)
      (bytevector-u8-set! bv i (pointer-ref-c-uint8 p i)))))

(define integer->pointer address->pointer)
(define (pointer->integer p)
  (void*->address (pointer-ptr p)))
  
)
