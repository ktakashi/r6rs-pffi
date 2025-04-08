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
	    (rnrs eval)
            (ffi unsafe)
            (ffi vector)
	    (for (rename (only (racket base) cons string->keyword read)
			 (cons icons)
			 (read racket:read))
		 run expand)
	    (pffi misc)
	    (pffi ffi-type-descriptor)
            (only (srfi :13) string-index-right))

(define-syntax define-ftype
  (syntax-rules ()
    ((_ name alias)
     (define name
       (make-ffi-type-descriptor 'name alias (ctype-sizeof alias))))))
(define-ftype char           _sint8)
(define-ftype unsigned-char  _uint8)
(define-ftype short          _sshort)
(define-ftype unsigned-short _ushort)
(define-ftype int            _sint)
(define-ftype unsigned-int   _uint)
(define-ftype long           _slong)
(define-ftype unsigned-long  _ulong)
(define-ftype float          _float)
(define-ftype double         _double)
(define-ftype int8_t         _int8)
(define-ftype uint8_t        _uint8)
(define-ftype int16_t        _int16)
(define-ftype uint16_t       _uint16)
(define-ftype int32_t        _int32)
(define-ftype uint32_t       _uint32)
(define-ftype int64_t        _int64)
(define-ftype uint64_t       _uint64)
(define-ftype pointer        _pointer)
(define ___            '___)

;; for convenience
(define-ftype int8         _int8)
(define-ftype uint8        _uint8)
(define-ftype int16        _int16)
(define-ftype uint16       _uint16)
(define-ftype int32        _int32)
(define-ftype uint32       _uint32)
(define-ftype int64        _int64)
(define-ftype uint64       _uint64)

(define-syntax callback
  (syntax-rules ()
    ((_ ret (args ...))
     (_cprocedure
      (->immutable-list (list (ffi-type-descriptor-alias args) ...))
      (ffi-type-descriptor-alias ret)))))
(define-ftype void           _void)

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

(define :varargs-after (string->keyword "varargs-after"))

(define (make-c-function lib conv oret name arg-type)
  (define ret (ffi-type-descriptor-alias oret))
  (define (parse-arg-types arg-type)
    (let loop ((n 0) (r '()) (types arg-type))
      (cond ((null? types) (values (reverse r) #f))
	    ((eq? ___ (car types))
	     (unless (null? (cdr types))
	       (assertion-violation 'make-c-function "___ must be the last"
				    arg-type))
	     (values (reverse r) n))
	    (else
	     (let ((t (car types)))
	       (if (ffi-type-descriptor? t)
		   (loop (+ n 1) (cons (ffi-type-descriptor-alias t) r)
			 (cdr types))
		   (loop (+ n 1) (cons t r) (cdr types))))))))
  (let-values (((required-type after) (parse-arg-types arg-type)))
    (define (convert-arg type arg)
      (cond ((eq? type _pointer)
	     (cond ((string? arg)
		    (string->utf8 (string-append arg "\x0;")))
		   (else arg)))
	    (else arg)))
    (define (arg->type arg)
      (cond ((string? arg) _string)
	    ((symbol? arg) _symbol)
	    ((bytevector? arg) _pointer)
	    ((number? arg)
	     (cond ((and (exact? arg) (integer? arg))
		    (cond ((fixnum? arg) _fixnum)
			  ((<= (bitwise-length arg) 64) _int64)
			  (else (assertion-violation 'make-c-function
						     "Number is too big" arg))))
		   ((real? arg) _double)
		   (else (assertion-violation 'make-c-function
					      "Complex number not supported"
					      arg))))
	    ((cpointer? arg) _pointer)
	    (else (assertion-violation 'make-c-function
				       "Unsupported argument" arg))))
    (if after
	(lambda arg*
	  (define (cprocedure input-type output-type after)
	    ;; Using eval for keyword argument...
	    (eval `(_cprocedure ',input-type ',output-type
				,:varargs-after ,after)
		  (environment '(racket base) '(pffi compat) '(ffi unsafe))))
	  (let-values (((required rest) (split-at arg* after)))
	    (let ((converted (map convert-arg required-type required))
		  (rest-types (map arg->type rest)))
	      (define f (get-ffi-obj
			 (symbol->string name) lib
			 (cprocedure
			  (->immutable-list (append required-type rest-types))
			  ret after)
			 (lambda ()
			   (error 'make-c-function "not found" name))))
	      (apply f (append converted rest)))))
	(let ((f (get-ffi-obj (symbol->string name) lib
			      ;; DAMN YOU MORON!!!
			      ;; seems this doesn't accept mutable pairs
			      ;; so convert it.
			      (_cprocedure (->immutable-list required-type) ret)
			      (lambda ()
				(error 'make-c-function "not found" name)))))
	  (lambda arg* (apply f (map convert-arg required-type arg*)))))))

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
             (define ref
	       (let ((t (ffi-type-descriptor-alias type)))
		 (lambda (ptr offset)
		   (let ((bv (make-bytevector size)))
                     (do ((i 0 (+ i 1)))
			 ((= i size) (ptr-ref bv t 0))
                       (bytevector-u8-set! bv i
			(ptr-ref ptr _uint8 (+ i offset))))))))
             (define (set! ptr offset value)
               (let ((bv (->bv value)))
                 (do ((len (bytevector-length bv)) (i 0 (+ i 1)))
                     ((= i len))
                   (ptr-set! ptr _uint8 (+ offset i)
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
        ((= i size-of-pointer) (ptr-ref bv _pointer 0))
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
         #'(define name (ctype-sizeof (ffi-type-descriptor-alias type))))))))
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
  ;;(cast bv _scheme _gcpointer)
  (u8vector->cpointer bv))

(define (pointer->bytevector p len . maybe-offset)
  (define offset (if (null? maybe-offset) 0 (car maybe-offset)))
  ;; For CS implementation, make-sized-byte-string is not supported
  ;; So we do manually (since Racket v8?)
  (let ((bv (make-bytevector len)))
    (do ((i 0 (+ i 1)))
	((= i len) bv)
      (bytevector-u8-set! bv i (pointer-ref-c-uint8 p (+ i offset))))))

;; assume it's _gcpointer...
(define (pointer->integer p) (cast p _pointer _sintptr))
(define (integer->pointer i) (cast i _sintptr _pointer))

)
