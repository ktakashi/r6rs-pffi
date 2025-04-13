;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/variable/helper.sls - Foreign varialbe helper
;;;
;;;   Copyright (c) 2015-2025  Takashi Kato  <ktakashi@ymail.com>
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
#!r6rs
(library (pffi variable helper)
    (export type->pointer-ref
	    type->pointer-set!
	    type->size-of)
    (import (rnrs)
	    (only (chezscheme) foreign-ref foreign-set! ftype-sizeof)
	    (pffi compat)
	    (pffi helper)
	    (pffi ffi-type-descriptor))
;; maybe, we need to make pointer-ref generic...
(define-syntax type->pointer-ref
  (lambda (x)
    (syntax-case x ()
      ((_ type)
       (if (eq? (pffi-type->foreign-type (syntax->datum #'type)) 'void*)
	   #'(lambda (ptr offset)
	       (integer->pointer
		(foreign-ref (pffi-type->foreign-type 'type)
			     (pointer->integer ptr) offset)))
	   #'(lambda (ptr offset)
	       (foreign-ref (pffi-type->foreign-type 'type)
			    (pointer->integer ptr) offset)))))))

(define-syntax type->pointer-set!
  (lambda (x)
    (syntax-case x ()
      ((_ type)
       (if (eq? (pffi-type->foreign-type (syntax->datum #'type)) 'void*)
	   #'(lambda (ptr offset value)
	       (foreign-set! (pffi-type->foreign-type 'type)
			     (pointer->integer ptr)
			     offset (pointer->integer value)))
	   #'(lambda (ptr offset value)
	       (foreign-set! (pffi-type->foreign-type 'type)
			     (pointer->integer ptr) offset value)))))))

(define-syntax type->size-of (identifier-syntax ftype-sizeof))

)
