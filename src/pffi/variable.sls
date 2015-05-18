;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/variable.sls - Foreign variable
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

#!r6rs
(library (pffi variable)
  (export define-foreign-variable
	  ;; TODO should we export them from here?
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
	  pointer-set-c-pointer!)
  (import (rnrs)
	  (for (only (srfi :13) string-map) expand)
	  (pffi compat))

  ;; to make FFI variable settable, we use macro
  (define-syntax define-foreign-variable
    (lambda (x)
      (define (->scheme-name name)
	(string->symbol
	 (string-map (lambda (c) (if (char=? c #\_) #\- c))
		     (string-downcase (symbol->string (syntax->datum name))))))
      (define (derefs t)
	(let ((s (symbol->string (syntax->datum t))))
	  (list (string->symbol (string-append "pointer-ref-c-" s))
		(string->symbol (string-append "pointer-set-c-" s "!")))))
      (syntax-case x ()
	((k lib type name)
	 (with-syntax ((scheme-name
			(datum->syntax #'k (->scheme-name #'name))))
	   #'(k lib type name scheme-name)))
	((k lib type name scheme-name)
	 (with-syntax (((pointer-ref pointer-set!)
			(datum->syntax #'k (derefs #'type))))
	   #'(begin
	       (define dummy (lookup-shared-object lib (symbol->string 'name)))
	       (define-syntax scheme-name
		 (identifier-syntax
		  (_ (pointer-ref dummy 0))
		  ((set! _ e) (pointer-set! dummy 0 e))))))))))
  
  )
