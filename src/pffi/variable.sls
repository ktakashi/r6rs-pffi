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
  (export define-foreign-variable define-type-alias array)
  (import (rnrs)
          (for (pffi misc) expand)
	  (only (pffi misc) define-type-alias)
          (pffi compat)
	  (pffi variable helper))

(define-syntax array (syntax-rules ()))

;; to make FFI variable settable, we use macro
(define-syntax define-foreign-variable
  (lambda (x)
    (define (->scheme-name name)
      (string->symbol
       (string-map (lambda (c) (if (char=? c #\_) #\- c))
                   (string-downcase (symbol->string (syntax->datum name))))))
    (syntax-case x (* array)
      ((k lib type name)
       (with-syntax ((scheme-name
                      (datum->syntax #'k (->scheme-name #'name))))
         #'(k lib type name scheme-name)))
      ((k lib (* type) name scheme-name)
       (identifier? #'type)
       #'(k lib pointer name scheme-name))
      ((k lib type name scheme-name)
       (identifier? #'type)
       #'(begin
	   (define pointer-ref (type->pointer-ref type))
	   (define pointer-set! (type->pointer-set! type))
           (define dummy (lookup-shared-object lib (symbol->string 'name)))
           (define-syntax scheme-name
             (identifier-syntax
              (_ (pointer-ref dummy 0))
              ((set! _ e) (pointer-set! dummy 0 e))))))
      
      ((k lib (array type) name scheme-name)
       (identifier? #'type)
       #'(begin
	   (define pointer-ref (type->pointer-ref type))
	   (define pointer-set! (type->pointer-set! type))
	   (define size-of (type->size-of type))
           (define dummy (lookup-shared-object lib (symbol->string 'name)))
           (define-syntax scheme-name
	     (make-variable-transformer
	      (lambda (xx)
		(syntax-case xx (set!)
		  ((set! scheme-name (n val))
		   #'(pointer-set! dummy (* size-of n) val))
		  ((_ n) #'(pointer-ref dummy (* size-of n)))
		  (id (identifier? #'id) #'dummy))))))))))
)
