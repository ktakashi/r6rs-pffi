;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/helper.chezscheme.sls - Helper for Chez Scheme
;;;
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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
(library (pffi helper)
    (export pffi-type->foreign-type
	    registered-alias?
	    adjust-argument-types
	    callback ___
	    define-type-alias)
    (import (rnrs)
	    (pffi global)
	    (only (chezscheme) reverse! define-ftype))

;; Because chez's foreign-procedure is a syntax
;; we need to do a bit sloppy way of handling typedef...
(define-syntax define-type-alias
  (lambda (x)
    (define (register-type-alias! name alias)
      (hashtable-set! *typedef-table* name alias))
    (syntax-case x ()
      ((_ name alias)
       (register-type-alias! (syntax->datum #'name)
			     (syntax->datum #'alias))
       #'(define-ftype name alias)))))

(define (registered-alias? v) (hashtable-ref *typedef-table* v #f))

;; We need it here for free-identifier
(define ___            '___) ;; varargs
(define (pffi-type->foreign-type type)
  (define resolved (hashtable-ref *typedef-table* type type))
  (case resolved
    ((void          ) 'void)
    ((char          ) 'integer-8)
    ((unsigned-char ) 'unsigned-8)
    ((short         ) 'short)
    ((unsigned-short) 'unsigned-short)
    ((int           ) 'int)
    ((unsigned-int  ) 'unsigned-int)
    ((long          ) 'long)
    ((unsigned-long ) 'unsigned-long)
    ((int8_t        ) 'integer-8)
    ((uint8_t       ) 'unsigned-8)
    ((int16_t       ) 'integer-16)
    ((uint16_t      ) 'unsigned-16)
    ((int32_t       ) 'integer-32)
    ((uint32_t      ) 'unsigned-32)
    ((int64_t       ) 'integer-64)
    ((uint64_t      ) 'unsigned-64)
    ((double        ) 'double)
    ((float         ) 'float)
    ((pointer       ) 'void*)
    ((___           ) #f) ;; ignore this for Chez
    ;; let chez complain if not defined
    (else (if (eq? resolved type)
	      resolved
	      ;; maybe typedef of typedef 
	      (pffi-type->foreign-type resolved)))))

(define-syntax callback
  (syntax-rules ()
    ((_ ignore ...) void*)))

(define (adjust-argument-types args)
  (define (types args acc varargs?)
    (syntax-case args (callback ___)
      (() (list (reverse! acc) varargs?))
      (((callback ret (ignore ...)) rest ...)
       (types #'(rest ...) (cons #'void* acc) varargs?))
      (((callback ignore ...) rest ...)
       (syntax-violation 'foreign-procedure "Invalid callback format" args))
      ((___ a rest ...)
       (syntax-violation 'foreign-procedure "___ must be the last" args))
      ((___) (types #'() acc #t))
      ((type rest ...) (types #'(rest ...) (cons #'type acc) varargs?))))
  (types args '() #f))
)
