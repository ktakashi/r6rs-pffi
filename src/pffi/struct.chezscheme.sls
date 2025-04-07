;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/struct.chezscheme.sls - Foreign structure for Chez
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

;; Interact with Chez ftype, we need to have extra compat layer for Chez

#!r6rs
(library (pffi struct)
    (export define-foreign-struct
            define-foreign-union
	    fields parent protocol alignment struct)
    (import (rnrs)
	    (pffi compat)
	    (pffi helper)
	    (pffi struct helper)
	    (pffi struct chez)
            (only (chezscheme)
		  define-ftype ftype-sizeof make-weak-eq-hashtable))

(define *type-descriptors* (make-weak-eq-hashtable))
;; use fields, protocol and parent from (rnrs)
;; e.g.
;; (define-foreign-struct foreign-vector
;;   (fields (int size)
;;           (pointer elements))
;;   (protocol
;;     (lambda (p)
;;       (lambda (size)
;;         (p size (bytevector->pointer (make-bytevector size)))))))
(define-syntax define-foreign-struct
  (lambda (x)
    (define process-clauses
      (make-process-clauses x 'define-foreign-struct
			    (fields parent protocol alginment)))
    (define ->type-name (make->type-name 'define-foreign-struct (struct)))
    (syntax-case x ()
      ((k (name ctr pred) specs ...)
       (and (identifier? #'name) (identifier? #'ctr) (identifier? #'pred))
       (with-syntax (((((type field ref set) ...) parent protocol alignment)
		      (process-clauses #'k #'name #'(specs ...)))
		     (sizeof (->sizeof #'k #'name))
		     ((name) (->type-name #'(name))))
	 (with-syntax (((ft ...) (->type-name #'(type ...)))
		       (((define-ref define-set!) ...)
			(process-accessors #'k #'name #'pred
					   #'((type field ref set) ...))))
	   #`(begin
	       (define-ftype name (struct #,@(->parent #'parent) (field ft) ...))
	       (define this-protocol protocol)
	       (define sizeof (ftype-sizeof name))
	       (define dummy
		 (make-foreign-struct-descriptor
		  'name
		  sizeof
		  '((field type . #f) ...)
		  (hashtable-ref *type-descriptors* 'parent #f)
		  this-protocol))
	       (define ctr (make-constructor dummy this-protocol))
	       (define (pred o)
		 (and (bytevector? o)
		      (>= (bytevector-length o) sizeof)))
	       define-ref ...
	       define-set! ...
	       (define dummy2
		 (begin
		   (hashtable-set! *type-descriptors* 'name dummy)
		   (foreign-struct-descriptor-ctr-set! dummy ctr)
		   (foreign-struct-descriptor-getters-set! dummy (list ref ...))
		   (foreign-struct-descriptor-setters-set! dummy (list set ...))))
	       ))))
      ((k name specs ...)
       (identifier? #'name)
       (with-syntax (((ctr pred) (->ctr&pred #'k #'name)))
	 #'(k (name ctr pred) specs ...))))))

(define-syntax define-foreign-union
  (lambda (x)
    (define process-clauses
      (make-process-clauses x 'define-foreign-union
			    (fields parent protocol alginment)))
    (define ->type-name (make->type-name 'define-foreign-union (struct)))
    (syntax-case x ()
      ((k (name ctr pred) specs ...)
       (and (identifier? #'name) (identifier? #'ctr) (identifier? #'pred))
       (with-syntax (((((type field ref set) ...) parent protocol alignment)
		      (process-clauses #'k #'name #'(specs ...)))
		     (sizeof (->sizeof #'k #'name))
		     ((name) (->type-name #'(name))))
	 (when #'parent
	   (syntax-violation 'define-foreign-union "Union can't have parent" x))
	 (when #'alignment
	   (syntax-violation 'define-foreign-union "Union can't have alignment" x))
	 (with-syntax (((ft ...) (->type-name #'(type ...)))
		       (((define-ref define-set!) ...)
			(process-accessors #'k #'name #'pred
					   #'((type field ref set) ...))))
	   #`(begin
	       (define-ftype name (union (field ft) ...))
	       (define this-protocol protocol)
	       (define sizeof (ftype-sizeof name))
	       (define dummy
		 (make-foreign-struct-descriptor
		  'name
		  sizeof
		  '((field type . #f) ...)
		  (hashtable-ref *type-descriptors* 'parent #f)
		  this-protocol))
	       (define ctr (make-union-constructor dummy this-protocol))
	       (define (pred o)
		 (and (bytevector? o)
		      (>= (bytevector-length o) sizeof)))
	       define-ref ...
	       define-set! ...
	       (define dummy2
		 (begin
		   (hashtable-set! *type-descriptors* 'name dummy)
		   (foreign-struct-descriptor-ctr-set! dummy ctr)
		   (foreign-struct-descriptor-getters-set! dummy (list ref ...))
		   (foreign-struct-descriptor-setters-set! dummy (list set ...))))
	       ))))
      ((k name specs ...)
       (identifier? #'name)
       (with-syntax (((ctr pred) (->ctr&pred #'k #'name)))
         #'(k (name ctr pred) specs ...))))))
)
