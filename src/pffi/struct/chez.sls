;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/struct/chez.sls - Chez specific helper
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
(library (pffi struct chez)
    (export process-accessors
	    ->parent
	    ;; funny that we need to export these 2... (meta -1) binding issue?
	    ->value struct-field-address)
    (import (rnrs)
	    (pffi compat)
	    (pffi helper)
	    (pffi struct helper)
	    (only (chezscheme)
		  make-ftype-pointer object->reference-address
		  ftype-pointer-address
		  ftype-ref ftype-set! ftype-sizeof
		  ftype-&ref foreign-set! foreign-ref))

(define (->parent parent)
  (if parent
      (with-syntax ((parent parent))
	#'((dummy parent)))
      #'()))

(define (process-accessors k name pred fields)
  (with-syntax ((name name) (pred pred))
    (let loop ((fields fields) (r '()))
      (syntax-case fields (struct)
	(() (reverse r))
	(((type field ref set) rest ...)
	 (identifier? #'type)
	 (loop #'(rest ...)
	       (cons #'((define (ref o)
			  (unless (pred o)
			    (assertion-violation 'ref
			      "It's not a struct instance" o))
			  (let* ((p (object->reference-address o))
				 (fp (make-ftype-pointer name p)))
			    (let ((r (ftype-ref name (field) fp)))
			      (case (pffi-type->foreign-type 'type)
				((void*) (integer->pointer r))
				(else r)))))
			(define (set o v)
			  (unless (pred o)
			    (assertion-violation 'set
			      "It's not a struct instance" o))
			  (let* ((p (object->reference-address o))
				 (fp (make-ftype-pointer name p)))
			    (ftype-set! name (field) fp (->value v)))))
		     r)))
	((((struct type) field ref set) rest ...)
	 (loop #'(rest ...)
	       (cons #'((define (ref o)
			  (unless (pred o)
			    (assertion-violation 'ref
			      "It's not a struct instance" o))
			  (let ((ad (struct-field-address o name field))
				(bv (make-bytevector (ftype-sizeof type))))
			    (do ((i 0 (+ i 1)))
				((= i (bytevector-length bv)) bv)
			      (bytevector-u8-set! bv i 
				(foreign-ref 'unsigned-8 ad i)))))
			(define (set o v)
			  (unless (pred o)
			    (assertion-violation 'set
			      "It's not a struct instance" o))
			  (let ((ad (struct-field-address o name field)))
			    (do ((i 0 (+ i 1)))
				((= i (bytevector-length v)))
			      (foreign-set! 'unsigned-8 ad i
				(bytevector-u8-ref v i))))))
		     r)))))))

(define (->value v)
  (if (pointer? v)
      (pointer->integer v)
      v))

(define-syntax struct-field-address
  (syntax-rules ()
    ((_ o name field)
     (let* ((p (object->reference-address o))
	    (fp (make-ftype-pointer name p)))
       (ftype-pointer-address (ftype-&ref name (field) fp))))))
)
