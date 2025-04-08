;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/ffi-type-descriptor.sls - Foreign type descriptor
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
(library (pffi ffi-type-descriptor)
    (export (rename (type-descriptor <type-descriptor>))
	    type-descriptor?
	    type-descriptor-name type-descriptor-size

	    ffi-type-descriptor? make-ffi-type-descriptor
	    (rename (type-descriptor-name ffi-type-descriptor-name)
		    (type-descriptor-size ffi-type-descriptor-size))
	    ffi-type-descriptor-alias

	    (rename (foreign-struct-descriptor <foreign-struct-descriptor>))
	    foreign-struct-descriptor? make-foreign-struct-descriptor
	    (rename (type-descriptor-name foreign-struct-descriptor-name)
		    (type-descriptor-size foreign-struct-descriptor-size))
	    foreign-struct-descriptor-fields
	    foreign-struct-descriptor-parent
	    foreign-struct-descriptor-protocol
	    foreign-struct-descriptor-protocol-set!
	    foreign-struct-descriptor-has-protocol?
	    foreign-struct-descriptor-ctr foreign-struct-descriptor-ctr-set!
	    foreign-struct-descriptor-getters
	    foreign-struct-descriptor-getters-set!
	    foreign-struct-descriptor-setters
	    foreign-struct-descriptor-setters-set!

	    make-generic-foreign-struct-descriptor
	    generic-foreign-struct-descriptor-alignment
	    generic-foreign-struct-descriptor-type-ref
	    generic-foreign-struct-descriptor-type-set!
	    )
    (import (rnrs))
(define-record-type type-descriptor
  (fields name size))

;; primitive types
(define-record-type ffi-type-descriptor
  (parent type-descriptor)
  (fields alias)
  (protocol (lambda (n)
	      (lambda (name alias size)
		((n name size) alias)))))

;; this can be in struct/helper.sls but Guile doesn't like it...
(define-record-type foreign-struct-descriptor
  (parent type-descriptor)
  (fields fields
          parent
	  (mutable protocol)
          has-protocol?
          (mutable ctr)
	  (mutable getters)
	  (mutable setters))
  (protocol (lambda (p)
	      (lambda (name size fields parent protocol)
		((p name size) fields parent #f protocol #f '() '())))))

;; this can be struct.sls, but Guile doesn't like it...
(define-record-type generic-foreign-struct-descriptor
  (parent foreign-struct-descriptor)
  (fields alignment type-ref type-set!)
  (protocol (lambda (n)
              (lambda (name size alignment fields parent proto ref set)
                ((n name size fields parent proto)
		 alignment ref set)))))


)
