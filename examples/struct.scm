#!r6rs
(import (rnrs) (pffi))

(define libstruct
  (open-shared-object "struct.so"))

(define fill-struct (foreign-procedure libstruct void fill_struct (pointer)))

(define-foreign-struct st1
  (fields (int count)
	  (pointer elements)))
(define-foreign-struct st2
  (fields (st1 p)
	  (short attr)))
(define-foreign-struct st2*
  (fields (short attr))
  (parent st1))

(let ((st (make-st2 (make-st1 0 (integer->pointer 0)) 0))
      (st* (make-st2* 0 (integer->pointer 0) 0)))
  (fill-struct st)
  (fill-struct st*)
  (display (st1-count st)) (newline)
  (display st) (newline)
  (display (st2-p st)) (newline)
  (display (st1-count (st2-p st))) (newline)
  (display (st1-count st*)) (newline))

