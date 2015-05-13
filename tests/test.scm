#!r6rs
(import (rnrs)
	(pffi)
	(srfi :64))

(test-begin "PFFI")

(define test-lib (open-shared-object "functions.so"))

;; there is no particular type for this, yet
;; TODO should we make 'shared-object?' or so?
(test-assert "shared object" test-lib)

;; TODO should we make 'foreign-procedure?' or so?
;;      or can we assume it's always a procedure in any case?
(test-assert "foreign-procedure" 
	     (foreign-procedure test-lib int plus (int int)))

(test-equal "plus" 2
	    ((foreign-procedure test-lib int plus (int int)) 1 1))

(define callback-proc
  (foreign-procedure test-lib int callback_proc ((callback int (int)) int)))
(let ((proc (c-callback int ((int i)) (lambda (i) (* i i)))))
  (test-equal "callback" 4 (callback-proc proc 2))
  (test-assert "free" (free-c-callback proc)))

(test-end)
