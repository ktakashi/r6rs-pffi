#!r6rs
(import (rnrs)
	(pffi)
	(srfi :64)
	(rename (pffi bv-pointer)
		(bytevector->pointer bytevector->address))
	(only (chezscheme) collect))

(test-begin "PFFI Chez specific")

(define test-lib (open-shared-object "./functions.so"))
(define fill-one (foreign-procedure test-lib void fill_one (pointer int)))

(define (allocate-alot n)
  (do ((i 0 (+ i 1))
       (bv (make-bytevector n) (make-bytevector (bytevector-length bv))))
      ((= i 100000))
    (bytevector-u8-set! bv (mod i n) (mod i 255))))

(let* ((bv (make-bytevector (* 4 5) 0))
       (orig-address (bytevector->address bv)))
  (fill-one (bytevector->pointer bv) 1)
  (test-equal '(1 0 0 0 0) (bytevector->uint-list bv (native-endianness) 4))
  (collect)
  (fill-one (bytevector->pointer bv) 2)
  (test-equal '(1 1 0 0 0) (bytevector->uint-list bv (native-endianness) 4))
  (test-assert (not (= orig-address (bytevector->address bv)))))
  

(test-end)

