#!r6rs
(import (rnrs)
	(pffi)
	(srfi :64))

(test-begin "PFFI struct alignment")

;; alignment
(define (?p p8 p4)
  (if (= size-of-pointer 8)
      p8
      p4))


(let ()
  (define-foreign-struct packed
    (fields (char c) (short s) (pointer p))
    (alignment 4))

  (define-foreign-struct non-packed
    (fields (char c) (short s) (pointer p)))

  (define-foreign-struct mixed
    (fields ((struct packed) p) ((struct non-packed) np)))

  (define-foreign-struct mixed-packed
    (fields ((struct packed) p) ((struct non-packed) np))
    (alignment 4))

  (test-equal "size-of-packed" (?p 12 8) size-of-packed)
  (test-equal "size-of-non-packed" (?p 16 8) size-of-non-packed)
  (test-equal "size-of-mixed" (?p 32 16) size-of-mixed)
  (test-equal "size-of-mixed-packed" (?p 28 16) size-of-mixed-packed)

  (let ((p (make-packed 20 1 (integer->pointer 2)))
	(np (make-non-packed 20 1 (integer->pointer 2))))
    (test-equal "packed instance" size-of-packed (bytevector-length p))
    (test-equal "packed-c" 20 (packed-c p))
    (test-equal "packed-s" 1 (packed-s p))
    (test-equal "packed-p" 2 (pointer->integer (packed-p p)))
    
    (let ((m (make-mixed p np)))
      (test-equal "mixed instance" size-of-mixed (bytevector-length m))
      (test-equal "mixed-p" p (mixed-p m))
      (test-equal "mixed-np" np (mixed-np m)))

    (let ((m (make-mixed-packed p np)))
      (test-equal "mixed-packed instance" size-of-mixed-packed (bytevector-length m))
      (test-equal "mixed-packed-p" p (mixed-packed-p m))
      (test-equal "mixed-packed-np" np (mixed-packed-np m)))))

(test-end)
(exit (test-runner-fail-count (test-runner-current)))
