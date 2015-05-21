#!r6rs
(import (rnrs) (pffi))

(define lib (open-shared-object "variable.so"))

(define (print . args)
  (for-each display args) (newline)
  (flush-output-port (current-output-port)))

(define-foreign-variable lib int global_int)

(print global-int)
(set! global-int (+ global-int 1))
(print global-int)
(print ((foreign-procedure lib int get_global_int ())))

;; string
((foreign-procedure lib void init_global_string ()))
(define-foreign-variable lib pointer global_string)


(print global-string) ;; pointer

(define (null-terminate-pointer->string p)
  (let loop ((i 0) (l '()))
    (let ((c (pointer-ref-c-uint8 p i)))
      (if (zero? c)
	  (list->string (reverse l))
	  (loop (+ i 1) (cons (integer->char c) l))))))

(print (null-terminate-pointer->string global-string))

(pointer-set-c-uint8! global-string 0 (char->integer #\H))
(print (null-terminate-pointer->string global-string))
(print (null-terminate-pointer->string
	((foreign-procedure lib pointer get_global_string ()))))
