#!r6rs
(import (rnrs)
	(rnrs eval)
	(pffi)
	(srfi :64))

(test-begin "PFFI")

(define test-lib (open-shared-object "./functions.so"))

(define-type-alias ppp pointer)

;; (define (print . args)
;;   (for-each display args) (newline)
;;   (flush-output-port (current-output-port)))

;; there is no particular type for this, yet
;; TODO should we make 'shared-object?' or so?
(test-assert "shared object" test-lib)

;; TODO should we make 'foreign-procedure?' or so?
;;      or can we assume it's always a procedure in any case?
(test-assert "foreign-procedure" 
	     (foreign-procedure test-lib int plus (int int)))
(test-equal "plus" 2
	    ((foreign-procedure test-lib int plus (int int)) 1 1))

(test-assert "null-pointer? (1)" (null-pointer? (integer->pointer 0)))
(test-assert "null-pointer? (2)" (not (null-pointer? (integer->pointer 1))))
(test-error (null-pointer? 'not-a-pointer))

(let ((proc (c-callback int ((int i)) (lambda (i) (* i i)))))
  (define callback-proc
    (foreign-procedure test-lib int callback_proc ((callback int (int)) int)))
  (test-equal "callback" 4 (callback-proc proc 2))
  (test-assert "free" (free-c-callback proc)))

(let ((proc (c-callback int ((pointer p)) 
			(lambda (p) (pointer-ref-c-int32 p 0)))))
  (define callback-proc
    (foreign-procedure test-lib int callback_proc2 
		       ((callback int (pointer)) int)))
  (test-equal "callback (2)" 2 (callback-proc proc 2))
  (test-assert "free" (free-c-callback proc)))

(let ((proc (c-callback pointer ((pointer p)) (lambda (p) p))))
  (define callback-proc
    (foreign-procedure test-lib pointer callback_proc3
		       ((callback pointer (pointer)) int)))
  (test-assert "callback (3)" (pointer? (callback-proc proc 2)))
  (test-assert "free" (free-c-callback proc)))

(let ()
  (define-foreign-variable test-lib int externed_variable)
  (test-equal "foreign-variable" 10 externed-variable)
  (test-assert "set! foreign-variable" (set! externed-variable 11))
  (test-equal "foreign-variable (2)" 11 externed-variable)
  (test-equal "foreign-variable (3)" 11
	      ((foreign-procedure test-lib int get_externed_variable ()))))

(let ((bv (make-bytevector (* 4 5) 0)))
  (test-assert "calling with bytevector->pointer"
	       ((foreign-procedure test-lib void fill_one (pointer int)) 
		(bytevector->pointer bv) 5))
  (test-equal "passing bytevector"
	      '(1 1 1 1 1) (bytevector->uint-list bv (native-endianness) 4)))

(test-equal "size-of-char"  1 size-of-char)
(test-equal "size-of-short" 2 size-of-short)
(test-equal "size-of-int"   4 size-of-int)
(test-assert "size-of-long"   (memv size-of-long '(4 8)))
(test-assert "size-of-pointer"   (memv size-of-pointer '(4 8)))
;; I think we can assume this
(test-equal "size-of-float"  4 size-of-float)
(test-equal "size-of-double" 8 size-of-double)
(test-equal "size-of-int8_t" 1 size-of-int8_t)
(test-equal "size-of-int16_t" 2 size-of-int16_t)
(test-equal "size-of-int32_t" 4 size-of-int32_t)
(test-equal "size-of-int64_t" 8 size-of-int64_t)

;; pointer operations
(let* ((bv (u8-list->bytevector '(0 1 2 3 4 5 6 7 8 9 0)))
       (p (bytevector->pointer bv)))
  ;; for convenience
  (define (bytevector-u8-ref/endian bv index endian)
    (bytevector-u8-ref bv index))
  (define (bytevector-u8-set/endian! bv index v endian)
    (bytevector-u8-set! bv v index))
  (define (bytevector-s8-ref/endian bv index endian)
    (bytevector-s8-ref bv index))
  (define (bytevector-s8-set/endian! bv index v endian)
    (bytevector-s8-set! bv v index))

  (define-syntax test-pointer-ref
    (syntax-rules ()
      ((_ p-ref bv-ref)
       (test-equal 'p-ref (bv-ref bv 1 (native-endianness)) (p-ref p 1)))))

  (define-syntax test-pointer-set!
    (lambda (x)
      (define (->names type)
	(let ((s (symbol->string (syntax->datum type))))
	  (list (string->symbol (string-append "pointer-ref-c-" s))
		(string->symbol (string-append "pointer-set-c-" s "!")))))
      (syntax-case x ()
	((k type value)
	 (with-syntax (((ref set) (datum->syntax #'k (->names #'type))))
	   #'(begin
	       (test-equal 'set value
			   (let* ((t (bytevector-copy bv))
				  (p (bytevector->pointer t)))
			     (set p 1 value)
			     (ref p 1)))))))))
  (test-assert "pointer?" (pointer? p))
  (test-pointer-ref pointer-ref-c-int8 bytevector-s8-ref/endian)
  (test-pointer-ref pointer-ref-c-uint8 bytevector-u8-ref/endian)
  (test-pointer-ref pointer-ref-c-int16 bytevector-s16-ref)
  (test-pointer-ref pointer-ref-c-uint16 bytevector-u16-ref)
  (test-pointer-ref pointer-ref-c-int32 bytevector-s32-ref)
  (test-pointer-ref pointer-ref-c-uint32 bytevector-u32-ref)
  (test-pointer-ref pointer-ref-c-int64 bytevector-s64-ref)
  (test-pointer-ref pointer-ref-c-uint64 bytevector-u64-ref)
  (test-pointer-ref pointer-ref-c-float bytevector-ieee-single-ref)
  (test-pointer-ref pointer-ref-c-double bytevector-ieee-double-ref)
  (test-equal "pointer-ref-c-pointer"
	      (if (= size-of-pointer 8)
		  (bytevector-u64-ref bv 1 (native-endianness))
		  (bytevector-u32-ref bv 1 (native-endianness)))
	      (pointer->integer (pointer-ref-c-pointer p 1)))

  ;; sets
  (test-pointer-set! int8 -128)
  (test-pointer-set! int8  127)
  (test-pointer-set! uint8 0)
  (test-pointer-set! uint8 255)
  (test-pointer-set! int16  #x-8000)
  (test-pointer-set! int16  #x7FFF)
  (test-pointer-set! uint16 0)
  (test-pointer-set! uint16 #xFFFF)
  (test-pointer-set! int32  #x-80000000)
  (test-pointer-set! int32  #x7FFFFFFF)
  (test-pointer-set! uint32 0)
  (test-pointer-set! uint32 #xFFFFFFFF)
  ;; lazy...
  (test-pointer-set! int64  #x-80000000)
  (test-pointer-set! int64  #x7FFFFFFF)
  (test-pointer-set! uint64 0)
  (test-pointer-set! uint64 #xFFFFFFFF)
  (test-pointer-set! float 1.0)
  (test-pointer-set! double 1.0)
  (test-equal 'pointer-set-c-pointer! 12345
	      (let* ((t (bytevector-copy bv))
		     (p (bytevector->pointer t)))
		(pointer-set-c-pointer! p 1 (integer->pointer 12345))
		(pointer->integer (pointer-ref-c-pointer p 1))))
)

;; struct field
(let ()
  (define-foreign-struct st-parent
    (fields (int count)
	    (pointer elements)))
  (define-foreign-struct st-child
    (fields ((struct st-parent) p)
	    (short attr)))
  (test-assert "struct ctr (0)" (make-st-child (make-st-parent 
					    0 (integer->pointer 0)) 0))
  (let ((st (make-st-child (make-st-parent 0 (integer->pointer 0)) 0)))
    (define (check-elements p)
      (do ((i 0 (+ i 1))) ((= i 10) #t)
	(test-equal (string-append "element (" (number->string i) ")")
		    i (pointer-ref-c-int32 p (* i size-of-int32_t)))))
    (test-assert "predicate (child)" (st-child? st))
    (test-assert "predicate (parent)" (st-parent? st))
    (test-assert "predicate (bv)" (bytevector? st))
    (test-equal "size" size-of-st-child (bytevector-length st))
    ((foreign-procedure test-lib void fill_st_values (pointer))
     (bytevector->pointer st))
    (test-equal "count" 10 (st-parent-count st))
    (test-assert "elements" (st-parent-elements st))
    (check-elements (st-parent-elements st))
    (let ((parent (st-child-p st)))
      (check-elements (st-parent-elements parent)))
    (test-equal "attr" 5 (st-child-attr st))
    ((foreign-procedure test-lib void free_st_values (pointer))
     (bytevector->pointer st))))

;; parent
(let ()
  (define-foreign-struct st-parent
    (fields (int count)
	    (pointer elements)))
  (define-foreign-struct st-child
    (fields (short attr))
    (parent st-parent))
  (test-assert "struct ctr (1)" (make-st-child 0 (integer->pointer 0) 0))
  (let ((st (make-st-child 0 (integer->pointer 0) 0)))
    (test-assert "predicate (child)" (st-child? st))
    (test-assert "predicate (parent)" (st-parent? st))
    (test-assert "predicate (bv)" (bytevector? st))
    (test-equal "size" size-of-st-child (bytevector-length st))
    ((foreign-procedure test-lib void fill_st_values (pointer))
     (bytevector->pointer st))
    (test-equal "count" 10 (st-parent-count st))
    (test-assert "elements" (st-parent-elements st))
    (let ((p (st-parent-elements st)))
      (do ((i 0 (+ i 1))) ((= i 10) #t)
	(test-equal (string-append "element (" (number->string i) ")")
		    i (pointer-ref-c-int32 p (* i size-of-int32_t)))))
    (test-equal "attr" 5 (st-child-attr st))
    ((foreign-procedure test-lib void free_st_values (pointer))
     (bytevector->pointer st))))

;; protocol thing
(let ()
  (define-foreign-struct st-parent
    (fields (int count)
	    (pointer elements))
    (protocol
     (lambda (p)
       (lambda (size)
	 (p size (integer->pointer 0))))))
  (define-foreign-struct st-child
    (fields (short attr))
    (parent st-parent)
    (protocol
     (lambda (n)
       (lambda (size attr)
	 ((n size) attr)))))
  (test-assert "struct ctr" (make-st-child 0 0))
  (let ((st (make-st-child 5 10)))
    (test-assert "predicate (child)" (st-child? st))
    (test-assert "predicate (parent)" (st-parent? st))
    (test-assert "predicate (bv)" (bytevector? st))
    (test-equal "parent-slot" 5 (st-parent-count st))
    (test-equal "this-slot" 10 (st-child-attr st))
    ))

;; passing string or bytevector to pointer type
(let ((id-str (foreign-procedure test-lib pointer id_str (pointer))))
  (test-equal "id-str (1)"
	      (string->utf8 "foo") (pointer->bytevector (id-str "foo") 3))
  (test-equal "id-str (2)"
	      #vu8(1 2 3) (pointer->bytevector (id-str #vu8(1 2 3)) 3)))

;; union
(let ()
  (define-foreign-struct a-st
    (fields (short s1)
	    (short s2)))
  (define-foreign-union a-union
    (fields (int i)     ;; 4
	    (pointer p) ;; 4 or 8
	    ((struct a-st) st)))
  (test-equal "union size (1)" size-of-pointer size-of-a-union)
  (let ((bv (make-a-union)))
    (test-assert "a-union? (1)" (a-union? bv))
    (test-assert "a-union-i-set (1)" (a-union-i-set! bv 1))
    (test-equal "a-union-p (1)" 1 (pointer->integer (a-union-p bv)))

    (test-equal "a-st-s1 (1)" 1 (a-st-s1 (a-union-st bv)))
    (test-equal "a-st-s2 (1)" 0 (a-st-s2 (a-union-st bv)))))

;; Next offset computation was incorrect.
(let ()
  (define-foreign-struct WIN-BORDER
    (fields (int ls)
	    (int rs)
	    (int ts)
	    (int bs)
	    (int tl)
	    (int tr)
	    (int bl)
	    (int br)))
  (define-foreign-struct WIN
    (fields (int startx)
	    (int starty)
	    (int height)
	    (int width)
	    ((struct WIN-BORDER) border)))
  (define c char->integer)

  (test-equal (* 8 size-of-int) size-of-WIN-BORDER)
  (test-equal (+ (* 4 size-of-int) size-of-WIN-BORDER)
	      size-of-WIN)
  (let ((border (make-WIN-BORDER (c #\|) (c #\|) (c #\-) (c #\-)
				 (c #\+) (c #\+) (c #\+) (c #\+)))
	(height 3)
	(width 10))
    (test-assert (make-WIN height width 0 0 border))))


(let ()
  (define-foreign-struct a-st
    (fields (short s1)
	    (short s2)))
  (define-foreign-union a-union
    (fields (int i)     ;; 4
	    (pointer p) ;; 4 or 8
	    ((struct a-st) st))
    (protocol (lambda (p)
		(lambda (i)
		  (p 'i i)))))
  (test-equal "union size (2)" size-of-pointer size-of-a-union)
  (let ((bv (make-a-union 2)))
    (test-assert "a-union? (2)" (a-union? bv))
    ;;(test-assert "a-union-i-set" (a-union-i-set! bv 1))
    (test-equal "a-union-p (2)" 2 (pointer->integer (a-union-p bv)))

    (test-equal "a-st-s1 (2)" 2 (a-st-s1 (a-union-st bv)))
    (test-equal "a-st-s2 (2)" 0 (a-st-s2 (a-union-st bv)))))

(let ()
  (test-error "field is not a symbol"
	      syntax-violation?
	      (eval '(define-foreign-struct a
		       (fields ((callback double (double pointer))  function)))
		    (environment '(pffi))))
  (test-error "field is not a symbol (union)"
	      syntax-violation?
	      (eval '(define-foreign-union b
		       (fields ((callback double (double pointer))  function)))
		    (environment '(pffi)))))

;; varargs
(let ((sum (foreign-procedure test-lib int sum (int ___))))
  (test-equal "variadic argument" 10 (sum 4 1 2 3 4)))

;; typedef
(let ((id-str (foreign-procedure test-lib ppp id_str (ppp))))
  (test-equal "id-str (1)"
	      (string->utf8 "foo") (pointer->bytevector (id-str "foo") 3))
  (test-equal "id-str (2)"
	      #vu8(1 2 3) (pointer->bytevector (id-str #vu8(1 2 3)) 3)))

(let ()
  (define-foreign-struct st-ppp
    (fields (ppp p)))
  (define-foreign-struct un-ppp
    (fields (ppp p)))
  (test-assert "make-st-ppp" (make-st-ppp (integer->pointer 0)))
  (test-assert "make-un-ppp" (make-un-ppp (integer->pointer 0))))

;; boolean type
(let ((is-even? (foreign-procedure test-lib boolean is_even (int)))
      (check-dispatch
       (foreign-procedure test-lib boolean check_dispatch (int boolean))))
  ;; (display size-of-boolean)
  ;; We don't check the size of boolean here, as some use C99 bool
  ;; and some, or Chez..., use just int
  (test-assert "size-of-boolean" size-of-boolean)
  (test-assert "is-even? (1)" (boolean? (is-even? 2)))
  (test-assert "is-even? (2)" (not (is-even? 1)))
  (test-assert "check-dispatch (1)" (check-dispatch 2 #t))
  (test-assert "check-dispatch (2)" (not (check-dispatch 2 #f))))


(test-end)
