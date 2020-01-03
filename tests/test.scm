#!r6rs
(import (rnrs)
	(pffi)
	(srfi :64))

(test-begin "PFFI")

(define test-lib (open-shared-object "./functions.so"))

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
			(lambda (p)
			  (pointer-ref-c-int32 p 0)))))
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
    (fields (st-parent p)
	    (short attr)))
  (test-assert "struct ctr" (make-st-child (make-st-parent 
					    0 (integer->pointer 0)) 0))
  
  (let ((st (make-st-child (make-st-parent 0 (integer->pointer 0)) 0)))
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
	(test-equal "element" i (pointer-ref-c-int32 p (* i size-of-int32_t)))))
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
  (test-assert "struct ctr" (make-st-child 0 (integer->pointer 0) 0))
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
	(test-equal "element" i (pointer-ref-c-int32 p (* i size-of-int32_t)))))
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
  (test-equal (string->utf8 "foo") (pointer->bytevector (id-str "foo") 3))
  (test-equal #vu8(1 2 3) (pointer->bytevector (id-str #vu8(1 2 3)) 3)))

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
    (fields (packed p) (non-packed np)))

  (define-foreign-struct mixed-packed
    (fields (packed p) (non-packed np))
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

;; union
(let ()
  (define-foreign-struct a-st
    (fields (short s1)
	    (short s2)))
  (define-foreign-union a-union
    (fields (int i)     ;; 4
	    (pointer p) ;; 4 or 8
	    (a-st st)))
  (test-equal "union size (1)" size-of-pointer size-of-a-union)
  (let ((bv (make-a-union)))
    (test-assert "a-union? (1)" (a-union? bv))
    (test-assert "a-union-i-set (1)" (a-union-i-set! bv 1))
    (test-equal "a-union-p (1)" 1 (pointer->integer (a-union-p bv)))

    (test-equal "a-st-s1 (1)" 1 (a-st-s1 (a-union-st bv)))
    (test-equal "a-st-s2 (1)" 0 (a-st-s2 (a-union-st bv)))))

(let ()
  (define-foreign-struct a-st
    (fields (short s1)
	    (short s2)))
  (define-foreign-union a-union
    (fields (int i)     ;; 4
	    (pointer p) ;; 4 or 8
	    (a-st st))
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
  
(test-end)
