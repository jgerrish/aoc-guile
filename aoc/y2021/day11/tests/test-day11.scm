;;
;; Test day11
;;
(define-module (aoc y2021 day11 tests day11)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 exceptions)
  #:use-module ((rnrs hashtables) :version (6))
  #:use-module (aoc y2021 day11 day11))

;; Test parse-line
(test-begin "day11-parse-line")
(test-equal "(parse-line '1234') => '(1 2 3 4)" '(1 2 3 4)
	    (parse-line "1234"))
(test-equal
    "(parse-line '0123456789') => '(0 1 2 3 4 5 6 7 8 9)"
  '(0 1 2 3 4 5 6 7 8 9)
  (parse-line "0123456789"))
(test-error
 "(parse-line '1234a') => '(1 2 3 4) => &message"
 &message
 (parse-line "1234a"))
(test-equal
    "(parse-line '1234a') => '(1 2 3 4) => &message"
  #f
  (with-exception-handler
      (lambda (exn) #f)
    (lambda () (let ((result (parse-line "1234a"))) result))
    #:unwind? #t
    #:unwind-for-type &message))
(test-end)

(test-begin "day11-parse-line")
(test-equal "(parse-line '1234') => '(1 2 3 4)" '(1 2 3 4)
	    (parse-line "1234"))
(test-equal
    "(parse-line '0123456789') => '(0 1 2 3 4 5 6 7 8 9)"
  '(0 1 2 3 4 5 6 7 8 9)
  (parse-line "0123456789"))
(test-error
 "(parse-line '1234a') => '(1 2 3 4) => &message"
 &message
 (parse-line "1234a"))
(test-equal
    "(parse-line '1234a') => '(1 2 3 4) => &message"
  #f
  (with-exception-handler
      (lambda (exn) #f)
    (lambda () (let ((result (parse-line "1234a"))) result))
    #:unwind? #t
    #:unwind-for-type &message))
(test-end)

(test-begin "day11-flashed?")
(test-equal
    "(flashed? flashed '(0 0)) => #t"
  #t
  (let ((flashed (make-hashtable (lambda (x) (hash x 10000)) equal?)))
    (begin
      (hashtable-set! flashed '(0 0) #t))
      (flashed? flashed '(0 0))))
(test-equal
    "(flashed? flashed '(0 1)) => #f"
  #f
  (let ((flashed (make-hashtable (lambda (x) (hash x 10000)) equal?)))
    (begin
      (hashtable-set! flashed '(0 0) #t))
      (flashed? flashed '(0 1))))
(test-end)

(test-begin "day11-mark-flashed")
(test-equal
    "(flashed? flashed '(0 0)) => #t"
  #t
  (let ((flashed (make-hashtable (lambda (x) (hash x 10000)) equal?)))
    (begin
      (mark-flashed flashed '(0 0))
      (flashed? flashed '(0 0)))))
(test-equal
    "(flashed? flashed '(0 1)) => #f"
  #f
  (let ((flashed (make-hashtable (lambda (x) (hash x 10000)) equal?)))
    (begin
      (mark-flashed flashed '(0 0))
      (flashed? flashed '(0 1)))))
(test-end)

(test-begin "day11-reset-flashed")
(let ((arr (make-array 0 '(0 2) '(0 2)))
      (flashed (make-hashtable (lambda (x) (hash x 10000)) equal?)))
  (begin
    (mark-flashed flashed '(0 0))
    (array-set! arr 10 0 0)
    (mark-flashed flashed '(0 1))
    (array-set! arr 10 0 1)
    (test-equal
	"(flashed? flashed '(0 0)) => #t"
      #t
      (and
       (flashed? flashed '(0 0))
       (= (array-ref arr 0 0) 10)))
    (test-equal
	"(flashed? flashed '(0 1)) => #t"
      #t
      (and
       (flashed? flashed '(0 0))
       (= (array-ref arr 0 1) 10)))
    (reset-flashed arr flashed 2 2)
    (test-equal
	"(flashed? flashed '(0 0)) => #f"
      0
      (array-ref arr 0 0))
    (test-equal
	"(flashed? flashed '(0 1)) => #f"
      0
      (array-ref arr 0 1))))
(test-end)

(test-begin "day11-build-neighbors")
(test-equal
    "(build-neighbors #2((0)) 0 0) => #2((()))"
  #2((()))
  (build-neighbors #2((0)) 0 0))
(let ((arr (make-array 0 '(0 2) '(0 2))))
  (test-equal
      "(build-neighbors #2((0 0 0) (0 0 0) (0 0 0)) 2 2) => #2((((0 1) (1 0) (1 1)) ((0 0) (0 2) (1 0) (1 1) (1 2)) ((0 1) (1 1) (1 2))) (((0 0) (0 1) (1 1) (2 0) (2 1)) ((0 0) (0 1) (0 2) (1 0) (1 2) (2 0) (2 1) (2 2)) ((0 1) (0 2) (1 1) (2 1) (2 2))) (((1 0) (1 1) (2 1)) ((1 0) (1 1) (1 2) (2 0) (2 2)) ((1 1) (1 2) (2 1))))"
    #2((((0 1) (1 0) (1 1)) ((0 0) (0 2) (1 0) (1 1) (1 2)) ((0 1) (1 1) (1 2))) (((0 0) (0 1) (1 1) (2 0) (2 1)) ((0 0) (0 1) (0 2) (1 0) (1 2) (2 0) (2 1) (2 2)) ((0 1) (0 2) (1 1) (2 1) (2 2))) (((1 0) (1 1) (2 1)) ((1 0) (1 1) (1 2) (2 0) (2 2)) ((1 1) (1 2) (2 1))))
    (build-neighbors arr 2 2)))
(test-end)

(test-begin "day11-flash")
(let* ((arr (list->array 2 '((6 7) (9 10))))
       (arr2 (list->array 2 '((7 8) (10 10))))
       (neighbors (build-neighbors arr 1 1))
       (flashed (make-hashtable (lambda (x) (hash x 10000)) equal?)))
  (begin
    (test-equal
	"(flash #2((6 7) (9 10)) #() 1 1) => (1 ((1 0)))"
      '(1 ((1 0)))
      (flash arr neighbors flashed '(1 1)))
    (test-equal
	"(flash #2((6 7) (9 10)) #() 1 1) => flashed => #((1 1))"
      #((1 1))
      (hashtable-entries flashed))
    (test-equal
	"(flash #2((7 8) (10 10)) #((1 1)) 1 0) => (1 ((1 1)))"
      '(1 ((1 1)))
      (flash arr2 neighbors flashed '(1 0)))
    (test-equal
	"(flash #2((7 8) (10 10)) #((1 1)) 1 0) => flashed => #((1 1) (1 0))"
      #((1 1) (1 0))
      (hashtable-entries flashed))))
(test-end)

(test-begin "day11-step")
(let* ((arr #2((1 1 1 1 1) (1 9 9 9 1) (1 9 1 9 1) (1 9 9 9 1) (1 1 1 1 1)))
      (neighbors (build-neighbors arr 4 4)))
  (begin
    (test-equal
	#2((3 4 5 4 3) (4 0 0 0 4) (5 0 0 0 5) (4 0 0 0 4) (3 4 5 4 3))
	(begin
	  (step arr neighbors 4 4)
	  arr))
    (test-equal
	#2((4 5 6 5 4) (5 1 1 1 5) (6 1 1 1 6) (5 1 1 1 5) (4 5 6 5 4))
	(begin
	  (step arr neighbors 4 4)
	  arr))))
(test-end)
