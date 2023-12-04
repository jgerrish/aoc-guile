;;
;; Test day6
;;
(define-module (aoc y2021 day6 tests day6)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (aoc y2021 day6 day6))

;; Test parse-line
(test-begin "day6-parse-line")
(test-equal "(parse-line '') => '()" '()
	    (parse-line ""))
(test-equal "(parse-line '1,2,3,4') => '(1 2 3 4)" '(1 2 3 4)
	    (parse-line "1,2,3,4"))
(test-end)

;; Test converting a lanternfish list to a lanternfish count vector
(test-begin "day6-fish-list->fish-count-vector")
(test-equal
    "((fish-list->fish-count-vector '()) -> #(0 0 0 0 0 0 0 0 0)"
  #(0 0 0 0 0 0 0 0 0)
  (fish-list->fish-count-vector '()))
(test-equal
    "((fish-list->fish-count-vector '(0)) -> #(1 0 0 0 0 0 0 0 0)"
  #(1 0 0 0 0 0 0 0 0)
  (fish-list->fish-count-vector '(0)))
(test-equal
    "((fish-list->fish-count-vector '(1)) -> #(0 1 0 0 0 0 0 0 0)"
  #(0 1 0 0 0 0 0 0 0)
  (fish-list->fish-count-vector '(1)))
(test-equal
    "((fish-list->fish-count-vector '(1 1)) -> #(0 2 0 0 0 0 0 0 0)"
  #(0 2 0 0 0 0 0 0 0)
  (fish-list->fish-count-vector '(1 1)))
(test-equal
    "((fish-list->fish-count-vector '(1 2 1)) -> #(0 2 1 0 0 0 0 0 0)"
  #(0 2 1 0 0 0 0 0 0)
  (fish-list->fish-count-vector '(1 2 1)))
(test-end "day6-fish-list->fish-count-vector")

;; Test step
(test-begin "day6-step")
(test-equal "(step '()) => '()" '()
	    (step '()))
(test-equal "(step '(0)) => '(6 8)" '(6 8)
	    (step '(0)))
(test-equal "(step '(1)) => '(0)" '(0)
	    (step '(1)))
(test-equal "(step '(3 4 3 1 2)) => '(2 3 2 0 1)" '(2 3 2 0 1)
	    (step '(3 4 3 1 2)))
(test-equal "(step '(2 3 2 0 1)) => '(1 2 1 6 0 8)" '(1 2 1 6 0 8)
	    (step '(2 3 2 0 1)))
(test-equal "(step '(1 2 1 6 0 8)) => '(0 1 0 5 6 7 8)" '(0 1 0 5 6 7 8)
	    (step '(1 2 1 6 0 8)))
(test-end)

;; Test run-steps for lists
(test-begin "day6-run-steps")
(test-equal "(run-steps '() 0) => '()" '()
	    (run-steps '() 0 step))
(test-equal "(run-steps '() 1) => '()" '()
	    (run-steps '() 1 step))
(test-equal "(run-steps '() 2) => '()" '()
	    (run-steps '() 2 step))
(test-equal "(run-steps '(1) 0) => '(1)" '(1)
	    (run-steps '(1) 0 step))
(test-equal "(run-steps '(3 4 3 1 2) 1) => '(2 3 2 0 1)" '(2 3 2 0 1)
	    (run-steps '(3 4 3 1 2) 1 step))
(test-equal "(run-steps '(3 4 3 1 2) 2) => '(1 2 1 6 0 8)" '(1 2 1 6 0 8)
	    (run-steps '(3 4 3 1 2) 2 step))
(test-equal "(run-steps '(3 4 3 1 2) 3) => '(0 1 0 5 6 7 8)" '(0 1 0 5 6 7 8)
	    (run-steps '(3 4 3 1 2) 3 step))
(test-end)

;; Test run-steps for count vectors
(test-begin "day6-run-steps vector")
(test-equal "(run-steps (make-vector 9 0) 0) => #(0 0 0 0 0 0 0 0 0)" #(0 0 0 0 0 0 0 0 0)
	    (run-steps (make-vector 9 0) 0 step-vector))
(test-equal "(run-steps (make-vector 9 0) 1) => #(0 0 0 0 0 0 0 0 0)" #(0 0 0 0 0 0 0 0 0)
	    (run-steps (make-vector 9 0) 1 step-vector))
(test-equal "(run-steps (make-vector 9 0) 2) => #(0 0 0 0 0 0 0 0 0)" #(0 0 0 0 0 0 0 0 0)
	    (run-steps (make-vector 9 0) 2 step-vector))
(test-equal "(run-steps #(0 1 0 0 0 0 0 0 0) 0 => #(0 0 0 0 0 0 0 0 0)" #(0 1 0 0 0 0 0 0 0)
	    (run-steps #(0 1 0 0 0 0 0 0 0) 0 step-vector))
(test-equal "(run-steps #(0 1 1 2 1 0 0 0 0) 1) => #(1 1 2 1 0 0 0 0 0)" #(1 1 2 1 0 0 0 0 0)
	    (run-steps #(0 1 1 2 1 0 0 0 0) 1 step-vector))
(test-equal "(run-steps #(0 1 1 2 1 0 0 0 0) 2) => #(1 2 1 0 0 0 1 0 1)" #(1 2 1 0 0 0 1 0 1)
	    (run-steps #(0 1 1 2 1 0 0 0 0) 2 step-vector))
(test-equal "(run-steps #(0 1 1 2 1 0 0 0 0) 3) => #(2 1 0 0 0 1 1 1 1)" #(2 1 0 0 0 1 1 1 1)
	    (run-steps #(0 1 1 2 1 0 0 0 0) 3 step-vector))
(test-end)
