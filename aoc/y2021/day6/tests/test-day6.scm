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

;; Test run-step
(test-begin "day6-run-steps")
(test-equal "(run-steps '() 0) => '()" '()
	    (run-steps '() 0))
(test-equal "(run-steps '() 1) => '()" '()
	    (run-steps '() 1))
(test-equal "(run-steps '() 2) => '()" '()
	    (run-steps '() 2))
(test-equal "(run-steps '(1) 0) => '(1)" '(1)
	    (run-steps '(1) 0))
(test-equal "(run-steps '(3 4 3 1 2) 1) => '(2 3 2 0 1)" '(2 3 2 0 1)
	    (run-steps '(3 4 3 1 2) 1))
(test-equal "(run-steps '(3 4 3 1 2) 2) => '(1 2 1 6 0 8)" '(1 2 1 6 0 8)
	    (run-steps '(3 4 3 1 2) 2))
(test-equal "(run-steps '(3 4 3 1 2) 3) => '(0 1 0 5 6 7 8)" '(0 1 0 5 6 7 8)
	    (run-steps '(3 4 3 1 2) 3))
(test-end)
