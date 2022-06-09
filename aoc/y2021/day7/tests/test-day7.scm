;;
;; Test day7
;;
(define-module (aoc y2021 day7 tests day7)
  #:use-module (srfi srfi-64)
  #:use-module (aoc y2021 day7 day7))

;; Test parse-lines
(test-begin "day7-parse-lines")
(test-equal "() => ()" '() (parse-lines '()))
(test-equal "(\"1,2,3,4\") => ((1 2 3 4))" '((1 2 3 4)) (parse-lines '("1,2,3,4")))
(test-equal "(\"1,2\" \"3,4\") => ((1 2) (3 4))" '((1 2) (3 4)) (parse-lines '("1,2" "3,4")))
(test-end)

(test-begin "day7-get-range")
(test-equal "() => ()" '() (get-range '()))
(test-equal "(1 2 3) => (1 2 3)" '(1 2 3) (get-range '(1 2 3)))
(test-equal "(3 1 2) => (1 2 3)" '(1 2 3) (get-range '(3 1 2)))
(test-equal "(3 7 2) => (2 3 4 5 6 7)" '(2 3 4 5 6 7) (get-range '(3 7 2)))
(test-end)

(test-begin "day7-find-distance")
(test-equal "0 0 => 0" 0 (find-distance 0 0))
(test-equal "1 3 => 2" 2 (find-distance 1 3))
(test-equal "3 1 => 2" 2 (find-distance 3 1))
(test-end)

(test-begin "day7-find-distances")
(test-equal "() 0 => ()" '() (find-distances '() 0))
(test-equal "(1) 0 => (1)" '(1) (find-distances '(1) 0))
(test-equal "(1 8) 5 => (4 3)" '(4 3) (find-distances '(1 8) 5))
(test-end)

(test-begin "day7-cost-function")
(test-equal "1 -> () => 0" 0 ((cost-function 1) '()))
(test-equal "2 -> () => 0" 0 ((cost-function 2) '()))
(test-equal "1 -> (3 2 4) => 9" 9 ((cost-function 1) '(3 2 4)))
(test-equal "2 -> (3 2 4) => 19" 19 ((cost-function 2) '(3 2 4)))
(test-end)

(test-begin "day7-search-next-step")
(test-equal "() 2 200 (cost-function 1) => 0"
  0 (search-next-step '() 2 200 (cost-function 1)))
(test-equal "(1 3 4) 2 200 (cost-function 1) => 4"
  4 (search-next-step '(1 3 4) 2 200 (cost-function 1)))
(test-equal "(1 3 4) 2 200 (cost-function 2) => 5"
  5 (search-next-step '(1 3 4) 2 200 (cost-function 2)))
(test-equal "(1 5 4) 2 3 (cost-function 1) => 4"
  4 (search-next-step '(1 5 4) 2 3 (cost-function 1)))
(test-end)

(test-begin "day7-search-range")
(test-equal "() (1 2) (cost-function 1) => ()"
  '() (search-range '() '(1 2) (cost-function 1)))
(test-equal "(1 2) () (cost-function 1) => ()"
  '() (search-range '(1 2) '() (cost-function 1)))
(test-equal "(4 8) (3 7 9) (cost-function 1) => (7 4)"
  '(7 4) (search-range '(4 8) '(3 7 9) (cost-function 1)))
(test-end)
