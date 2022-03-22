;;
;; Test day2
;;
(define-module (aoc y2021 day2 tests day2)
  #:use-module (srfi srfi-64)
  #:use-module (aoc y2021 day2 day2))

;; Test parse-commands
(test-begin "parse-commands")
(begin
  (test-equal "forward 3\ndown 2\nup 1" '(("forward" 3) ("down" 2) ("up" 1))
	      (parse-commands '("forward 3" "down 2" "up 1"))))
(test-end)

(test-begin "run-commands")
(begin
  (test-equal "((forward 3) (down 2) (up 1))"
    '(3 1)
    (run-commands '(("forward" 3) ("down" 2) ("up" 1)))))
(test-end)

(test-begin "final-value")
(begin
  (test-equal "(3 1)" 3 (final-value '(3 1))))
(test-end)
