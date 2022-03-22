;;
;; Test day1
;;
(define-module (aoc y2021 day1 tests day1)
  #:use-module (srfi srfi-64)
  #:use-module (aoc y2021 day1 day1))

;; Test find-increasing
(test-begin "find-increasing")
(begin
  (test-equal "(0)" '((0 #f)) (find-increasing '(0)))
  (test-equal "(0 1)" '((1 #t) (0 #f)) (find-increasing '(0 1)))
  (test-equal "(1 0)" '((0 #f) (1 #f)) (find-increasing '(1 0)))
  (test-equal "(0 1 2 3)" '((3 #t) (2 #t) (1 #t) (0 #f)) (find-increasing '(0 1 2 3))))
(test-end)

;; Test find-increasing
(test-begin "count-increasing")
(begin
  (test-equal "(0)" 0 (count-increasing '((0 #f))))
  (test-equal "(0 1)" 1 (count-increasing '((1 #t) (0 #f))))
  (test-equal "(1 0)" 0 (count-increasing '((0 #f) (1 #f))))
  (test-equal "(0 1 2 3)" 3 (count-increasing '((3 #t) (2 #t) (1 #t) (0 #f)))))
(test-end)

;; test sliding-window
(test-begin "sliding-window")
(begin
  (test-equal "(0 1 2) 1" '((0) (1) (2)) (sliding-window '(0 1 2) 1))
  (test-equal "(0 1 2) 3" '((0 1 2)) (sliding-window '(0 1 2) 3))
  (test-equal "(0 1 2 4) 3" '((0 1 2) (1 2 3)) (sliding-window '(0 1 2 3) 3)))
(test-end)

;; test reduce-sliding-window
(test-begin "reduce-sliding-window")
(begin
  (test-equal "((0) (1) (2)) (0 1 2)" '(0 1 2) (reduce-sliding-window '((0) (1) (2))))
  (test-equal "((0 1 2)) (3)" '(3) (reduce-sliding-window '((0 1 2))))
  (test-equal "((0 1 2) (1 2 3)) (3 6)" '(3 6) (reduce-sliding-window '((0 1 2) (1 2 3)))))
(test-end)
