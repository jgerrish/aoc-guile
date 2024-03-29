;;
;; Test day9
;;
(define-module (aoc y2021 day9 tests day9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 exceptions)
  #:use-module ((rnrs hashtables) :version (6))
  #:use-module ((rnrs records syntactic) :version (6))
  #:use-module ((rnrs sorting) :version (6))
  #:use-module (aoc y2021 day9 day9))

;; Test parse-line
(test-begin "day9-parse-line")
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

(test-begin "day9-parse-lines")
(test-equal "(parse-line () => #2()"
  #2()
  (parse-lines '()))
(test-equal "(parse-line ('12' '34')) => #2(1 2) (3 4))"
  #2((1 2) (3 4))
  (parse-lines '("12" "34")))
(test-end)

(test-begin "day9-find-min-spots")
(test-equal
    "(find-min-spots #2((1 1) (1 1))) -> (0 0)"
  '()
  (find-min-spots #2((1 1) (1 1))))
(test-equal
    "(find-min-spots #2((3 2) (4 4))) -> (0 0)"
  '((0 1))
  (find-min-spots #2((3 2) (4 4))))
(test-equal
    "(find-min-spots #2((3 2) (1 4))) -> (0 0)"
  '((1 0) (0 1))
  (find-min-spots #2((3 2) (1 4))))
(test-end)

(test-begin "day9-risk-levels")
(test-equal
    "(find-min-spots #2((1 1) (1 1))) -> 0"
  0
  (risk-levels #2((1 1) (1 1)) (find-min-spots #2((1 1) (1 1)))))
(test-equal
    "(find-min-spots #2((3 2) (4 4))) -> 3"
  3
  (risk-levels #2((3 2) (4 4)) (find-min-spots #2((3 2) (4 4)))))
(test-equal
    "(find-min-spots #2((3 2) (1 4))) -> 5"
  5
  (risk-levels #2((3 2) (1 4)) (find-min-spots #2((3 2) (1 4)))))
(test-end)

(test-begin "day9-higher-or-equal-neighbors")
;;
;; 129
;; 293
;; 294
;;
(let* ((cave-data '("129" "293" "294"))
       (cave-array (parse-lines cave-data)))
  (test-equal
      "(higher-or-equal-neighbors cave-array 1 '((1 0) (0 1)) -> '((0 1) (1 0))"
    '((0 1) (1 0))
    (list-sort
     (lambda (a b) (or (< (first a) (first b)) (< (second a) (second b))))
     (higher-or-equal-neighbors cave-array 1 '((1 0) (0 1)))))
  (test-equal
      "(higher-or-equal-neighbors cave-array 2 '((0 0) (1 1) (0 2)) -> '()"
    '()
    (list-sort
     (lambda (a b) (or (< (first a) (first b)) (< (second a) (second b))))
     (higher-or-equal-neighbors cave-array 2 '((0 0) (1 1) (0 2))))))
(test-end)

(test-begin "day9-find-basin")
;;
;; 129
;; 293
;; 294
;;
(let* ((cave-data '("129" "293" "294"))
       (cave-array (parse-lines cave-data)))
  (test-equal
      "(find-basin cave-array '(0 0)) -> #((0 0) (0 1) (1 0) (2 0))"
    #((0 0) (0 1) (1 0) (2 0))
    (vector-sort
     (lambda (a b) (or (< (first a) (first b)) (< (second a) (second b))))
     (find-basin cave-array '(0 0))))
  (test-equal
      "(find-basin cave-array '(1 2)) -> #((1 2) (2 2))"
    #((1 2) (2 2))
    (vector-sort
     (lambda (a b) (or (< (first a) (first b)) (< (second a) (second b))))
     (find-basin cave-array '(1 2)))))
(test-end)
