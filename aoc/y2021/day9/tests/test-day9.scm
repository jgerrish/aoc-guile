;;
;; Test day9
;;
(define-module (aoc y2021 day9 tests day9)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 exceptions)
  #:use-module ((rnrs hashtables) :version (6))
  #:use-module ((rnrs records syntactic) :version (6))
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

(test-begin "day9-index-pairs")
(test-equal
    "(index-pairs () 0 #f) -> ()"
  '()
  (index-pairs '() 0 #f))
(test-equal
    "(index-pairs () 0 #t) -> ()"
  '()
  (index-pairs '() 0 #t))
(test-equal
    "(index-pairs (1) 0 #f) -> ((1 0))"
  '((1 0))
  (index-pairs '(1) 0 #f))
(test-equal
    "(index-pairs (1) 0 #t) -> ((0 1))"
  '((0 1))
  (index-pairs '(1) 0 #t))
(test-equal
    "(index-pairs (1 3) 0 #f) -> ((1 0) (3 0))"
  '((1 0) (3 0))
  (index-pairs '(1 3) 0 #f))
(test-equal
    "(index-pairs (1 3) 0 #t) -> ((0 1) (0 3))"
  '((0 1) (0 3))
  (index-pairs '(1 3) 0 #t))
(test-end)

(test-begin "day9-flatten-one-level")
(test-equal
    "(flatten-one-level (())) -> #f"
  '()
  (flatten-one-level '(())))
(test-equal
    "(flatten-one-level (())) -> (1 2 3)"
  '(1 2 3)
  (flatten-one-level '((1 2 3))))
(test-equal
    "(flatten-one-level (())) -> (1 2 3)"
  '(1 2 3)
  (flatten-one-level '((1) (2) (3))))
(test-end)

(test-begin "day9-find-neighbors")
(test-equal
    "(find-neighbors 0 0 5 9) -> ((1 0) (0 1)))"
  '((1 0) (0 1))
  (find-neighbors 0 0 5 9))
(test-equal
    "(find-neighbors 0 1 5 9) -> ((1 1) (0 0) (0 2)))"
  '((1 1) (0 0) (0 2))
  (find-neighbors 0 1 5 9))
(test-equal
    "(find-neighbors 0 2 5 9) -> ((1 2) (0 1) (0 3)))"
  '((1 2) (0 1) (0 3))
  (find-neighbors 0 2 5 9))
(test-equal
    "(find-neighbors 1 0 5 9) -> ((0 0) (2 0) (1 1)))"
  '((0 0) (2 0) (1 1))
  (find-neighbors 1 0 5 9))
(test-equal
    "(find-neighbors 1 1 5 9) -> ((0 1) (2 1) (1 0) (1 2)))"
  '((0 1) (2 1) (1 0) (1 2))
  (find-neighbors 1 1 5 9))
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
