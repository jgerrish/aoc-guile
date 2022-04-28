;;
;; Test day5
;;
(define-module (aoc y2021 day5 tests day5)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-69)
  #:use-module (aoc y2021 day5 day5))

;; Test parse-line
(test-begin "parse-line")
(begin
  (test-equal "\"\" -> ()" '() (parse-line ""))
  (test-equal "\"0,0 -> 0,0\" -> ((0 0) (0 0))" '((0 0) (0 0)) (parse-line "0,0 -> 0,0"))
  (test-equal "\"1,2 -> 3,4\" -> ((1 2) (3 4))" '((1 2) (3 4)) (parse-line "1,2 -> 3,4")))
(test-end)

;; Test parse-lines
(test-begin "parse-lines")
(begin
  (test-equal "(\"\") -> (())" '(()) (parse-lines '("")))
  (test-equal "(\"0,0 -> 0,0\") -> (((0 0) (0 0)))" '(((0 0) (0 0)))
	      (parse-lines '("0,0 -> 0,0")))
  (test-equal "(\"1,2 -> 3,4\" \"5,6 -> 7,8\") -> (((5 6) (7 8)))"
    '(((1 2) (3 4)) ((5 6) (7 8)))
    (parse-lines '("1,2 -> 3,4" "5,6 -> 7,8"))))
(test-end)

;; Test filter-horizontal-vertical-lines
(test-begin "filter-horizontal-vertical-lines")
(begin
  (test-equal "(((0 0) (10 0))) -> (((0 0) (10 0)))"
    '(((0 0) (10 0)))
    (filter-horizontal-vertical-lines '(((0 0) (10 0)))))
  (test-equal "(((0 0) (10 0)) ((0 1) (1 0))) -> (((0 0) (10 0)))"
    '(((0 0) (10 0)))
    (filter-horizontal-vertical-lines '(((0 0) (10 0)) ((0 1) (1 0))))))
(test-end)

;; Test extend
(test-begin "extend")
(begin
  (test-equal "(1) 5 -> (1 1 1 1 1)" '(1 1 1 1 1) (extend '(1) 5))
  (test-equal "(1) 1 -> (1)" '(1) (extend '(1) 1))
  (test-equal "(1) 2 -> (1 1)" '(1 1) (extend '(1) 2)))
(test-end)

;; Test points-covered, only horizontal or vertical lines
(test-begin "points-covered")
(begin
  (test-equal "((1 1) (1 3) -> ((1 1) (1 2) (1 3))"
    '((1 1) (1 2) (1 3))
    (points-covered '((1 1) (1 3))))
  (test-equal "((1 1) (1 1) -> ((1 1))"
    '((1 1))
    (points-covered '((1 1) (1 1))))
  (test-equal "((1 1) (3 1) -> ((1 1) (2 1) (3 1))"
    '((1 1) (2 1) (3 1))
    (points-covered '((1 1) (3 1)))))
(test-end)

;; Test flatten-one-level
(test-begin "flatten-one-level")
(begin
  (test-equal "() -> ()" '() (flatten-one-level '()))
  (test-equal "(()) -> ()" '() (flatten-one-level '(())))
  (test-equal "((1)) -> (1)" '(1) (flatten-one-level '((1))))
  (test-equal "((1) (2)) -> (1 2)" '(1 2) (flatten-one-level '((1) (2)))))
  (test-equal "(((1) (2))) -> ((1) (2))" '((1) (2)) (flatten-one-level '(((1) (2)))))
(test-end)

;; Test build-overlap-map
(test-begin "build-overlap-map")
(begin
  (test-equal "() -> empty hash"
    0
    (length (hash-table-values (build-overlap-map '()))))
  (let ((overlap-map (build-overlap-map '((0 0)))))
    (test-equal "((0 0)) -> one item, one count"
      '(1 1)
      (list
       (length (hash-table-values overlap-map))
       (hash-table-ref overlap-map '(0 0)))))
  (test-equal "((0 0) (0 0)) -> one item, two counts"
    '(1 2)
    (let ((overlap-map (build-overlap-map '((0 0) (0 0)))))
      (list
       (length (hash-table-values overlap-map))
       (hash-table-ref overlap-map '(0 0)))))
  (test-equal "((0 0) (1 1)) -> two items, one count"
    '(2 1 1)
    (let ((overlap-map (build-overlap-map '((0 0) (1 1)))))
      (list
       (length (hash-table-values overlap-map))
       (hash-table-ref overlap-map '(0 0))
       (hash-table-ref overlap-map '(1 1))))))
(test-end)

;; Test count-overlaps
(test-begin "count-overlaps")
(begin
  (test-equal "() -> 0"
    0
    (count-overlaps (build-overlap-map '()) 1))
  (test-equal "((0 0)) -> 0"
      0
      (let ((overlap-map (build-overlap-map '((0 0)))))
	(count-overlaps overlap-map 1)))
  (test-equal "((0 0) (0 0)) -> 1"
    1
    (let ((overlap-map (build-overlap-map '((0 0) (0 0)))))
      (count-overlaps overlap-map 1)))
  (test-equal "((0 0) (1 1)) -> 0"
    0
    (let ((overlap-map (build-overlap-map '((0 0) (1 1)))))
      (count-overlaps overlap-map 1))))
(test-end)
