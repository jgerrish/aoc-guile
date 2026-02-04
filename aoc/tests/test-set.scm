;;
;; Test set module and class
;;
(define-module (aoc tests set)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (oop goops)
  #:use-module (ice-9 exceptions)
  #:use-module (aoc set))

(test-begin "aoc set initialize")
(let ((set (make <set>)))
  (test-equal
      "set initializes to empty set"
    '()
    (set->list set)))
(test-end)

(test-begin "aoc set with init-data")
(let ((set (make <set> #:init-data '((a 1) (b 2)))))
  (test-equal
      "set with init-data initializes correctly"
    #t
    (and (contains? set '(a 1))
	 (contains? set '(b 2))
	 (= (size set) 2))))
(let ((set (make <set> #:init-data '((a 1) (b 2) (b 2)))))
  (test-equal
      "set with init-data initializes correctly"
    #t
    (and (contains? set '(a 1))
	 (contains? set '(b 2))
	 (= (size set) 2))))
(test-end)

(test-begin "aoc set add! works")
(let ((set (make <set> #:init-data '((a 1) (b 2)))))
  (test-equal
      "set with init-data initializes correctly"
    #t
    (and (contains? set '(a 1))
	 (contains? set '(b 2))
	 (= (size set) 2))))
(let* ((set (make <set> #:init-data '((a 1) (b 2))))
       (set (add! set '(c 3))))
  (test-equal
      "add! new item works"
    #t
    (and (contains? set '(a 1))
	 (contains? set '(b 2))
	 (contains? set '(c 3))
	 (= (size set) 3))))
(let* ((set (make <set> #:init-data '((a 1) (b 2))))
       (set (add! set '(a 1))))
  (test-equal
      "add! existing item works"
    #t
    (and (contains? set '(a 1))
	 (contains? set '(b 2))
	 (= (size set) 2))))
(test-end)

(test-begin "aoc set remove! works")
(let ((set (make <set> #:init-data '((a 1) (b 2)))))
  (test-equal
      "set with init-data initializes correctly"
    #t
    (and (contains? set '(a 1))
	 (contains? set '(b 2))
	 (= (size set) 2))))
(let* ((set (make <set> #:init-data '((a 1) (b 2))))
       (set (remove! set '(a 1))))
  (test-equal
      "remove! existing item works"
    #t
    (and (contains? set '(b 2))
	 (= (size set) 1))))
(let* ((set (make <set> #:init-data '((a 1) (b 2))))
       (set (remove! set '(c 3))))
  (test-equal
      "remove! non-existent item works"
    #t
    (and (contains? set '(a 1))
	 (contains? set '(b 2))
	 (= (size set) 2))))
(test-end)
