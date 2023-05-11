;;
;; Test array module
;;
(define-module (aoc tests array)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 exceptions)
  #:use-module (aoc array))

(test-begin "array-index-pairs")
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

(test-begin "array-flatten-one-level")
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

(test-begin "array-find-neighbors")

;; Test with array of size 0x0
;; (test-equal
;;     "(find-neighbors 0 0 0 0) -> ())"
;;   '()
;;   (find-neighbors 0 0 0 0))

;; This uses the simple SRFI-64 test-error function
(test-error
 "(find-neighbors 1 1 0 0) -> 'out-of-range"
 (find-neighbors 1 1 0 0))

;; find-neighbors with an out-of-range index SHOULD throw an
;; out-of-range error
;; This uses a custom test-error test function
;; It is based on the "guile condition conversions" with-test-prefix test
;; in the Guile r6rs-exceptions.test file.
(test-equal
    "(find-neighbors 1 1 0 0) -> 'out-of-range"
  #t
  (catch #t
    (lambda ()
      (find-neighbors 1 1 0 0))
    (lambda (key . args)
      (eq? key 'out-of-range))))

;; find-neighbors with an out-of-range index SHOULDN'T throw an
;; invalid-index error
;; This uses a custom test-error test function
;; It is based on the "guile condition conversions" with-test-prefix test
;; in the Guile r6rs-exceptions.test file.  There are some other
;; similar tests in the Guile project
(test-equal
    "(find-neighbors 1 1 0 0) -> 'out-of-range"
  #f
  (catch #t
    (lambda ()
      (find-neighbors 1 1 0 0))
    (lambda (key . args)
      (eq? key 'invalid-index))))

;; Test with array of size 1x1
;; This should find no neighbors
(test-equal
    "(find-neighbors 0 0 0 0) -> ()"
  '()
  (find-neighbors 0 0 0 0))

;; Test with array of size 2x2
(test-equal
    "(find-neighbors 0 0 1 1) -> ((1 0) (0 1))"
  '((1 0) (0 1))
  (find-neighbors 0 0 1 1))

(test-equal
    "(find-neighbors 0 0 5 9) -> ((1 0) (0 1))"
  '((1 0) (0 1))
  (find-neighbors 0 0 5 9))
(test-equal
    "(find-neighbors 0 1 5 9) -> ((1 1) (0 0) (0 2))"
  '((1 1) (0 0) (0 2))
  (find-neighbors 0 1 5 9))
(test-equal
    "(find-neighbors 0 2 5 9) -> ((1 2) (0 1) (0 3))"
  '((1 2) (0 1) (0 3))
  (find-neighbors 0 2 5 9))
(test-equal
    "(find-neighbors 1 0 5 9) -> ((0 0) (2 0) (1 1))"
  '((0 0) (2 0) (1 1))
  (find-neighbors 1 0 5 9))
(test-equal
    "(find-neighbors 1 1 5 9) -> ((0 1) (2 1) (1 0) (1 2))"
  '((0 1) (2 1) (1 0) (1 2))
  (find-neighbors 1 1 5 9))
(test-end)
