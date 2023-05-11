;; Module to work with arrays
;;
;; Provides common features like finding neighbors
(define-module (aoc array)
  #:use-module (srfi srfi-1)
  #:export (index-pairs flatten-one-level find-neighbors))

;; Generate list of indexes from short neighbor list format
(define (index-pairs lst index first-index?)
  (map
   (lambda (x)
     (if first-index?
	 (list index x)
	 (list x index)))
   lst))

;; flatten list by one level
(define (flatten-one-level lst)
  (apply append lst))

;; out-of-range error helper
;; Copied from the Guile SRFI-1 implementation (srfi-1.scm)
(define (out-of-range proc arg)
  (scm-error 'out-of-range proc
             "Value out of range: ~A" (list arg) (list arg)))

;; Return a list of neighbors of this index
;; Neighbors are up, down, left and right with no wrapping
;; size is the largest index size
;; This function generates pairs like:
;; (find-neighbors 0 0 4 4) -> ((1) (1))
;; (find-neighbors 1 1 4 4) -> ((0 2) (0 2))
;; Where the function result is two lists, the neighbor indexes in the
;; first dimension and the neighbor indexes in the second dimension.
;; It then transforms them into full index pairs.
;;
;; coordinates take this form: (row column)
;;
;; Parameters:
;;   i1 is the first coordinate term
;;   i2 is the second coordinate term
;;   i1-max is the size (width - 1)
;;   i2-max is the size (height - 1)
;;
;; i1-max and i2-max are defined in terms of the maximum value of i1 and i2
;; so they range from zero (1x1 array) to whatever
;;
;; A zero-by-zero array has no entries.
;; A one-by-one array has one entry.
;; A two-by-two array has four entries, etc.
;;
(define (find-neighbors i1 i2 i1-max i2-max)
  ;; Throw an out-of-range for the first out-of-range found
  (if (> i1 i1-max)
      (out-of-range #f i1)
      (if (> i2 i2-max)
	  (out-of-range #f i2)
	  (let ((i1-neighbors
		 (cond ((= i1 0) (if (> i1-max 0) '(1) '()))
		       ((= i1 i1-max) (list (1- i1)))
		       (else (list (1- i1) (1+ i1)))))
		(i2-neighbors
		 (cond ((= i2 0) (if (> i2-max 0) '(1) '()))
		       ((= i2 i2-max) (list (1- i2)))
		       (else (list (1- i2) (1+ i2))))))
	    ;; TODO Fix this, not efficient, should handle most logic on the leaves
	    ;; using more visitor / walker patterns
	    (flatten-one-level (list
       				(index-pairs i1-neighbors i2 #f)
       				(index-pairs i2-neighbors i1 #t)))))))
