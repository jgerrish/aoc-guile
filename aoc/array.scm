;; Module to work with arrays
;;
;; Provides common features like finding neighbors.
;; These functions are not optimized.
;;
;; For those interested, a fun project might be to take some of these
;; functions and optimize them using generators (Scheme continuations)
;; or transducers (SRFI-171).
;;
;; There are extensive tests for these functions, which should help
;; with refactoring.
(define-module (aoc array)
  #:use-module (srfi srfi-1)
  #:export (array-indexes index-pairs flatten-one-level
			  find-neighbors find-neighbors-with-diag))

;; array-indexes, from the Guile Scheme array-map.c source code.
(define (array-indexes array)
  (let ((ra (apply make-array #f (array-shape array))))
    (array-index-map! ra (lambda x x))
    ra))

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
  (if (or (< i1 0) (> i1 i1-max))
      (out-of-range #f i1)
      (if (or (< i2 0) (> i2 i2-max))
	  (out-of-range #f i2)
	  (let ((i1-neighbors
		 (cond ((= i1 0) (if (> i1-max 0) '(1) '()))
		       ((= i1 i1-max) (list (1- i1)))
		       (else (list (1- i1) (1+ i1)))))
		(i2-neighbors
		 (cond ((= i2 0) (if (> i2-max 0) '(1) '()))
		       ((= i2 i2-max) (list (1- i2)))
		       (else (list (1- i2) (1+ i2))))))
	    (flatten-one-level (list
       				(index-pairs i1-neighbors i2 #f)
       				(index-pairs i2-neighbors i1 #t)))))))



;; cross-product of two lists.
;; Given two lists: '(0 1 2) and '(a b) returns '((0 a) (0 b) (1 a) (1 b) (2 a) (2 b))
;; This could be refactored with an exclusion filter parameter and
;; using transducers.
(define (cross-product list-a list-b)
  (let ((res (map
	      (lambda (a)
		(map
		 (lambda (b)
		   (list a b))
		 list-a))
	      list-b)))
    (apply append res)))


;; Return a list of neighbors of this index including diagonals
;;
;; Neighbors are the cells directly up, down, left and right
;; and also the cells to the upper-left, upper-right, lower-left and lower-right.
;; It does no no wrapping.
;;
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

(define (find-neighbors-with-diag i1 i2 i1-max i2-max)
  ;; Throw an out-of-range for the first out-of-range found
  (if (or (< i1 0) (> i1 i1-max))
      (out-of-range #f i1)
      (if (or (< i2 0) (> i2 i2-max))
	  (out-of-range #f i2)
	  (let* ((i1-start (if (= i1 0) 0 (1- i1)))
		 (i1-end (if (= i1 i1-max) i1-max (1+ i1)))
		 (i2-start (if (= i2 0) 0 (1- i2)))
		 (i2-end (if (= i2 i2-max) i2-max (1+ i2)))
		 (i1-items (iota (1+ (- i1-end i1-start)) i1-start))
		 (i2-items (iota (1+ (- i2-end i2-start)) i2-start)))
	    (begin
	      ;; (display (format #f "i1-start: ~a, i1-end: ~a\n" i1-start i1-end))
	      ;; (display
	      ;;  (format #f "~a ~a\n" i1-items i2-items))
	      ;; More inefficiencies
	      (filter
	       (lambda (x) (not (equal? x (list i1 i2))))
	       (cross-product i2-items i1-items)))))))
