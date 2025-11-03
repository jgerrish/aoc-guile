;; This module contains a set of experiments with transducers and how
;; to compose multiple transducers that have state where the state
;; needs to be retrieved after the transduction is complete.
;;
;; The experiments are based around two different functions that are
;; required for Advent of Code 2021, day 14.  That day requires
;; finding a most and least common element in a result.
;;
;; There are four different transducer types defined in this module,
;; and some helper transducers to setup the structure.
;;
;; The first one simply cons the state to the stream, requiring
;; downstream transducers to unpack that structure or lift their data
;; out.  This is a functional approach with no side-effects.
;;
;; The second uses a box to retrieve state after a transducer has run.
;; This is probably the easiest model but has side-effects.
;;
;; The third uses shared persistent variables.  It also has side-effects.
;;
;; The fourth model creates a custom transducer storage system that
;; each transducer can read and write from.  Each transducer class has
;; a globally unique ID that is used for setting and getting data
;; during and after processing.  This has no side-effects but is
;; complicated.  It probably has the most interesting applications to
;; distributed object storage and distributed services technologies in
;; operating systems and the web.
(define-module (aoc y2021 day14 transducers)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-111)
  #:use-module (srfi srfi-171)
  #:use-module (srfi srfi-171 gnu)
  #:use-module (srfi srfi-171 meta)
  #:use-module ((rnrs hashtables) :version (6))
  #:export (t-max-pair t-min-pair t-max-box t-min-box
		       get-cur-max t-max-shared get-cur-min t-min-shared
		       t-setup-transducer-storage t-max-transducer-storage t-min-transducer-storage))

;; These transducers cons their state to the input stream
;;
;; Any transducer downstream in the composition needs
;; to understand how to unpack them.
;;
;; Example use (assuming t-min-pair is also defined):
;; (list-transduce (compose (t-max-pair) (t-min-pair) (tfilter pair?)) rcons '(3 8 2 5))
(define (t-max-pair)
  (lambda (reducer)
    (let ((cur-max 0))
      (case-lambda
        (() (reducer))
        ((result) (cons (cons 'max cur-max) (reducer result)))
        ((result input)
	 (begin
           (if (> input cur-max)
	       (set! cur-max input))
           (reducer result input)))))))


;; These transducers cons their state to the input stream
;;
;; Any transducer downstream in the composition needs
;; to understand how to unpack them.
;;
;; Example use (assuming t-max-pair is also defined):
;; (list-transduce (compose (t-max-pair) (t-min-pair) (tfilter pair?)) rcons '(3 8 2 5))
(define (t-min-pair)
  (lambda (reducer)
    (let ((cur-min (inf)))
      (set! get-cur-min (lambda () cur-min))
      (case-lambda
        (() (reducer))
        ((result) (cons (cons 'min cur-min) (reducer result)))
        ((result input)
	 (begin
           (if (< input cur-min)
	       (set! cur-min input))
           (reducer result input)))))))

;; A transducer using boxes to pass around state
;;
;; cur-max should be a box
;;
;; It's basically a visitor pattern that saves state for transducers
;;
;; Pros: Simple, efficient
;; Cons: has side-effects, not purely functional
;;
;; I suppose it's one more for another zoo book
;;
;; Example use (assuming t-min-box is also defined):
;;
;; (define my-max-box (box 0))
;; (define my-min-box (box (inf)))
;; (list-transduce (compose (t-max-box my-max-box) (t-min-box my-min-box)) rcons '(3 8 2 5))
;; (unbox my-max-box)
(define* (t-max-box cur-max #:optional (ignore-zero? #f))
  (lambda (reducer)
    (case-lambda
      (()
       ;; This isn't getting called
       (begin
	 (display (format #f "init case for t-max-box\n"))
	 (set-box! cur-max 0)
	 (reducer)))
      ((result) (reducer result))
      ((result input)
       (begin
         (if (and (> input (unbox cur-max))
		  (or (not ignore-zero?) (not (= input 0))))
	     (set-box! cur-max input))
         (reducer result input))))))

;; A transducer using boxes to pass around state
;;
;; cur-min should be a box
;;
;; It's basically a visitor pattern that saves state for transducers
;;
;; Pros: Simple, efficient
;; Cons: has side-effects, not purely functional
;;
;; I suppose it's one more for another zoo book
;;
;; Example use (assuming t-max-box is also defined):
;;
;; (define my-max-box (box 0))
;; (define my-min-box (box (inf)))
;; (list-transduce (compose (t-max-box my-max-box) (t-min-box my-min-box)) rcons '(3 8 2 5))
;; (unbox my-min-box)
(define* (t-min-box cur-min #:optional (ignore-zero? #f))
  (lambda (reducer)
    (case-lambda
      (()
       ;; This isn't getting called
       (begin
	 (display (format #f "init case for t-min-box\n"))
	 (set-box! cur-min (inf))
	 (reducer)))
      ((result) (reducer result))
      ((result input)
       (begin
         (if (and (< input (unbox cur-min))
		  (or (not ignore-zero?) (not (= input 0))))
	     (set-box! cur-min input))
         (reducer result input))))))


;; In this version, cur-max doesn't need to be a box
;;
;; We do it using shared persistent variables, as in the Guile info
;; page section 3.4.7
;;
;; It's basically a visitor pattern that saves state for transducers
;;
;; Pros: Simple, efficient
;; Cons: has side-effects, not purely functional, different instances of the
;; transducer share the same get-cur-max.
;;
;; I suppose it's one more for another zoo book
;;
;; Example use (assuming t-min-shared is also defined):
;; (list-transduce (compose (t-max-shared) (t-min-shared)) rcons '(3 8 2 5))
;; (get-cur-max)
(define get-cur-max #f)
(define (t-max-shared)
  (lambda (reducer)
    (let ((cur-max 0))
      (set! get-cur-max (lambda () cur-max))
      (case-lambda
	(()
	 ;; This isn't getting called
	 (begin
	   (set! cur-max 0)
	   (reducer)))
	((result) (reducer result))
	((result input)
	 (begin
           (if (> input cur-max)
	       (set! cur-max input))
           (reducer result input)))))))


;; In this version, cur-min doesn't need to be a box
;;
;; We do it using shared persistent variables, as in the Guile info
;; page section 3.4.7
;;
;; It's basically a visitor pattern that saves state for transducers
;;
;; Pros: Simple, efficient
;; Cons: has side-effects, not purely functional, different instances of the
;; transducer share the same get-cur-min.
;;
;; I suppose it's one more for another zoo book
;;
;; Example use (assuming t-max-shared is also defined):
;; (list-transduce (compose (t-max-shared) (t-min-shared)) rcons '(3 8 2 5))
;; (get-cur-min)
(define get-cur-min #f)
(define (t-min-shared)
  (lambda (reducer)
    (let ((cur-min (inf)))
      (set! get-cur-min (lambda () cur-min))
      (case-lambda
	(()
	 ;; This isn't getting called
	 (begin
	   (display (format #f "init case for t-pair-min\n"))
	   (set! cur-min (inf))
	   (reducer)))
	((result) (reducer result))
	((result input)
	 (begin
           (if (< input cur-min)
	       (set! cur-min input))
           (reducer result input)))))))


;; A transducer that creates a shared storage cell
;;
;; It creates a pair from every element in the input stream, with the
;; first element being the original item, and the second element being
;; a hash table.
;;
;; Downstream transducers can then add and remove items from this
;; storage cell.  Each transducer that uses this storage should have a
;; globally unique ID.
;;
;; Unlike the previous versions, this version doesn't have any
;; side-effects.
;;
;; Pros: no side effects, purely functional
;; Cons: complicated, hash tables are low-cost but not zero-cost,
;;       Requires management of some GUIDs or UUIDs.
;;       hash-tables are generated per-element, instead of possibly one per
;;       list-transducer run.  This lets us look at intermediate
;;       values, but can also be expensive.  UUIDs are currently unique for a
;;       transducer class, not instance.  You may want multiple intstances of the same
;;       transducer operating on a single stream in some cases.
;;
;; I suppose it's one more for another zoo book
(define* (t-setup-transducer-storage)
  """Return a transducer that creates a shared storage cell as @code{(value . shared-storage)}.
Downstream transducers can then add and remove items from this storage cell.
Each transducer that uses this storage should have a  globally unique ID."""
  (lambda (reducer)
    (case-lambda
      (() (reducer))
      ((result) (reducer result))
      ((result input)
       (let ((input (cons input (make-eq-hashtable))))
         (reducer result input))))))


;; A maximum value transducer that uses transducer storage.
;;
;; Example:
;;
;; (define my-result (list-transduce (compose (t-setup-transducer-storage) (t-max-transducer-storage)) rcons '(3 8 2 5)))
;; (hashtable-ref (cdr (last my-result)) (string-hash "b011b0ba-5b52-43ec-9c04-51caea871a8a") #f)
(define* (t-max-transducer-storage #:optional (ignore-zero? #f))
  (let ((my-uuid "b011b0ba-5b52-43ec-9c04-51caea871a8a"))
    (lambda (reducer)
      (let ((cur-max 0))
	(case-lambda
	  (()
	   ;; This isn't getting called
	   (begin
	     (display (format #f "init case for t-max-transducer-storage\n"))
	     (set! cur-max 0)
	     (reducer)))
	  ((result) (reducer result))
	  ((result input)
	   (let ((value (car input))
		 (ht (hashtable-copy (cdr input) #t)))
	     (begin
               (if (> value cur-max)
               ;; (if (and (> value cur-max)
	       ;; 		(or (not ignore-zero?) (not (= value 0))))
		   (set! cur-max value))
	       (hashtable-set! ht (string-hash my-uuid) cur-max)
	       (reducer result (cons value ht))))))))))

;; A minimum value transducer that uses transducer storage.
;;
;; Example:
;;
;; (define my-result (list-transduce (compose (t-setup-transducer-storage) (t-min-transducer-storage)) rcons '(3 8 2 5)))
;; (hashtable-ref (cdr (last my-result)) (string-hash "79c7a299-c789-47ed-aa04-1cf75cad416f") #f)
(define* (t-min-transducer-storage #:optional (ignore-zero? #f))
  (let ((my-uuid "79c7a299-c789-47ed-aa04-1cf75cad416f"))
    (lambda (reducer)
      (let ((cur-min (inf)))
	(case-lambda
	  (()
	   ;; This isn't getting called
	   (begin
	     (display (format #f "init case for t-min-transducer-storage\n"))
	     (set! cur-min (inf))
	     (reducer)))
	  ((result) (reducer result))
	  ((result input)
	   (let ((value (car input))
		 (ht (hashtable-copy (cdr input) #t)))
	     (begin
               (if (and (< value cur-min)
			(or (not ignore-zero?) (not (= value 0))))
		   (set! cur-min value))
	       (hashtable-set! ht (string-hash my-uuid) cur-min)
	       (reducer result (cons value ht))))))))))
