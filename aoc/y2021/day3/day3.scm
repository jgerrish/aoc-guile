(define-module (aoc y2021 day3 day3)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (aoc port)
  #:use-module (aoc main)
  #:export (bitstring->bitvector
	    sum-bitvector-bits-in-position gamma-rate epsilon-rate
	    boolean-list->binary-string
	    most-common-bit least-common-bit filter-x-bit
	    oxygen-generator-rating co2-scrubber-rating
	    final-value parse-binary day3-part1 day3-part2))


;; Convert a bit string like "11000" to a bitvector
(define (bitstring->bitvector str)
  (list->bitvector (map (lambda (x) (if (eq? x #\1) #t #f)) (string->list str))))

;; Return the sum of bits set in a bit position in a list of bitvectors
;; bit-position is from the left-hand side
;; So: (bitvector-bit-set? #*0001 0) -> #f
;;     (bitvector-bit-set? #*0001 3) -> #t
(define (sum-bitvector-bits-in-position bitvectors bit-position)
  (fold
   (lambda (bitvector prev)
     (begin
       ;; (display (format #f "bitvector: ~a, prev: ~a\n" bitvector prev))
       (+
	(if (bitvector-bit-set? bitvector bit-position) 1 0)
	prev)))
   0
   bitvectors))

;; Return the "x-rate" of a list of bitvectors
(define (rate bitvectors calc)
  ;; Assume all bitvectors have the same length
  (let ((number-bits (bitvector-length (car bitvectors))))
    (let loop-through-bitpositions ((bit-position 0) (result '()))
      (begin
	;; (display (format #f "bit-position: ~a, result: ~a\n" bit-position result))
	(if (eq? bit-position number-bits)
	    (reverse (map calc result))
	    (let ((sum (sum-bitvector-bits-in-position bitvectors bit-position)))
	      (loop-through-bitpositions
	       (1+ bit-position)
	       (cons sum result))))))))

;; Each bit in the gamma rate is the most common value in each position of the bitvectors
;; So, if there three 1s and two 0s in position zero of the bitvectors, then
;; position zero of the gamma-rate is 1
;; Select the most common bit
;; From part two:
;; If 0 and 1 are equally common, keep values with a 1 in the position being considered.
(define (gamma-rate bitvectors)
  (let ((bitvectors-length (length bitvectors)))
    ;; We have a list of sums of the bits
    ;; Find the most common bit
    ;; On ties, pick 1
    ;; TODO: Review this
    (let ((calc (lambda (x) (>= (/ x bitvectors-length) 0.5))))
      (rate bitvectors calc))))

;; Select the least common bit
;; From part two:
;; If 0 and 1 are equally common, keep values with a 0 in the position being considered.
(define (epsilon-rate bitvectors)
  (let ((bitvectors-length (length bitvectors)))
    ;; We have a list of sums of the bits
    ;; Find the most common bit
    ;; On ties, pick 0
    ;; TODO: Review this
    (let ((calc (lambda (x) (< (/ x bitvectors-length) 0.5))))
      (rate bitvectors calc))))

;; Convert a list of #t #f values to a binary string
(define (boolean-list->binary-string lst)
  (list->string
   (map (lambda (x) (if x #\1 #\0)) lst)))

;; parse a binary string like "01001" into an integer
(define (parse-binary str)
  (let ((str-len (string-length str)))
    (cadr
     (string-fold
      (lambda (char result)
	(let* ((bit-pos (car result))
	       (sum (cadr result)))
	  (list (1- bit-pos)
		(if (eq? char #\1)
		    (+ sum (expt 2 (1+ bit-pos)))
		    sum))))
      ;; The intermediate result is a list of the bit position and sum
      (list (- str-len 2)  0)
      str))))

;; Just multiply the values, position times depth
(define (final-value a b)
  (* a b))


;; (final-value
;;   (parse-binary (boolean-list->binary-string (gamma-rate (map bitstring->bitvector my-test))))
;;   (parse-binary (boolean-list->binary-string (epsilon-rate (map bitstring->bitvector my-test)))))


;; Find the most common bit in a bit position in a list of bitvectors
;; returns #t if one is the most common bit, #f is zero is the most common bit
;; From part two:
;; If 0 and 1 are equally common, keep values with a 1 in the position being considered,
;; so this function returns #t when they're equally common
(define (most-common-bit bitvectors bit-position)
  (let ((bitvectors-length (length bitvectors))
	(sum (sum-bitvector-bits-in-position bitvectors bit-position)))
    (if (>= (/ sum bitvectors-length) 0.5)
	#t
	#f)))

;; Find the least common bit in a bit position in a list of bitvectors
;; returns #t if one is the least common bit, #f is zero is the least common bit
;; From part two:
;; If 0 and 1 are equally common, keep values with a 0 in the position being considered.
;; so this function returns #f when they're equally common
(define (least-common-bit bitvectors bit-position)
  (let ((bitvectors-length (length bitvectors))
	(sum (sum-bitvector-bits-in-position bitvectors bit-position)))
    (if (< (/ sum bitvectors-length) 0.5)
	#t
	#f)))

;; Given a bit position, find the most common or least common (or another predicate function)
;; bit value in that position and return the elements with the same value in that position
;; This could be shortened up somehow
(define (filter-x-bit pred bitvectors bit-position)
  ;; Find the most common bit
  (let ((mcb (pred bitvectors bit-position))
	(filter-func (lambda (bv) (bitvector-bit-set? bv bit-position))))
    (filter
     (lambda (x)
       (if mcb
	   (filter-func x)
	   (not (filter-func x))))
     bitvectors)))

;; Get the boolean list from gamma-rate
;; Loop through each element of the boolean list, filtering the bitvectors list
;; on that bit position
;; TODO: Refactor to pass in a most-common-bit style function, not filter function
;; When there is only one number left, stop
(define (device-rating bitvectors filter-function)
  ;; Assume all bitvectors have the same length
  (let ((number-bits (bitvector-length (car bitvectors))))
    (let filter-bitstrings-helper ((bit-position 0) (result bitvectors))
      (begin
	;; (display (format #f "current result: ~a\n" result))
	(if (or (>= bit-position number-bits)
		(eq? (length result) 1))
	    result
	    (filter-bitstrings-helper
	     (1+ bit-position)
	     (filter-x-bit filter-function result bit-position)))))))

;; Get the boolean list from gamma-rate
;; Loop through each element of the boolean list, filtering the bitvectors list
;; on that bit position
;; From part two:
;; The oxygen generator rating keeps the bit strings with the most common bit in a position
;; If 0 and 1 are equally common, keep values with a 1 in the position being considered.
;; TODO: Refactor one more time to pass in a most-common-bit signature function
;; instead of a filter signature function
(define (oxygen-generator-rating bitvectors)
  (device-rating bitvectors most-common-bit))

;; From part two:
;; The co2 scrubber rating keeps the bit strings with the least common bit in a position
;; If 0 and 1 are equally common, keep values with a 0 in the position being considered.
(define (co2-scrubber-rating bitvectors)
  (device-rating bitvectors least-common-bit))

(define (day3-part1 filename)
  (let ((lines (get-lines filename)))
    (display
     (format #f "day 3 part 1 final-value: ~a\n"
	     (final-value
	      (parse-binary
	       (boolean-list->binary-string
		(gamma-rate (map bitstring->bitvector lines))))
	      (parse-binary
	       (boolean-list->binary-string
		(epsilon-rate (map bitstring->bitvector lines)))))))))

(define (day3-part2 filename)
  (let ((lines (get-lines filename)))
    (display
     (format #f "day 3 part 2 final-value: ~a\n"
	     (final-value
	      (parse-binary
	       (boolean-list->binary-string
		(bitvector->list
		 (car
		  (oxygen-generator-rating (map bitstring->bitvector lines))))))
	      (parse-binary
	       (boolean-list->binary-string
		(bitvector->list
		 (car
		  (co2-scrubber-rating (map bitstring->bitvector lines)))))))))))


;; (day3 (get-args))
