;; --- Day 8: Seven Segment Search ---
;; You barely reach the safety of the cave when the whale smashes into
;; the cave mouth, collapsing it. Sensors indicate another exit to
;; this cave at a much greater depth, so you have no choice but to
;; press on.

;; As your submarine slowly makes its way through the cave system, you
;; notice that the four-digit seven-segment displays in your submarine
;; are malfunctioning; they must have been damaged during the
;; escape. You'll be in a lot of trouble without them, so you'd better
;; figure out what's wrong.

;; Each digit of a seven-segment display is rendered by turning on or
;; off any of seven segments named a through g:

;;   0:      1:      2:      3:      4:
;;  aaaa    ....    aaaa    aaaa    ....
;; b    c  .    c  .    c  .    c  b    c
;; b    c  .    c  .    c  .    c  b    c
;;  ....    ....    dddd    dddd    dddd
;; e    f  .    f  e    .  .    f  .    f
;; e    f  .    f  e    .  .    f  .    f
;;  gggg    ....    gggg    gggg    ....

;;   5:      6:      7:      8:      9:
;;  aaaa    aaaa    aaaa    aaaa    aaaa
;; b    .  b    .  .    c  b    c  b    c
;; b    .  b    .  .    c  b    c  b    c
;;  dddd    dddd    ....    dddd    dddd
;; .    f  e    f  .    f  e    f  .    f
;; .    f  e    f  .    f  e    f  .    f
;;  gggg    gggg    ....    gggg    gggg
;; So, to render a 1, only segments c and f would be turned on; the
;; rest would be off. To render a 7, only segments a, c, and f would
;; be turned on.

;; The problem is that the signals which control the segments have
;; been mixed up on each display. The submarine is still trying to
;; display numbers by producing output on signal wires a through g,
;; but those wires are connected to segments randomly. Worse, the
;; wire/segment connections are mixed up separately for each
;; four-digit display! (All of the digits within a display use the
;; same connections, though.)

;; So, you might know that only signal wires b and g are turned on,
;; but that doesn't mean segments b and g are turned on: the only
;; digit that uses two segments is 1, so it must mean segments c and f
;; are meant to be on. With just that information, you still can't
;; tell which wire (b/g) goes to which segment (c/f). For that, you'll
;; need to collect more information.

;; For each display, you watch the changing signals for a while, make
;; a note of all ten unique signal patterns you see, and then write
;; down a single four digit output value (your puzzle input). Using
;; the signal patterns, you should be able to work out which pattern
;; corresponds to which digit.

;; For example, here is what you might see in a single entry in your
;; notes:

;; acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
;; cdfeb fcadb cdfeb cdbaf
;; (The entry is wrapped here to two lines so it fits; in your notes,
;; it will all be on a single line.)

;; Each entry consists of ten unique signal patterns, a | delimiter,
;; and finally the four digit output value. Within an entry, the same
;; wire/segment connections are used (but you don't know what the
;; connections actually are). The unique signal patterns correspond to
;; the ten different ways the submarine tries to render a digit using
;; the current wire/segment connections. Because 7 is the only digit
;; that uses three segments, dab in the above example means that to
;; render a 7, signal lines d, a, and b are on. Because 4 is the only
;; digit that uses four segments, eafb means that to render a 4,
;; signal lines e, a, f, and b are on.

;; Using this information, you should be able to work out which
;; combination of signal wires corresponds to each of the ten
;; digits. Then, you can decode the four digit output
;; value. Unfortunately, in the above example, all of the digits in
;; the output value (cdfeb fcadb cdfeb cdbaf) use five segments and
;; are more difficult to deduce.

;; For now, focus on the easy digits. Consider this larger example:

;; be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
;; fdgacbe cefdb cefbgd gcbe
;; edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
;; fcgedb cgb dgebacf gc
;; fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
;; cg cg fdcagb cbg
;; fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
;; efabcd cedba gadfec cb
;; aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
;; gecf egdcabf bgf bfgea
;; fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
;; gebdcfa ecba ca fadegcb
;; dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
;; cefg dcbef fcge gbcadfe
;; bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
;; ed bcgafe cdgba cbgef
;; egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
;; gbdfcae bgc cg cgb
;; gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
;; fgae cfgab fg bagce
;; Because the digits 1, 4, 7, and 8 each use a unique number of
;; segments, you should be able to tell which combinations of signals
;; correspond to those digits. Counting only digits in the output
;; values (the part after | on each line), in the above example, there
;; are 26 instances of digits that use a unique number of segments
;; (highlighted above).

;; In the output values, how many times do digits 1, 4, 7, or 8
;; appear?


;; --- Part Two ---
;; Through a little deduction, you should now be able to determine the
;; remaining digits. Consider again the first example above:

;; acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
;; cdfeb fcadb cdfeb cdbaf
;; After some careful analysis, the mapping between signal wires and
;; segments only make sense in the following configuration:

;;  dddd
;; e    a
;; e    a
;;  ffff
;; g    b
;; g    b
;;  cccc
;; So, the unique signal patterns would correspond to the following
;; digits:

;; acedgfb: 8
;; cdfbe: 5
;; gcdfa: 2
;; fbcad: 3
;; dab: 7
;; cefabd: 9
;; cdfgeb: 6
;; eafb: 4
;; cagedb: 0
;; ab: 1
;; Then, the four digits of the output value can be decoded:

;; cdfeb: 5
;; fcadb: 3
;; cdfeb: 5
;; cdbaf: 3
;; Therefore, the output value for this entry is 5353.

;; Following this same process for each entry in the second, larger
;; example above, the output value of each entry can be determined:

;; fdgacbe cefdb cefbgd gcbe: 8394
;; fcgedb cgb dgebacf gc: 9781
;; cg cg fdcagb cbg: 1197
;; efabcd cedba gadfec cb: 9361
;; gecf egdcabf bgf bfgea: 4873
;; gebdcfa ecba ca fadegcb: 8418
;; cefg dcbef fcge gbcadfe: 4548
;; ed bcgafe cdgba cbgef: 1625
;; gbdfcae bgc cg cgb: 8717
;; fgae cfgab fg bagce: 4315
;; Adding all of the output values in this larger example produces
;; 61229.

;; For each entry, determine all of the wire/segment connections and
;; decode the four-digit output values. What do you get if you add up
;; all of the output values?


;; Notes

;; Complex solution: This can be solved with a constraint solver or
;; declarative logic program.

;; Simple solution: There's a couple simple paths that will identify
;; every wire.
(define-module (aoc y2021 day8 day8)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module ((rnrs hashtables) :version (6))
  #:use-module ((rnrs records syntactic) :version (6))
  #:use-module (aoc port)
  #:use-module (aoc main)
  #:export (wires
	    make-wires wires?
	    wires-a wires-b wires-c wires-d wires-e wires-f wires-g
	    get-wires
	    lookup-func-builder
	    wires-to-key wires-to-digits
	    list->hashtable functional-wires-update
	    solve-a-cf solve-c-f solve-b solve-d solve-g solve-e
	    parse-line index-by-number-of-wires solve
	    day8-part1 day8-part2))

;; Define a record type for storing the wires / segments
;; This is an record with all fields immutable.
;; It must be constructed in a functional manner.
(define-record-type (wires make-wires wires?)
  (fields
   (immutable a wires-a)
   (immutable b wires-b)
   (immutable c wires-c)
   (immutable d wires-d)
   (immutable e wires-e)
   (immutable f wires-f)
   (immutable g wires-g)))


;; To construct a wires structure with a and b set, and the rest unset:
;; (make-wires #\a #\b #f #f #f #f #f)

;; To access the a field:
;; (wires-a (make-wires #\a #\b #f #f #f #f #f))

;; Get a set of wires from a wires structure
(define (get-wires wires list-of-wires)
  (map
   (lambda (wire-field-name)
     (cond
      ((eq? wire-field-name #\a) (wires-a wires))
      ((eq? wire-field-name #\b) (wires-b wires))
      ((eq? wire-field-name #\c) (wires-c wires))
      ((eq? wire-field-name #\d) (wires-d wires))
      ((eq? wire-field-name #\e) (wires-e wires))
      ((eq? wire-field-name #\f) (wires-f wires))
      ((eq? wire-field-name #\g) (wires-g wires))
      (else '())))
   list-of-wires))

(define (lookup-func-builder wires)
  (lambda (character)
    (cond
      ((eq? character #\a) (wires-a wires))
     ((eq? character #\b) (wires-b wires))
     ((eq? character #\c) (wires-c wires))
     ((eq? character #\d) (wires-d wires))
     ((eq? character #\e) (wires-e wires))
     ((eq? character #\f) (wires-f wires))
     ((eq? character #\g) (wires-g wires)))))

;; Create a custom key to index wires.
;; This key is unique for every combination wires,
;; but isn't a good hash key since it's not uniformly distributed.
;; lookup-func is a mapping function to convert keys before lookig
;; them up.
(define (wires-to-key wires lookup-func)
  (fold
   (lambda (wire-field-name prev)
     (begin
       (+ prev
	  (expt
	   2
	   (cond
	    ((eq? wire-field-name (lookup-func #\a)) 0)
	    ((eq? wire-field-name (lookup-func #\b)) 1)
	    ((eq? wire-field-name (lookup-func #\c)) 2)
	    ((eq? wire-field-name (lookup-func #\d)) 3)
	    ((eq? wire-field-name (lookup-func #\e)) 4)
	    ((eq? wire-field-name (lookup-func #\f)) 5)
	    ((eq? wire-field-name (lookup-func #\g)) 6))))))
   0
   wires))

;; Using the wires-to-keys key function, convert from wires to digits.
;; Since the signals are mixed up, there needs to be a step before
;; this.
;; TODO: Need to make a map function to go from the mixed up signals
;; to the normal signals.
(define (wires-to-digits wires mapped)
  (let ((key (wires-to-key wires (lookup-func-builder mapped))))
    (cond
     ((eq? key 119) 0)
     ((eq? key 36) 1)
     ((eq? key 93) 2)
     ((eq? key 109) 3)
     ((eq? key 46) 4)
     ((eq? key 107) 5)
     ((eq? key 123) 6)
     ((eq? key 37) 7)
     ((eq? key 127) 8)
     ((eq? key 111) 9))))

;; Convert a list of key-value lists to a hashtable
(define (list->hashtable lst)
  (let ((ht (make-eq-hashtable)))
    (fold (lambda (x prev)
	    (begin
	      ;; (display (format #f "Adding x: ~a\n" x))
	      (hashtable-set! prev (car x) (cadr x))
	      prev))
	  ht
	  lst)))

;; Update a wires structure with a list of updated fields.
;; The list of updated fields is simply a list of lists.
;; Each sublist should have the field name as a character
;; and the new value for that field as a character.
;;
;; We use a hashtable, instead of a more efficient lookup-table for
;; the signal (character) to segment (character) lookup.  On most
;; machines and this version of Scheme, integer representations of
;; characters are ordered and continuous, but this may not be always
;; the case.
;;
;; We also create an entirely new wires record, treating the wires record
;; as an immutable data structure since every field is immutable.
;;
;; TODO: Save the hashtable in the record
(define (functional-wires-update wires updated-fields)
  ;; First, convert the updated-fields to a hashmap
  ;; (display (format #f "updated fields: ~a\n" updated-fields))
  (let ((ht (list->hashtable updated-fields)))
    (make-wires
     (hashtable-ref ht #\a (wires-a wires))
     (hashtable-ref ht #\b (wires-b wires))
     (hashtable-ref ht #\c (wires-c wires))
     (hashtable-ref ht #\d (wires-d wires))
     (hashtable-ref ht #\e (wires-e wires))
     (hashtable-ref ht #\f (wires-f wires))
     (hashtable-ref ht #\g (wires-g wires)))))

;; unique solutions
;; Some wires can be solved by looking at specific digits
;; For example, after finding the wires in digit 1,
;; We can look at digit 2, 5, and 6 to determine what c and f are
;; 2: 5, 5: 5, 6: 6

;; Looking at # wires 2 and # wires 3 we can find what 'a is (the
;; unique one in # wires 3)

;; These function parameters require having certain solutions set.  We
;; could define a tag or type system to ensure that requirements are
;; met.  A custom structure or class would work here, with custom slots
;; for each wire / segment / letter.
;; TODO: Move to a custom class or structure, test the class on each
;; function.
;; TODO low-priority: These functions use the lset-* functions, which operate on lists
;; and are inefficient.  We can move to efficient set operations.

;; For now, functions are named solve-*, where * is something like
;; ab or a-c
;; ab means both a and b are solved, but it's unknown which is which.
;; a-c means a and c are solved, a is first in the list, c is second.

;; Returns a list with two elements:
;; a list containing a wires field with a set and a list containing c
;; and f
(define (solve-a-cf index wires-record)
  (let ((two-wires (string->list (car (hashtable-ref index 2 0))))
	(three-wires (string->list (car (hashtable-ref index 3 0)))))
    (begin
      ;; (display (format #f "two-wires: ~a, three-wires: ~a\n" two-wires three-wires))
      (list
       (functional-wires-update
	wires-record
	(list (list #\a (car (lset-difference eqv? three-wires two-wires)))))
       (lset-intersection eqv? three-wires two-wires)))))

;; Looking at digits 0, 6, 9 which all have 6 wires:
;; The one without both c and f is missing c (has f), so we can identify
;; from that we can identify f
;; Returns a list with two elements, c and f, in order
(define (solve-c-f index cf wires-record)
  (let ((six-wires (map string->list (hashtable-ref index 6 0))))
    ;; Find the one with c or f missing
    (begin
      ;; (display (format #f "six-wires: ~a\n" six-wires))
      (let ((one-with-c
	     (car (filter (lambda (x)
			    (begin
			      ;; (display (format #f "Comparing ~a and ~a\n" x cf))
			      (= (length (lset-intersection eqv? x cf)) 1)))
			  six-wires))))
	;; Identify c
	;; Search for the first element in cf.
	;; The elements returned from the second list of solve-a-cf are unordered,
	;; But this will determine which is which
	(if (member (car cf) one-with-c)
	    ;; We found the element f, it's the car, so return the cadr first
	    ;; TODO: Update this
	    (functional-wires-update
	     wires-record
	     (list
	      (list #\c (cadr cf))
	      (list #\f (car cf))))
	    ;; We didn't find the element f, we don't need to bother searching again
	    ;; just immediately return the car and cadr
	    (functional-wires-update
	     wires-record
	     (list
	      (list #\c (car cf))
	      (list #\f (cadr cf)))))))))

;; (solve-c-f my-index (solve-a-cf my-index))

;; We now know a, c and f
;; Looking at digits 2, 3 and 5
;; All three have a, d and g
;; The one without c has b
(define (solve-b index wires-record)
  (let ((five-wires (map string->list (hashtable-ref index 5 0))))
    ;; Find a, d and g
    (let ((c (wires-c wires-record))
	  (f (wires-f wires-record))
	  (adg
	   (fold
	    (lambda (x prev) (lset-intersection eqv? x prev))
	    '(#\a #\b #\c #\d #\e #\f #\g)
	    five-wires)))
      ;; Find the one with c missing
      (begin
	(let ((one-without-c
	       (car (filter (lambda (x)
			      (begin
				;; (display (format #f "Comparing ~a and ~a\n" x c))
				(= (length (lset-intersection eqv? x (list c))) 0)))
			    five-wires))))
	  (begin
	    ;; (display (format #f "one-without-c: ~a, adg: ~a, c: ~a, f: ~a\n"
	    ;; 		     one-without-c adg c f))
	    (let ((adfg (cons f adg)))
	      (begin
		;; (display (format #f "adfg: ~a\n" adfg))
		(let ((b (lset-difference eqv? one-without-c adfg)))
		  (functional-wires-update
		   wires-record
		   (list (list #\b (car b)))))))))))))

;; (solve-b my-index (solve-c-f my-index (solve-a-cf my-index)))


;; Then we have a, b, c and f
;; Then look at four, we can find d
(define (solve-d index wires-record)
  (let ((four-wires (map string->list (hashtable-ref index 4 0))))
    ;; (display (format #f "four-wires: ~a\n" four-wires))
    (let ((bcf
	   (list (wires-b wires-record)
		 (wires-c wires-record)
		 (wires-f wires-record))))
      (let ((d (lset-difference eqv? (car four-wires) bcf)))
	(functional-wires-update wires-record (list (list #\d (car d))))))))

;; (solve-d my-index (append (solve-b my-index (solve-c-f my-index (solve-a-cf my-index)))
;;                           (cadr (solve-a-cf my-index))))


;; Now we have a, b, c, d and f
;; We can then find g, so we know a, b, c, d, f and g
(define (solve-g index wires-record)
  (let ((five-wires (map string->list (hashtable-ref index 5 0))))
    (let ((a (wires-a wires-record))
	  (d (wires-d wires-record))
	  (adg
	   (fold
	    (lambda (x prev)
	      (lset-intersection eqv? x prev))
	    '(#\a #\b #\c #\d #\e #\f #\g)
	    five-wires)))
      (functional-wires-update
       wires-record
       (list (list #\g (car (lset-difference eqv? adg (list a) (list d)))))))))

;; (solve-g my-index (car (solve-a-cf my-index)) (solve-d my-index (append (solve-b my-index (solve-c-f my-index (solve-a-cf my-index))) (cadr (solve-a-cf my-index)))) (append (solve-b my-index (solve-c-f my-index (solve-a-cf my-index))) (cadr (solve-a-cf my-index))))


;; The last remaining one is e
(define (solve-e wires-record)
  (functional-wires-update
   wires-record
   (list
    (list #\e (car (lset-difference
		    eqv?
		    '(#\a #\b #\c #\d #\e #\f #\g)
		    (filter
		     (lambda (x) (not (null? x)))
		     (get-wires wires-record '(#\a #\b #\c #\d #\e #\f #\g)))))))))


;; Some alternate paths for solving this problem are commented out below:

;; Looking at digits 2, 3 and 5 which all have 5 wires:
;; we can tie c to e and b to f
;; tools needed:
;;   Find 3, the one with both c and f, mark a d and g to ignore
;;   Find 2, the one with c, identify e
;;   Find 5, the one with f, identify b
;; (define (solve-e-b index a-cf)
;;   (let ((six-wires (map string->list (hashtable-ref index 6 0))))
;;     ;; Find the one with c or f missing
;;     (begin
;;       (display (format #f "six-wires: ~a\n" six-wires))
;;       (let ((one-with-c
;; 	     (car (filter (lambda (x)
;; 			    (begin
;; 			      (display (format #f "Comparing ~a and ~a\n" x (cadr a-cf)))
;; 			      (= (length (lset-intersection eqv? x (cadr a-cf))) 1)))
;; 			  six-wires))))
;; 	;; Identify c
;; 	;; Search for the first element in cf.
;; 	;; The elements returned from the second list of solve-a-cf are unordered,
;; 	;; But this will determine which is which
;; 	(if (member (car (cadr a-cf)) one-with-c)
;; 	    ;; We found the element f, it's the car, so return the cadr first
;; 	    (list (cadr (cadr a-cf)) (car (cadr a-cf)))
;; 	    ;; We didn't find the element f, we don't need to bother searching again
;; 	    ;; just immediately return the car and cadr
;; 	    (list (car (cadr a-cf)) (cadr (cadr a-cf))))))))

;; Look at digit 4, d is common to 2, 3 and 5
;; b, c and f are known.  e can be identified
;; from that c can be identified
;; then b and f can be identified

;; five wires should be identified now: a, b, c, e and f using digit
;; 4, d can be identified.  Then the last one identified.


;; Parse a single line from the input file

;; Each line consists of ten unique signal patterns, a | delimiter,
;; and the four digit output value.  Like:
;;
;; acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
;; cdfeb fcadb cdfeb cdbaf
;;
;; This function parses the line and returns it as a list of two lists.
;; The first list conains ten elements, the ten signal patterns.
;; The second list contains four elements, the four output values.
;;
;; TODO: Compile the regex in here and the other days
(define (parse-line line)
  (let ((line-match (string-match "([^|]+)([ |]+)(.+)" line)))
    (if line-match
	(begin
	  ;; (display (format #f "line-match: ~a\n" line-match))
	  (let ((p1 (match:substring line-match 1))
		(p2 (match:substring line-match 3)))
	    (begin
	      ;; (display (format #f "p1: ~a, p2: ~a\n" p1 p2))
	      (list
	       (map match:substring (list-matches "[a-z]+" p1))
	       (map match:substring (list-matches "[a-z]+" p2))))))
	'( () () ))))

;; (define my-parse (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))

;; Index the read in data.

;; First, we want to do things like look up digit 1, the signal pattern with
;; two segments / wires.
(define (index-by-number-of-wires signal-patterns)
  (let ((ht (make-eq-hashtable)))
    (fold (lambda (x prev)
	    (begin
	      ;; (display (format #f "Adding x: ~a\n" x))
	      (hashtable-update! prev (string-length x) (lambda (val) (cons x val)) '())
	      prev))
	  ht
	  signal-patterns)))

;; (define my-ht (index-by-number-of-wires (car my-parse)))


;; (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))

;; Full set of instructions
(define (solve line)
  (let* ((my-parse (parse-line line))
	(my-ht (index-by-number-of-wires (car my-parse)))
	(my-wires (make-wires #f #f #f #f #f #f #f))
	(res (solve-a-cf my-ht my-wires)))
    (let ((my-wires (car res))
	  (cf (cadr res)))
      (solve-e
       (solve-g my-ht (solve-d my-ht (solve-b my-ht (solve-c-f my-ht cf my-wires))))))))

(define (day8-part1 filename)
  (let ((lines (get-lines filename))
	(counts (make-eq-hashtable)))
    (begin
      (map
       (lambda (line)
	 (begin
	   (let ((my-parse (parse-line line))
		 (mapped (solve line)))
	     (map
	      (lambda (x)
		(let ((soln (wires-to-digits
			     (string->list x)
			     mapped)))
		  (hashtable-update!
		   counts
		   soln
		   (lambda (val)
		     (begin
		       (1+ val)))
		   0)))
	      (cadr my-parse)))))
       lines)
      (let ((result (vector->list (hashtable-keys counts))))
	(display (format #f "day 8 part 1 final-value:: ~a\n"
			 (fold
			  (lambda (a prev)
			    (+ prev (hashtable-ref counts a 0)))
			  0
			  (filter
			   (lambda (x)
			     (or (= x 1) (= x 4) (= x 7) (= x 8)))
			   result))))))))

;; TODO: Clean up, refactor, test
(define (day8-part2 filename)
  (let ((lines (get-lines filename))
	(counts (make-eq-hashtable)))
    (begin
      (display (format #f "day 8 part 2 final-value:: ~a\n" (reduce + 0
	      (map
	       (lambda (line)
		 (begin
		   (let ((my-parse (parse-line line))
			 (mapped (solve line)))
		     ;; Every line has four digits we need to decode
		     ;; We then need to combine those digits to a four-digit number
		     (let ((result
			    (let build-digits ((digits (cadr my-parse)) (current-number ""))
			      (if (null? digits)
				  (string->number current-number)
				  (let ((digit (car digits))
					(next (cdr digits)))
				    (let ((soln
					   (wires-to-digits (string->list digit) mapped)))
				      (build-digits
				       next
				       (string-append current-number (number->string soln)))))))))
		       result))))
	       lines)))))))
