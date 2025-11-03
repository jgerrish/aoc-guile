;; --- Day 14: Extended Polymerization ---

;; The incredible pressures at this depth are starting to put a strain
;; on your submarine. The submarine has <a href="
;; https://en.wikipedia.org/wiki/Polymerization" target="_blank"
;; >polymerization</a> equipment that would produce suitable materials
;; to reinforce the submarine, and the nearby volcanically-active
;; caves should even have the necessary input elements in sufficient
;; quantities.

;; The submarine manual contains instructions for finding the optimal
;; polymer formula; specifically, it offers a polymer template and a
;; list of pair insertion rules (your puzzle input). You just need to
;; work out what polymer would result after repeating the pair
;; insertion process a few times.

;; For example:

;; NNCB

;; CH > B
;; HH > N
;; CB > H
;; NH > C
;; HB > C
;; HC > B
;; HN > C
;; NN > C
;; BH > H
;; NC > B
;; NB > B
;; BN > B
;; BB > N
;; BC > B
;; CC > N
;; CN > C

;; The first line is the polymer template - this is the starting point
;; of the process.

;; The following section defines the pair insertion rules. A rule like
;; AB > C means that when elements A and B are immediately adjacent,
;; element C should be inserted between them. These insertions all
;; happen simultaneously.

;; So, starting with the polymer template NNCB, the first step
;; simultaneously considers all three pairs:

;; * The first pair (NN) matches the rule NN > C, so element C is
;; inserted between the first N and the second N.

;; * The second pair (NC) matches the rule NC > B, so element B is
;; inserted between the N and the C.

;; * The third pair (CB) matches the rule CB > H, so element H is
;; inserted between the C and the B.

;; Note that these pairs overlap: the second element of one pair is
;; the first element of the next pair. Also, because all pairs are
;; considered simultaneously, inserted elements are not considered to
;; be part of a pair until the next step.

;; After the first step of this process, the polymer becomes NCNBCHB.

;; Here are the results of a few steps using the above rules:

;; Template:     NNCB
;; After step 1: NCNBCHB
;; After step 2: NBCCNBBBCBHCB
;; After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
;; After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB

;; This polymer grows quickly. After step 5, it has length 97; After
;; step 10, it has length 3073. After step 10, B occurs 1749 times, C
;; occurs 298 times, H occurs 161 times, and N occurs 865 times;
;; taking the quantity of the most common element (B, 1749) and
;; subtracting the quantity of the least common element (H, 161)
;; produces 1749 - 161 = 1588.

;; Apply 10 steps of pair insertion to the polymer template and find
;; the most and least common elements in the result. What do you get
;; if you take the quantity of the most common element and subtract
;; the quantity of the least common element?

;; --- Part Two ---

;; The resulting polymer isn't nearly strong enough to reinforce the
;; submarine. You'll need to run more steps of the pair insertion
;; process; a total of 40 steps should do it.

;; In the above example, the most common element is B (occurring
;; 2192039569602 times) and the least common element is H (occurring
;; 3849876073 times); subtracting these produces 2188189693529.

;; Apply 40 steps of pair insertion to the polymer template and find
;; the most and least common elements in the result. What do you get
;; if you take the quantity of the most common element and subtract
;; the quantity of the least common element?

(define-module (aoc y2021 day14 main)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-111)
  #:use-module (srfi srfi-171)
  #:use-module (srfi srfi-171 gnu)
  #:use-module (srfi srfi-171 meta)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 receive)
  #:use-module ((aoc logging) :prefix logging:)
  #:use-module (aoc port)
  #:use-module ((aoc y2021 day14 polymer-instructions-parser) :prefix pip:)
  #:use-module ((aoc y2021 day14 transducers) :prefix td:)
  #:export (&char-error transparent-paper line point-line fold-line get-pairs
			<letter-freq-list> make-letter-freq-list
			character-pair->integer character-pairs->unique-id-map
			rules->unique-id-map least-freq most-freq
			map-it build-lookup-table
			transform-template
			<graph-node> build-graph
			build-subproblem-chart init-subproblem-chart fill-chart make-the-chart
			run-it
			day14-part1 day14-part2))


(use-modules (ice-9 match))
(use-modules (ice-9 peg))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9 gnu))
(use-modules (srfi srfi-171))
(use-modules (srfi srfi-171 gnu))
(use-modules (srfi srfi-171 meta))
(use-modules ((aoc y2021 day14 polymer-instructions-parser) :prefix pip:))

;; Create a custom exception type &char-error that accepts an
;; argument called char, the character that caused the error
(define &char-error
  (make-exception-type '&char-error &exception (list 'char)))

(define make-char-error-with-char
  (record-constructor &char-error))

;; A letter-freq-list is a custom data structure that has letter
;; frequencies.  It uses a vector to store those frequencies.
;;
;; Only upper-case letters, A-Z, are supported
(define-immutable-record-type <letter-freq-list>
  (make-letter-freq-list data)
  letter-freq-list?
  (data letter-freq-list-data set-letter-freq-list-data))

(define (count-letter-freq str)
  (fold
   (lambda (letter freqs)
     (letter-freq-list-inc freqs letter))
   (make-letter-freq-list (make-vector 26 0))
   (string->list str)))


(define (letter-freq-list-ref lfl letter)
  (let ((letter-code (char->integer letter)))
      (if (or (< letter-code 65) (> letter-code 90))
	  (raise-exception
	   (make-char-error-with-char letter-code))
	  (let ((letter-index (- letter-code 65)))
	    (vector-ref (letter-freq-list-data lfl) letter-index)))))

;; Returns a new immutable letter-freq-list
(define (letter-freq-list-inc lfl letter)
  (let ((letter-code (char->integer letter)))
      (if (or (< letter-code 65) (> letter-code 90))
	  (raise-exception
	   (make-char-error-with-char letter-code))
	  (let ((letter-index (- letter-code 65))
		(data (vector-copy (letter-freq-list-data lfl))))
	    (set-letter-freq-list-data
	     lfl
	     (begin
	       (vector-set! data letter-index (1+ (vector-ref data letter-index)))
	       data))))))

;; Adds the corresponding elements of two letter frequency lists.
;; Returns a new immutable letter-freq-list
(define (letter-freq-list-add lfl1 lfl2)
  (let* ((v1 (letter-freq-list-data lfl1))
	 (v2 (letter-freq-list-data lfl2))
	 (vl1 (vector-length v1))
	 (vl2 (vector-length v2))
	 (new-vec (vector-copy v1)))
    (if (not (= vl1 vl2))
	(raise-exception "unequal letter frequency list length exception")
	(let ((len (min vl1 vl2)))
	  (do ((i 0 (1+ i)))
	      ((>= i vl1))
	    (vector-set! new-vec i (+ (vector-ref new-vec i) (vector-ref v2 i))))
	  (make-letter-freq-list new-vec)))))

;; Parse a set of lines from the PEG tree
;; Returns a pair of two lists:
;; The template is the first item in the pair
;; A list of rules is the second item in the pair
(define (parse-lines lst)
  (fold
   (lambda (parsed-line-with-tag prev)
     (let ((tag (car parsed-line-with-tag))
	   (parsed-line (cdr parsed-line-with-tag)))
       (if (eq? tag 'template)
	   (cons (append (car prev) (list parsed-line-with-tag))
		 (cdr prev))
	   (cons (car prev)
		 (append (cdr prev) (list parsed-line-with-tag))))))
   (cons '() '())
   lst))

;; Split a template into a list of sequential pairs of characters
;; (get-pairs (template "NNCB")) -> ((#\C . #\B) (#\N . #\C) (#\N . #\N))
(define (get-pairs tagged-template)
  (let ((template (cadr tagged-template)))
    (cadr
     (fold
      (lambda (char prev)
	(let ((prev-char (car prev))
	      (pairs (cadr prev)))
	  (begin
	    (if (null? prev-char)
		;; This is the first character
		(list char pairs)
		;; This is every other character
		(list char (cons (cons prev-char char) pairs))))))
      (list '() '())
      (string->list template)))))


;; Alright.  Now what can we do.
;; Create a custom pair -> integer function?
;; It's not a true hash function, it's a map from character pairs to
;; unique IDs:
;;
;; It needs to meet these requirements:
;;   1. Every different character pair must map to a different ID.
;;   2. Order of characters in the pair matters.
;; Should be easy.  Map pairs of characters to unique IDs
;; (e.g. 26*char1 + char2)
;; First map ASCII values to integers in the range 0-26
;;
;; Then go through all the rules mapping each one to an integer
;; Same with the result of get-pairs
;; Then a simple lookup.  We can use a vector, 676 elements.  I'm
;; tired of dealing with hash tables in Guile.
;;
;; We don't actually need a 676 element long vector.  The number of
;; characters in use is less than 26.  It's 10 or so.
;; But keeping it a constant 676 means no intermediate lookup tables.
;; It's simple.
(define (character-pair->integer chars)
  (let ((first-char-code (char->integer (car chars)))
	(second-char-code (char->integer (cdr chars))))
    (begin
      (if (or (< first-char-code 65) (> first-char-code 90))
	  ;; Throw an exception
	  (raise-exception
	   (make-char-error-with-char (first-char-code))))
      (if (or (< second-char-code 65) (> second-char-code 90))
	  ;; Throw an exception
	  (raise-exception
	   (make-char-error-with-char (second-char-code))))
      (let ((first-char-code (- first-char-code 65))
	    (second-char-code (- second-char-code 65)))
	(+ (* first-char-code 26) second-char-code)))))

(define (character-pairs->unique-id-map pairs)
  (map (lambda (pair)
	 (cons
	  (character-pair->integer pair)
	  (list pair)))
  pairs))

;; Return a pair of the least frequent letter in a string along with
;; the number of occurences.
;;
;; I did some coolness with transducers in transducers.scm
;; It's used for part2 of the solution.
(define (least-freq freqs)
  (let lp ((cur-min-value (inf)) (cur-min-index 0) (index 0))
    (if (< index 26)
	(let ((letter (integer->char (+ index 65))))
	  ;; First test whether the frequency is zero.  If it is we ignore it.
	  ;; Then test whether we have a new minimum.
	  (if (and (not (= (letter-freq-list-ref freqs letter) 0))
		   (< (letter-freq-list-ref freqs letter) cur-min-value))
	      (lp (letter-freq-list-ref freqs letter) index (1+ index))
	      (lp cur-min-value cur-min-index (1+ index))))
	(cons (integer->char (+ cur-min-index 65)) cur-min-value))))

;; Return a pair of the most frequent letter in a string along with
;; the number of occurences.
;;
;; I did some coolness with transducers in transducers.scm
;; It's used for part2 of the solution.
(define (most-freq freqs)
  (let lp ((cur-max-value 0) (cur-max-index 0) (index 0))
    (if (< index 26)
	(let ((letter (integer->char (+ index 65))))
	  ;; First test whether the frequency is zero.  If it is we ignore it.
	  ;; Then test whether we have a new maximum.
	  (if (and (not (= (letter-freq-list-ref freqs letter) 0))
		   (> (letter-freq-list-ref freqs letter) cur-max-value))
	      (lp (letter-freq-list-ref freqs letter) index (1+ index))
	      (lp cur-max-value cur-max-index (1+ index))))
	(cons (integer->char (+ cur-max-index 65)) cur-max-value))))


(define (rules->unique-id-map rules)
  (map
   (lambda (rule)
     (let ((char-list (string->list (second (cadr rule)))))
       (let ((char-pair (cons (first char-list) (second char-list))))
	 (cons
	  (character-pair->integer char-pair)
	  (second (caddr rule))))))
   rules))


(define (map-it peg-tree)
  (let ((instructions (parse-lines peg-tree)))
    (let ((pairs (get-pairs (caar instructions)))
	  (rules (cdr instructions)))
      (cons
       (character-pairs->unique-id-map pairs)
       (list (rules->unique-id-map rules))))))

(define (build-lookup-table rule-map)
  (let ((lookup-table (make-vector 676 #f)))
    (for-each
     (lambda (rule)
       (let ((key (car rule))
	     ;; value here is of the form: (cg-range "H")
	     ;; We only want the letter string, so use caddr
	     (value (caddr rule)))
	 (vector-set! lookup-table key value)))
     rule-map)
    lookup-table))


(define (transform-template template lookup-table)
  (cdr
   (fold
    (lambda (cur prev)
      (let ((prev-char (car prev))
	    (built-string (cdr prev)))
	(if (not (char? prev-char))
	    (cons cur (string-append built-string (string cur)))
	    (begin
	      ;; TODO: Alright, we can optimize this calculation
	      ;;
	      ;; transform-template gets run in a loop, and this
	      ;; function has a fold too.  We calculate the integer
	      ;; value of the character-pair A LOT.  I don't know if
	      ;; it's one of the limiting factors but it might be.
	      (let ((key (character-pair->integer (cons prev-char cur))))
		(let ((lookup (vector-ref lookup-table key)))
		  (if lookup
		      (cons cur (string-append built-string lookup (string cur)))
		      (cons cur (string-append built-string (string cur))))))))))
    (cons #f "")
    (string->list template))))

;; Original algorithm to solve day1.  Expands the string, building a
;; large string.  Very inefficient but works for 10 iterations on
;; modern machines.
(define (run-it peg-tree iterations)
  (let ((orig-template (cadar peg-tree))
	(mapped (map-it peg-tree)))
    (let ((lookup-table (build-lookup-table (cadr mapped))))
      (let ((pair-insertion-result
	     (let lp ((template orig-template) (x iterations))
	       (if (positive? x)
		   (let ((res (transform-template template lookup-table)))
		     (lp res (- x 1)))
		   template))))
	(let ((letter-freqs (count-letter-freq pair-insertion-result)))
	  (cons (most-freq letter-freqs) (least-freq letter-freqs)))))))


;; Day 14 part 2, moving to a dynamic programming solution

;; dynamic programming functions


;; Working with rule graphs

;; Build a graph from the rules

;; A graph-node data type
;;
;; It contains the rule as a pair, the element the rule generates, and
;; links for the two pairs an expanded rule will create.
;; For example, given the rule CH -> B
;; The rule is:    '(#\C . #\H)
;; The element is: #\B
;; The lhs is:     '(#\C #\B)
;; The rhs is:     '(#\B #\H)
(define-immutable-record-type <graph-node>
  (make-graph-node rule element lhs rhs)
  graph-node?
  (rule graph-node-rule)
  (element graph-node-element)
  (lhs graph-node-lhs)
  (rhs graph-node-rhs))

;; Buid a rule graph from all the instructions
;;
;; Each node in a rule graph is <graph-node>.
;;
;; It contains the rule as a pair, the element the rule generates, and
;; links for the two pairs an expanded rule will create.
;; For example, given the rule CH -> B
;; The rule is:    '(#\C . #\H)
;; The element is: #\B
;; The lhs is:     '(#\C #\B)
;; The rhs is:     '(#\B #\H)
;;
;; Technically a real graph would have edges, but this is enough
;; information to fill in the chart.
(define (build-graph peg-tree)
  (let ((instructions (parse-lines peg-tree)))
    (let ((rules (cdr instructions)))
      (map
       (lambda (x)
	 (let ((pair (string->list (cadr (cadr x)))))
	   (let ((rule (cons (car pair) (cadr pair)))
		 (element (string-ref (car (cdr (car (cdr (car (cddr x)))))) 0)))
	     (let ((lhs (cons (car rule) element))
		   (rhs (cons element (cdr rule))))
	       (make-graph-node rule element lhs rhs)))))
       rules))))


;; Still using a max number of rules of 676 (A-Z)*(A-Z)
;; instead of optimizing for each input set.
;; It keeps it a little easier.
(define (build-subproblem-chart num-rules iterations)
  ;; TODO: Once again, make sure I have row-major / column-major clear here
  ;; Given (build-subproblem-chart 676 40), this works:
  ;; (array-set!
  ;;   my-chart
  ;;   (letter-freq-list-inc (make-letter-freq-list (make-vector 26 0)) #\C)
  ;;   53
  ;;   0)
  (make-array 0 (list 0 num-rules) (list 0 iterations)))


;; Initialize row zero of the dynamic programming chart with the
;; initial letters each rule generates.
(define (init-subproblem-chart graph chart)
  (begin
    (for-each
     (lambda (node)
       (let ((idx (character-pair->integer (graph-node-rule node)))
	     (element (graph-node-element node)))
	 (let ((freqs (letter-freq-list-inc (make-letter-freq-list (make-vector 26 0)) element)))
	   (array-set! chart freqs idx 0))))
     graph)
    chart))

;; Loop through the other rows, building from the previous row
;; This assumes an initialized start with row zero filled out.
(define (fill-chart graph chart iterations)
  (do ((i 1 (1+ i)))
      ((>= i iterations))
    (begin
      (for-each
       (lambda (node)
	 (let* ((idx (character-pair->integer (graph-node-rule node)))
		(element (graph-node-element node))
		(lhs (graph-node-lhs node))
		(rhs (graph-node-rhs node))
		(lhs-idx (character-pair->integer lhs))
		(rhs-idx (character-pair->integer rhs))
		;; So, with intelligent cycle detection and explicit
		;; formulas these won't always be (1- i) But for now it's a
		;; start.
		(lhs-counts (array-ref chart lhs-idx (1- i)))
		(rhs-counts (array-ref chart rhs-idx (1- i))))
	   (begin
	     ;; Add the node letter and the left-hand side and the
	     ;; right-hand side
	     (let ((lfl (letter-freq-list-inc
			 (letter-freq-list-add lhs-counts rhs-counts)
			 element)))
	       (array-set! chart lfl idx i)))))
       graph)
      chart)))


;; Make the dynamic programming chart
;; First build the rule graph which is used to fill out the chart
;; Then create an empty subproblem chart
;; Then initialize the "bottom" row of the subproblem chart (just the
;; elements each rule creates)
;; Then fill the chart additional level.
(define (make-the-chart data)
  (let* ((graph (build-graph (peg:tree (match-pattern pip:polymer-instructions data))))
	 (chart (init-subproblem-chart graph (build-subproblem-chart 676 40))))
    (begin
      (fill-chart graph chart 40)
      chart)))

;; part1 uses the original algorithm, which was exponential, it
;; expanded into a branching binary tree for every rule.
(define (day14-part1 filename)
  (let ((data (file-get-string-all filename))
	(iterations 10))
    (let ((peg-tree (peg:tree (match-pattern pip:polymer-instructions data))))
      (let ((results (run-it peg-tree iterations)))
	(let ((most-freq (car results))
	      (least-freq (cdr results)))
	  (let ((most-freq-val (cdr most-freq))
		(least-freq-val (cdr least-freq)))
	    (display (format #f "day 14 part 1 final-value: ~a\n" (- most-freq-val least-freq-val)))))))))

;; part2 uses a dynamic programming algorithm
;; It also uses transducers to find the most and least frequent
;; elements.
(define (day14-part2 filename)
  (let ((data (file-get-string-all filename))
	(iterations 40))
    (let ((peg-tree (peg:tree (match-pattern pip:polymer-instructions data)))
	  (chart (make-the-chart data)))
      (let* ((pair-indexes (map (lambda (x) (car x)) (car (map-it peg-tree))))
	     (orig-template (cadar peg-tree))
	     (template-letter-freqs (count-letter-freq orig-template))
	     (letter-freqs
	      (fold
	       (lambda (x prev)
		 (letter-freq-list-add (array-ref chart x (1- iterations)) prev))
	       template-letter-freqs
	      pair-indexes)))
	(begin
	  (let ((my-min (box (inf)))
		(my-max (box 0)))
	    (let* ((td-result
		    (vector-transduce
		     (compose
		      (td:t-min-box my-min #t)
		      (td:t-max-box my-max #t))
		     rcons (letter-freq-list-data letter-freqs)))
		   (least-freq-val (unbox my-min))
		   (most-freq-val (unbox my-max)))
	      (display (format #f "day 14 part 2 final-value: ~a\n" (- most-freq-val least-freq-val))))))))))
