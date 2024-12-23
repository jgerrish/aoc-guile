;; --- Day 12: Passage Pathing ---

;; With your submarine's subterranean subsystems subsisting
;; suboptimally, the only way you're getting out of this cave anytime
;; soon is by finding a path yourself. Not just a path - the only way
;; to know if you've found the best path is to find all of them.

;; Fortunately, the sensors are still mostly working, and so you build
;; a rough map of the remaining caves (your puzzle input). For
;; example:

;; start-A
;; start-b
;; A-c
;; A-b
;; b-d
;; A-end
;; b-end

;; This is a list of how all of the caves are connected. You start in
;; the cave named start, and your destination is the cave named
;; end. An entry like b-d means that cave b is connected to cave d -
;; that is, you can move between them.

;; So, the above cave system looks roughly like this:

;;     start
;;     /   \
;; c--A-----b--d
;;     \   /
;;      end


;; Your goal is to find the number of distinct paths that start at
;; start, end at end, and don't visit small caves more than
;; once. There are two types of caves: big caves (written in
;; uppercase, like A) and small caves (written in lowercase, like
;; b). It would be a waste of time to visit any small cave more than
;; once, but big caves are large enough that it might be worth
;; visiting them multiple times. So, all paths you find should visit
;; small caves at most once, and can visit big caves any number of
;; times.

;; Given these rules, there are 10 paths through this example cave
;; system:

;; start,A,b,A,c,A,end
;; start,A,b,A,end
;; start,A,b,end
;; start,A,c,A,b,A,end
;; start,A,c,A,b,end
;; start,A,c,A,end
;; start,A,end
;; start,b,A,c,A,end
;; start,b,A,end
;; start,b,end


;; (Each line in the above list corresponds to a single path; the
;; caves visited by that path are listed in the order they are visited
;; and separated by commas.)


;; Note that in this cave system, cave d is never visited by any path:
;; to do so, cave b would need to be visited twice (once on the way to
;; cave d and a second time when returning from cave d), and since
;; cave b is small, this is not allowed.

;; Here is a slightly larger example:

;; dc-end
;; HN-start
;; start-kj
;; dc-start
;; dc-HN
;; LN-dc
;; HN-end
;; kj-sa
;; kj-HN
;; kj-dc

;; The 19 paths through it are as follows:

;; start,HN,dc,HN,end
;; start,HN,dc,HN,kj,HN,end
;; start,HN,dc,end
;; start,HN,dc,kj,HN,end
;; start,HN,end
;; start,HN,kj,HN,dc,HN,end
;; start,HN,kj,HN,dc,end
;; start,HN,kj,HN,end
;; start,HN,kj,dc,HN,end
;; start,HN,kj,dc,end
;; start,dc,HN,end
;; start,dc,HN,kj,HN,end
;; start,dc,end
;; start,dc,kj,HN,end
;; start,kj,HN,dc,HN,end
;; start,kj,HN,dc,end
;; start,kj,HN,end
;; start,kj,dc,HN,end
;; start,kj,dc,end

;; Finally, this even larger example has 226 paths through it:

;; fs-end
;; he-DX
;; fs-he
;; start-DX
;; pj-DX
;; end-zg
;; zg-sl
;; zg-pj
;; pj-he
;; RW-he
;; fs-DX
;; pj-RW
;; zg-RW
;; start-pj
;; he-WI
;; zg-he
;; pj-fs
;; start-RW

;; How many paths through this cave system are there that visit small
;; caves at most once?

;; --- Part Two ---

;; After reviewing the available paths, you realize you might have
;; time to visit a single small cave twice. Specifically, big caves
;; can be visited any number of times, a single small cave can be
;; visited at most twice, and the remaining small caves can be visited
;; at most once. However, the caves named start and end can only be
;; visited exactly once each: once you leave the start cave, you may
;; not return to it, and once you reach the end cave, the path must
;; end immediately.

;; Now, the 36 possible paths through the first example above are:

;; start,A,b,A,b,A,c,A,end
;; start,A,b,A,b,A,end
;; start,A,b,A,b,end
;; start,A,b,A,c,A,b,A,end
;; start,A,b,A,c,A,b,end
;; start,A,b,A,c,A,c,A,end
;; start,A,b,A,c,A,end
;; start,A,b,A,end
;; start,A,b,d,b,A,c,A,end
;; start,A,b,d,b,A,end
;; start,A,b,d,b,end
;; start,A,b,end
;; start,A,c,A,b,A,b,A,end
;; start,A,c,A,b,A,b,end
;; start,A,c,A,b,A,c,A,end
;; start,A,c,A,b,A,end
;; start,A,c,A,b,d,b,A,end
;; start,A,c,A,b,d,b,end
;; start,A,c,A,b,end
;; start,A,c,A,c,A,b,A,end
;; start,A,c,A,c,A,b,end
;; start,A,c,A,c,A,end
;; start,A,c,A,end
;; start,A,end
;; start,b,A,b,A,c,A,end
;; start,b,A,b,A,end
;; start,b,A,b,end
;; start,b,A,c,A,b,A,end
;; start,b,A,c,A,b,end
;; start,b,A,c,A,c,A,end
;; start,b,A,c,A,end
;; start,b,A,end
;; start,b,d,b,A,c,A,end
;; start,b,d,b,A,end
;; start,b,d,b,end
;; start,b,end

;; The slightly larger example above now has 103 paths through it, and
;; the even larger example now has 3509 paths through it.

;; Given these new rules, how many paths through this cave system are
;; there?
(define-module (aoc y2021 day12 day12)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 peg)
  #:use-module ((rnrs hashtables) :version (6))
  #:use-module (aoc util)
  #:use-module (aoc port)
  #:use-module (aoc main)
  #:use-module (aoc array)
  #:export (make-cave string->cave cave-eq? cave-hash
		      big-cave? small-cave? start-cave? end-cave?
		      make-edge edge-eq? edge-hash easy-make-edge
		      make-visited-caves
		      parse-line parse-lines init
		      hashtable-copy-with-new-item hashtable-copy-with-inc
		      init visit-cave-part1? visit-cave-part2? search-helper
		      path-tree->path-list
		      day12-part1 day12-part2))


(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (ice-9 exceptions))
(use-modules ((rnrs hashtables) :version (6)))
(use-modules (aoc util))
(use-modules (aoc port))
(use-modules (aoc main))
(use-modules (aoc array))

;; Previous days used strings as keys in hashtables
;;
;; Strings aren't unique in Scheme, even if they have the same exact
;; data.
;; Symbols are unique and can be compared for equality with the eq?
;; operator.  This is more efficient.
;;
;; But this is complicated by the fact that the procedures
;; symbol->string and string-ci->symbol may or may not preserve the
;; case of the characters in the string (see Section 6.6.6.4
;; Operations Related to Symbols in the Guile manual).  R5RS specifies
;; that symbols should be read case-insensitively, but by default
;; Guile reads symbols case-sensitively.
;;
;; Using string->symbol WILL always preserve the case, but it may lead
;; to confusion if developers come from a different Scheme and expect
;; symbols to be case-insensitive.
;;
;; So be aware using symbols in day12 may be introducing cognitive
;; load to other developers.
;;
;; The other solution is a more complicated data structure to store
;; cave information instead of a simple #t or #f in the edge graph.
;; So I went with that.
(define-record-type <cave>
  (make-cave name key size start end)
  cave?
  (name cave-name)
  (key cave-key)
  (size cave-size)
  (start cave-start?)
  (end cave-end?))

(define (string->cave name)
  (let ((sym (string->symbol name)))
    (make-cave
     name
     sym
     ;; The start and end rooms aren't technically "big" or "small",
     ;; So this sets them as "special"
     (if (or (eq? sym 'start) (eq? sym 'end))
	 'special
	 (if (char-lower-case? (string-ref name 0))
	     'small
	     'big))
     (eq? sym 'start)
     (eq? sym 'end))))


(define (cave-eq? cave-1 cave-2)
  (eq? (cave-key cave-1) (cave-key cave-2)))

(define (cave-hash cave)
  (symbol-hash (cave-key cave)))

(define (big-cave? cave)
  (eq? (cave-size cave) 'big))

(define (small-cave? cave)
  (eq? (cave-size cave) 'small))

(define (start-cave? cave)
  (eq? (cave-key cave) 'start))

(define (end-cave? cave)
  (eq? (cave-key cave) 'end))


(define-record-type <edge>
  (make-edge src dst key)
  edge?
  (src edge-src)
  (dst edge-dst)
  (key edge-key))


(define (edge-eq? edge-1 edge-2)
  (and
   (eq? (edge-src edge-1) (edge-src edge-2))
   (eq? (edge-dst edge-1) (edge-dst edge-2))))


;; Add some additional structure to visited-caves for part2
;; We'd like to know at a glance if any small cave has been visited
;; twice
;; So visited caves has two fields now, the visited caves hash and a
;; boolean indicating whether a small cave has been visited twiced.
(define-record-type <visited-caves>
  (make-visited-caves caves small-cave-visited-twice)
  visited-caves?
  (caves visited-caves-caves)
  (small-cave-visited-twice visited-caves-small-cave-visited-twice?))

;; equal-hash won't work for edges
;;
;; It corectly computes hashes for the fields in the edge record, src
;; and dst, but it doesn't take into account the order of the fields.
;; (define (edge-hash edge)
;;   (equal-hash edge))
;;
;; So instead we create a new symbol by concatenating the src and dst
;; with a hyphen in the middle.
;;
;; The symbol has to be set on record creation.
;; An additional function called easy-make-edge is provided to create
;; the record.
(define (edge-hash edge)
  (symbol-hash (edge-key edge)))

;; Easy maker
(define (easy-make-edge src dst)
  (let ((key (symbol-append
	      (symbol-append (cave-key src) (symbol #\-))
	      (cave-key dst))))
    (make-edge src dst key)))


(define (parse-line line)
  (string-tokenize line char-set:letter+digit))

(define (parse-lines lst)
  ;; parse the edge list into edges
  (map (lambda (x) (parse-line x)) lst))


(define test-data "start-A start-b A-c A-b b-d A-end b-end")
(define test-data-2 "dc-end HN-start start-kj dc-start dc-HN LN-dc HN-end kj-sa kj-HN kj-dc")
(define test-data-3 "fs-end he-DX fs-he start-DX pj-DX end-zg zg-sl zg-pj pj-he RW-he fs-DX pj-RW zg-RW start-pj he-WI zg-he pj-fs start-RW")

(define (add-edge ht edge)
  (let ((src-cave (edge-src edge))
	(dst-cave (edge-dst edge)))
  (hashtable-update! ht src-cave
		     (lambda (edges-ht)
		       (begin
			 (hashtable-set! edges-ht dst-cave dst-cave)
			 edges-ht))
		     (make-hashtable cave-hash cave-eq?))))

(define (init data)
  (let ((caves (make-hashtable cave-hash cave-eq?))
	(edges (parse-lines data))
	(edge-graph (make-hashtable cave-hash cave-eq?))
	(start-cave #f))
    (begin
      (for-each
       (lambda (edge)
	 (let ((src-cave (string->cave (first edge)))
	       (dst-cave (string->cave (second edge))))
	   (begin
	     (if (not (hashtable-contains? caves src-cave))
		 (hashtable-set! caves src-cave src-cave))
	     (if (not (hashtable-contains? caves dst-cave))
		 (hashtable-set! caves dst-cave dst-cave))
	     (if (cave-start? src-cave)
		 (set! start-cave src-cave))
	     (if (cave-start? dst-cave)
		 (set! start-cave dst-cave))
	     (add-edge edge-graph (easy-make-edge src-cave dst-cave))
	     (add-edge edge-graph (easy-make-edge dst-cave src-cave)))))
       edges)
      (list caves edge-graph start-cave))))


(define (visit-cave-part1?
	 visited-caves visited-paths src cave-to-visit)
  (let ((small-visited-twice?
	 (visited-caves-small-cave-visited-twice? visited-caves)))
    (cond
     ((start-cave? cave-to-visit)
      (cons #f small-visited-twice?))
     ((end-cave? cave-to-visit)
      (cons #t small-visited-twice?))
     ((big-cave? cave-to-visit)
      (cons #t small-visited-twice?))
     ((small-cave? cave-to-visit)
      (let ((visited? (hashtable-contains?
		       (visited-caves-caves visited-caves)
 		       cave-to-visit)))
	(if visited?
	    ;; We have visited this small cave before
	    (cons #f  small-visited-twice?)
	    ;; We have not visited this cave before
	    (cons #t small-visited-twice?))))
     (else
      (display (format #f "else wtf?\n"))))))


;; This code mixes small-visited-twice? as the cdr to return and
;; constant #t and #f values.  We could be consistent and use constant
;; values everywhere.  But using the existing small-visited-twice? can
;; indicate that the value is independent of the current code path.
(define (visit-cave-part2?
	 visited-caves visited-paths src cave-to-visit)
  (let ((small-visited-twice?
	 (visited-caves-small-cave-visited-twice? visited-caves)))
    (cond
     ((start-cave? cave-to-visit)
      (cons #f small-visited-twice?))
     ((end-cave? cave-to-visit)
      (cons #t small-visited-twice?))
     ((big-cave? cave-to-visit)
      (cons #t small-visited-twice?))
     ((small-cave? cave-to-visit)
      (let ((visited? (hashtable-contains?
		       (visited-caves-caves visited-caves)
 		       cave-to-visit)))
	(if small-visited-twice?
	    ;; We've visited one of the small caves twice
	    (if visited?
		(let ((num-visits (hashtable-ref
				   (visited-caves-caves visited-caves)
				   cave-to-visit
				   0)))
		  (cond
		   ((= num-visits 2)
		    ;; We've already visited a cave twice, we can't visit any
		    ;; small cave with a visit count greater than zero.
		    (cons #f small-visited-twice?))
		   ((= num-visits 1)
		    ;; Otherwise visit it
		    (cons #f small-visited-twice?))
		   ((= num-visits 0)
		    ;; should never happen
		    (begin
		      (display (format #f "should never have this value: ~a\n" num-visits))
		      (cons #t small-visited-twice?)))
		   (else
		    ;; should never happen
		    (begin
		      (display (format #f "should never have this value: ~a\n" num-visits))
		      (cons #f small-visited-twice?)))))
		;; We haven't visited this cave yet
		(cons #t small-visited-twice?))
	    ;; We haven't visited one of the small caves twice
	    (if visited?
		(let ((num-visits (hashtable-ref
				   (visited-caves-caves visited-caves)
				   cave-to-visit
				   0)))
		  (cond
		   ((= num-visits 0)
		    ;; should never happen
		    (begin
		      (display (format #f "should never have this value: ~a\n" num-visits))
		      (cons #t #t)))
		   ((= num-visits 1)
		    ;; We're visiting a small cave for the second time, set
		    ;; the small-visited-twice? flag
		    (cons #t #t))
		   ((= num-visits 2)
		    (cons #f small-visited-twice?))
		   (else
		    ;; should never happen
		    (begin
		      (display (format #f "should never have this value: ~a\n" num-visits))
		      (cons #f small-visited-twice?)))))
		;; We're visiting a small cave for the first time, keep
		;; the small-visited-twice? flag the same value (#f)
		(cons #t small-visited-twice?)))))
     (else
      (display (format #f "else wtf?\n"))))))


;; Create an immutable copy of a R6RS hashtable with a new item
;; Returns an immutable hashtable
(define (hashtable-copy-with-new-item ht k v)
  (let ((mutable-ht (hashtable-copy ht #t)))
    (begin
      (hashtable-set! mutable-ht k v)
      (hashtable-copy mutable-ht))))

;; Create an immutable copy of a R6RS hashtable with an item where the
;; value is incremented.
;; If the item doesn't exist, create a new item with count 1
;; Returns an immutable hashtable
(define (hashtable-copy-with-inc ht k)
  (let ((mutable-ht (hashtable-copy ht #t))
	(new-cnt (1+ (hashtable-ref ht k 0))))
    (begin
      (hashtable-set! mutable-ht k new-cnt)
      (hashtable-copy mutable-ht))))

(define (update-visited-caves visited-caves current-cave)
  (make-visited-caves
   ;; first record field, actual caves
   (if (small-cave? current-cave)
       ;; small cave logic
       ;; Previously we were using a boolean to indicate a visit
       ;; This was updated to a count for part2
       (hashtable-copy-with-inc
	(visited-caves-caves visited-caves)
	current-cave)
       ;; big cave logic
       (hashtable-copy (visited-caves-caves visited-caves)))
   ;; second record field, small cave visited twice flag
   (if (small-cave? current-cave)
       ;; small cave logic
       (if (visited-caves-small-cave-visited-twice? visited-caves)
	   ;; No need to update the flag if we've already marked
	   ;; a small cave as visited twice.
	   #t
	   ;; Otherwise, check to see if this cave has been visited
	   ;; twice
	   (if (hashtable-contains?
		(visited-caves-caves visited-caves)
		current-cave)
	       #t
	       #f))
       ;; big cave logic, just return the existing flag value
       (visited-caves-small-cave-visited-twice? visited-caves))))

(define (search-helper caves edge-graph start-cave visit-cave?)
  (let lp ((visited-caves
	    (make-visited-caves
	     (make-hashtable cave-hash cave-eq?)
	     #f))
	   (visited-paths (make-hashtable edge-hash edge-eq?))
	   (current-cave start-cave)
	   (current-path (list start-cave)))
    (if (cave-end? current-cave)
	;; Return the current path if we've reached a cave end
	(map cave-name current-path)
	;; Otherwise visit edges of the current cave
	(if (hashtable-contains? edge-graph current-cave)
	    (let* ((visited-caves
		    (update-visited-caves visited-caves current-cave))
		   (edges
		    (vector->list
		     (hashtable-keys
		      (hashtable-ref edge-graph current-cave #f))))
		   (to-visit edges))
	      (fold-right
	       (lambda (cave-to-visit prev)
		 (let ((visit-cave-result
			(visit-cave?
			 visited-caves
			 visited-paths
			 current-cave
			 cave-to-visit)))
		   (let ((visit? (car visit-cave-result))
			 (new-small-visited-twice? (cdr visit-cave-result)))
		     (if visit?
			 (let ((res
				(lp
				 (make-visited-caves
				  (hashtable-copy
				   (visited-caves-caves visited-caves))
				  new-small-visited-twice?)
				 (hashtable-copy-with-new-item
				  visited-paths
				  (easy-make-edge current-cave cave-to-visit)
				  #t)
				 cave-to-visit
				 (append current-path (list cave-to-visit)))))
			   (if (not (null? res))
			       (append prev (list res))
			       prev))
			 prev))))
	       '()
	       to-visit))
	    current-path))))

(define (path-tree->path-list tree)
  (let lp ((tree tree))
    (if (not (null? tree))
	(fold-right
	 (lambda (branch prev)
	   (if (string? (car branch))
	       (append (list branch) prev)
	       (append (lp branch) prev)))
	 '()
	 tree))))


(define (search caves edge-graph start-cave visit-cave?)
  (let* ((paths (search-helper
		     caves
		     edge-graph
		     start-cave
		     visit-cave?)))
    (path-tree->path-list paths)))

(define (run data visit-cave?)
  (begin
    (let ((init-data (init data)))
      (search (first init-data) (second init-data) (third init-data)
	      visit-cave?))))

(define (day12-part1 filename)
  (let ((data (get-lines filename)))
    (begin
      (let ((num-paths (length (run data visit-cave-part1?))))
	(display (format #f "day 12 part 1 final-value: ~a\n" num-paths))))))

(define (day12-part2 filename)
  (let ((data (get-lines filename)))
    (begin
      (let ((num-paths (length (run data visit-cave-part2?))))
	(display (format #f "day 12 part 2 final-value: ~a\n" num-paths))))))

;; The parts of the day that should get run
(define parts (list day12-part1 day12-part2))
