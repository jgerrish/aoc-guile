;;
;; Day 5 works with the build-overlap-map and points-covered algorithm
;; described below.  There is a second more efficient algorithm
;; outlined in this file that is incomplete.  Apologies for the work.
;; It should still provide examples to others who may want to
;; implement their own anser.
;;
;; --- Day 5: Hydrothermal Venture ---
;; You come across a field of hydrothermal vents on the ocean floor!
;; These vents constantly produce large, opaque clouds, so it would be
;; best to avoid them if possible.

;; They tend to form in lines; the submarine helpfully produces a list
;; of nearby lines of vents (your puzzle input) for you to review. For
;; example:

;; 0,9 -> 5,9
;; 8,0 -> 0,8
;; 9,4 -> 3,4
;; 2,2 -> 2,1
;; 7,0 -> 7,4
;; 6,4 -> 2,0
;; 0,9 -> 2,9
;; 3,4 -> 1,4
;; 0,0 -> 8,8
;; 5,5 -> 8,2

;; Each line of vents is given as a line segment in the format x1,y1
;; -> x2,y2 where x1,y1 are the coordinates of one end the line
;; segment and x2,y2 are the coordinates of the other end. These line
;; segments include the points at both ends. In other words:

;; An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
;; An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
;; For now, only consider horizontal and vertical lines: lines where
;; either x1 = x2 or y1 = y2.

;; So, the horizontal and vertical lines from the above list would
;; produce the following diagram:

;; .......1..
;; ..1....1..
;; ..1....1..
;; .......1..
;; .112111211
;; ..........
;; ..........
;; ..........
;; ..........
;; 222111....

;; In this diagram, the top left corner is 0,0 and the bottom right
;; corner is 9,9. Each position is shown as the number of lines which
;; cover that point or . if no line covers that point. The top-left
;; pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row
;; is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.

;; To avoid the most dangerous areas, you need to determine the number
;; of points where at least two lines overlap. In the above example,
;; this is anywhere in the diagram with a 2 or larger - a total of 5
;; points.

;; Consider only horizontal and vertical lines. At how many points do
;; at least two lines overlap?

;; --- Part Two ---
;; Unfortunately, considering only horizontal and vertical lines doesn't give you the full picture; you need to also consider diagonal lines.

;; Because of the limits of the hydrothermal vent mapping system, the lines in your list will only ever be horizontal, vertical, or a diagonal line at exactly 45 degrees. In other words:

;; An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
;; An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.
;; Considering all lines from the above example would now produce the following diagram:

;; 1.1....11.
;; .111...2..
;; ..2.1.111.
;; ...1.2.2..
;; .112313211
;; ...1.2....
;; ..1...1...
;; .1.....1..
;; 1.......1.
;; 222111....
;; You still need to determine the number of points where at least two lines overlap. In the above example, this is still anywhere in the diagram with a 2 or larger - now a total of 12 points.

;; Consider all of the lines. At how many points do at least two lines overlap?

;; This file contains the outlines of two solutions, an inefficient but correct one that
;; does interpolation and computes every line point and does a point by point comparison.
;; This version works and outputs an answer.
;; The second solution computes line start and end points and would use a spin-lock 'esque
;; counter solution.  The outline is there but it doesn't compute an answer yet.
(define-module (aoc y2021 day5 day5)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-69)
  #:use-module (ice-9 regex)
  #:use-module (aoc port)
  #:use-module (aoc main)
  #:export (parse-line parse-lines filter-horizontal-vertical-lines
		       categorize-line categorize-lines extend points-covered
		       flatten-one-level build-overlap-map count-overlaps
		       day5-part1 day5-part2))

;; Parse a list of lines in the form:
;; "0,9 -> 5,9"
;; These are in the format: x1,y1 -> x2,y2
;; Return a list of lists: ((x1 y1) (x2 y2))
(define (parse-line line)
  (let ((line-match (string-match "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)" line)))
    (if line-match
	(let ((x1 (string->number (match:substring line-match 1)))
	      (y1 (string->number (match:substring line-match 2)))
	      (x2 (string->number (match:substring line-match 3)))
	      (y2 (string->number (match:substring line-match 4))))
	  (list (list x1 y1) (list x2 y2)))
	'())))

;; Parse a list of lines in the form:
;; ("0,9 -> 5,9")
(define (parse-lines lines)
  (map parse-line lines))

;; Filter out non horizontal or vertical lines 
(define (filter-horizontal-vertical-lines lines)
  (filter
   (lambda (line)
     (let ((x1 (caar line))
	   (y1 (cadar line))
	   (x2 (caadr line))
	   (y2 (cadadr line)))
       (or (eq? x1 x2) (eq? y1 y2))))
   lines))


;; categorize a line as 'horiontal, 'vertical, 'diagonal-forward, 'diagonal-backward
;; or 'invalid
;; In addition, only return one "line index" and a list "start" and "end" value for a line
;; For a horizontal line, the line index is the y value (y1 and y2 are equal)
;; This is calculated by finding the "first" point in the line, depending on the category
;; For example, a horizontal line the "start" point would be considered the smallest x value
;; For a vertical line, it would be the smallest y value
;; The "end" value is the other x or y value, the greater one
;; For diagonal lines, the "line index" is arbitrary but should be consistent
;; here it will be set to the difference between the x1 and y1 value
;; (negative and positive values are non-overlapping distinct lines)
;; TODO: Maybe transform diagonals to a straight line
(define (categorize-line line)
  (let ((x1 (caar line))
	(y1 (cadar line))
	(x2 (caadr line))
	(y2 (cadadr line)))
    (let ((comparator (lambda (x y) (if (< x y) (list x y) (list y x)))))
      (cond
       ((equal? x1 x2)
	(list 'vertical x1 (comparator y1 y2)))
       ((equal? y1 y2)
	(list 'horizontal y1 (comparator x1 x2)))
       ((equal? (abs (- x2 x1)) (abs (- y2 y1)))
	(if (< x2 x1)
	    ;;
	    (list 'diagonal-forward (- x1 y1) (comparator y1 y2))
	    (list 'diagonal-backward (- x1 y1) (comparator y1 y2))))
       (list 'invalid #f (list #f #f))))))

;; Now do something like a radix sort, iterating through the result of categorize-line,
;; putting all the horizontal lines in one bin, vertical in another, etc.
;; Then put all the horizontal lines on the same row in the same sub-bin, etc.
;; Then step through the start and end points, increasing a counter for every start and
;; decreasing for every stop


;; calculate point intersections
;;  x
;; xxxxx
;;  x
;;  x
;; ((1 0) (1 3))
;; ((0 1) (4 1))
;; min(l1_x) <= l2_x? && max(l1_x) >= l1_x?
;; min(l2_y) <= l1_y? && max(l2_y) >= l1_y?
;;
;; calculate diagonal
;; x
;;  x  x
;;   x  x
;;    x
;;  x
;;   x
;; ((0 0) (4 4))
;; ((4 1) (5 2))
;; ((1 4) (2 5))

;; sort lines into three different categories, horizontal, vertical and diagonal
;; returns a hash table with four category keys, the values are
;; lists of lines in that category
(define (categorize-lines lines)
  (let ((categorized-lines (make-hash-table)))
    (map
     (lambda (line)
       (let ((category (categorize-line line)))
	 (hash-table-update!
	  ;; the hash table to update
	  categorized-lines
	  ;; the key
	  (car category)
	  ;; the modifier function to update the value with
	  (lambda (lines)
	    (cons line lines))
	  ;; A thunk if key isn't in the hash table
	  (lambda ()
	    ;; (list line)
	    '()
	    ))))
     lines)
    categorized-lines))


(define (update-start-end-points-table start-points-table end-points-table value)
  (begin
    ;; (display (format #f "adding value: ~a\n" value))
    (hash-table-update!
     ;; the hash table to update
     start-points-table
     ;; the key
     (car value)
     ;; the modifier function to update the value with
     (lambda (start-point-count) (1+ start-point-count))
     ;; A thunk if the key isn't in the hash table
     (lambda () 0))
    (hash-table-update!
     ;; the hash table to update
     end-points-table
     ;; the key
     (cadr value)
     ;; the modifier function to update the value with
     (lambda (end-point-count) (1+ end-point-count))
     ;; A thunk if the key isn't in the hash table
     (lambda () 0))
    (list start-points-table end-points-table)))

(define (update-row row-table value)
  (begin
    ;; (display (format #f "adding value: ~a\n" value))
    (hash-table-update!
     ;; the hash table to update
     row-table
     ;; the key
     (car value)
     ;; the modifier function to update the value with
     (lambda (tables)
       (update-start-end-points-table (car tables) (cadr tables) (cadr value)))
     ;; A thunk if the key isn't in the hash table
     (lambda ()
       ;; hash table with the "row" a line is in (or "column" or whatever mapping)
       (list (make-hash-table) (make-hash-table))))
    row-table))

(define (update-table direction-table line)
  (begin
    ;; (display (format #f "adding line: ~a\n" line))
    (hash-table-update!
     ;; the hash table to update
     direction-table
     ;; the key
     (car line)
     ;; the modifier function to update the value with
     (lambda (row-table) (update-row row-table (cdr line)))
     ;; A thunk if the key isn't in the hash table
     (lambda ()
       ;; hash table with the "row" a line is in (or "column" or whatever mapping)
       (make-hash-table)))
    direction-table))

;; This builds a multi-level hash table with counts for line start points and end points
;; The first level key is the directions (horizontal, vertical, etc.)
;; The second level key is the "row" or "category" id
;; The third level is the line start and end points
(define (build-overlaps-new categorized-lines)
  (let (;; hash table with directions ('horizontal, etc.)
	(direction-table (make-hash-table))
	;; hash table with the "row" a line is in (or "column" or whatever mapping)
	;; (row-table (make-hash-table))
	;; hash table with the start and end points
	;; (start-end-points (make-hash-table))
	;; hash table with overlaps
	(overlaps (make-hash-table)))
    (map
     (lambda (line)
       (begin
	 ;; (display (format #f "direction-table: ~a\n" direction-table))
	 (update-table direction-table line)))
     categorized-lines)
    direction-table))
;;    overlaps))

;; walk both lists at the same time, taking the least element, until finished
;; keep a counter of how many lines are active, everytime an element is taken from the
;; first list, increase it, everytime an element is taken from the last list, decrement it
;; calculate the difference between points to find the number of overlapping points in that
;; range, multiply it by the counter
;; (sort (hash-table-keys (second (hash-table-ref (hash-table-ref my-direction-table 'horizontal) 9))) <)
;; (sort (hash-table-keys (first (hash-table-ref (hash-table-ref my-direction-table 'horizontal) 9))) <)
;; This iterates through but doesn't compute the solution.  I need a real break.
(define (count-overlaps-new categorized-lines)
  (let* ((overlaps (build-overlaps-new categorized-lines))
	 (categories (list 'horizontal 'vertical))
	 (category-maps
	  (map (lambda (category) (hash-table-ref overlaps category)) categories)))
    (map
     (lambda (category)
       (begin
	 (let ((sections (hash-table-keys category)))
	   (map
	    (lambda (section)
	      (let* ((start-and-end-tables (hash-table-ref category section))
		     (start-table (first start-and-end-tables))
		     (start-points (hash-table-keys start-table))
		     (end-table (second start-and-end-tables))
		     (end-points (hash-table-keys end-table)))
		(display (format #f "start-points: ~a\n" start-points))
		(display (format #f "end-points: ~a\n" end-points))))
	    sections))))
     category-maps)))

;; v1 only needs straight lines
;; extend a single point out to a line of length len
(define (extend points len)
  (make-list len (car points)))

;; Return the set of points covered by a line
;; For example, 1,1 -> 1,3 covers 1,1 1,2 and 1,3
;; expects line in format ((x1 y2) (x2 y2))
(define (points-covered line)
  (let ((x1 (caar line))
	(y1 (cadar line))
	(x2 (caadr line))
	(y2 (cadadr line)))
    (let ((x-covered
	   ;; The line slope can be negative and x1 greater or less than x2
	   (if (< x1 x2)
	       (iota (1+ (- x2 x1)) x1)
	       (iota (1+ (- x1 x2)) x1 -1)))
	  (y-covered
	   ;; The line slope can be negative and x1 greater or less than y2
	   (if (< y1 y2)
	       (iota (1+ (- y2 y1)) y1)
	       (iota (1+(- y1 y2)) y1 -1))))
      ;; "stretch" the shortest line, interpolating values in between,
      ;; rounding the interpolated values to nearest integer
      (begin
	;; (display (format #f "x-covered: ~a\n" x-covered))
	;; (display (format #f "y-covered: ~a\n" y-covered))
	(let* ((x-covered-length (length x-covered))
	       (y-covered-length (length y-covered))
	       (x-covered
		(if (< x-covered-length y-covered-length)
		    (extend x-covered y-covered-length)
		    x-covered))
	       (y-covered
		(if (> x-covered-length y-covered-length)
		    (extend y-covered x-covered-length)
		    y-covered)))
	  (begin
	    ;; (display (format #f "x-covered: ~a\n" x-covered))
	    ;; (display (format #f "y-covered: ~a\n" y-covered))
	    (zip x-covered y-covered)))))))

;; non-points covered algorithm
;; instead of generating a list of every point covered and matching them
;; split up the lines into three classes:
;;   horizontal, vertical and both diagonal directions
;;   deal with the horizontal first, split these up into sets based on which row they are on
;;     count overlaps for item on the same row based on start and end point
;;     sort those start and end points
;;     simple version saves every "entry" and "exit" point for the lines on the grid in a hashtable
;;     i.e., for horizontal lines, two lines start at (0 0), so (0 0) -> ((line1 line4) ())
;;     line1 ends at (3 0), line4 ends at (5 0), so (3 0) -> (() (line1)), (5 0) -> ((() (line4)))
;;     simple version iterates through the grid, complex veresion uses a hashtable with an ordered key
;;     implementation or augmented data structure
;;     repeat for all the rows
;;   Do the same for vertical and diagonal lines
;;   The other case is for intersection points.  There will be at most one point for any
;;   two types of line not in the same class, simple math with the start and end point should
;;   work.

;; flatten list by one level
(define (flatten-one-level lst)
  (apply append lst))

;; Build an overlap map from a list of points covered
(define (build-overlap-map covered-points)
  ;; figure out a good value for number, either through a rough estimate or stats
  (let ((counts (make-hash-table)))
    (fold
     (lambda (elem prev)
       (begin
	 (if (hash-table-exists? prev elem)
	     (let ((val (hash-table-ref prev elem)))
	       (hash-table-set! prev elem (1+ val)))
	     (hash-table-set! prev elem 1))
	 prev))
     counts
     covered-points)))

;; count the number of overlaps in the overlap map greater than the limit
(define (count-overlaps overlap-map limit)
  (length (filter (lambda (value) (> value limit)) (hash-table-values overlap-map))))
  ;; (hash-count (lambda (key value) (> value limit)) overlap-map))

;; day5 part 1
(define (day5-part1 filename)
  (let ((file-lines (get-lines filename)))
    (let* ((lines (filter-horizontal-vertical-lines (parse-lines file-lines)))
	   (points-covered (flatten-one-level (map points-covered lines)))
	   (overlap-map (build-overlap-map points-covered))
	   (overlap-counts (count-overlaps overlap-map 1)))
      (display (format #f "day 5 part 1 final-value: ~a\n" overlap-counts)))))

;; day5 part 2
(define (day5-part2 filename)
  (let ((file-lines (get-lines filename)))
    (let* ((lines (parse-lines file-lines))
	   (points-covered (flatten-one-level (map points-covered lines)))
	   (overlap-map (build-overlap-map points-covered))
	   (overlap-counts (count-overlaps overlap-map 1)))
      (display (format #f "day 5 part 2 final-value: ~a\n" overlap-counts)))))
