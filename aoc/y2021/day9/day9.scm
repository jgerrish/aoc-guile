;; --- Day 9: Smoke Basin ---

;; These caves seem to be lava tubes. Parts are even still
;; volcanically active; small hydrothermal vents release smoke into
;; the caves that slowly settles like rain.

;; If you can model how the smoke flows through the caves, you might
;; be able to avoid it and be that much safer. The submarine generates
;; a heightmap of the floor of the nearby caves for you (your puzzle
;; input).

;; Smoke flows to the lowest point of the area it's in. For example,
;; consider the following heightmap:

;; 2199943210
;; 3987894921
;; 9856789892
;; 8767896789
;; 9899965678
;; Each number corresponds to the height of a particular location,
;; where 9 is the highest and 0 is the lowest a location can be.

;; Your first goal is to find the low points - the locations that are
;; lower than any of its adjacent locations. Most locations have four
;; adjacent locations (up, down, left, and right); locations on the
;; edge or corner of the map have three or two adjacent locations,
;; respectively. (Diagonal locations do not count as adjacent.)

;; In the above example, there are four low points, all highlighted:
;; two are in the first row (a 1 and a 0), one is in the third row (a
;; 5), and one is in the bottom row (also a 5). All other locations on
;; the heightmap have some lower adjacent location, and so are not low
;; points.

;; The risk level of a low point is 1 plus its height. In the above
;; example, the risk levels of the low points are 2, 1, 6, and 6. The
;; sum of the risk levels of all low points in the heightmap is
;; therefore 15.

;; Find all of the low points on your heightmap. What is the sum of
;; the risk levels of all low points on your heightmap?

;; --- Part Two ---
;; Next, you need to find the largest basins so you know what areas
;; are most important to avoid.

;; A basin is all locations that eventually flow downward to a single
;; low point. Therefore, every low point has a basin, although some
;; basins are very small. Locations of height 9 do not count as being
;; in any basin, and all other locations will always be part of
;; exactly one basin.

;; The size of a basin is the number of locations within the basin,
;; including the low point. The example above has four basins.

;; The top-left basin, size 3:

;; 2199943210
;; 3987894921
;; 9856789892
;; 8767896789
;; 9899965678
;; The top-right basin, size 9:

;; 2199943210
;; 3987894921
;; 9856789892
;; 8767896789
;; 9899965678
;; The middle basin, size 14:

;; 2199943210
;; 3987894921
;; 9856789892
;; 8767896789
;; 9899965678
;; The bottom-right basin, size 9:

;; 2199943210
;; 3987894921
;; 9856789892
;; 8767896789
;; 9899965678
;; Find the three largest basins and multiply their sizes together. In
;; the above example, this is 9 * 14 * 9 = 1134.

;; What do you get if you multiply together the sizes of the three
;; largest basins?



;; Day 9 Part 1 notes
;; Initial algorithm thought:
;; Just iterate through every location, checking each neighbor.  if
;; there is a neighbor that is lower, move on.  If it's the lowest
;; area, return it or append it to the solution list.
;;
;; Functions required;
;; Given location index, return neighbors, including edge rules.
;; Iterator function that walks through the map.



;; Day 9 Part 2 notes
;; Algorithm:
;; Iterate through every location:

;; Do a DFS or BFS, where a path or sub-path is valid from the
;; starting point to a neighbor point if that neighbor point is less
;; than the current point (verify any lesser value, not just one less)
;; and the current point is not depth 9.

;; Find all paths from the current point.  Collect them all and index
;; the paths by the point they end at.  Deduplicate them (may be done
;; earlier in the search) (several ways to do this, any path with a
;; point p < 9 is in the same basin.)  A set or hashtable data
;; structure from the DFS or BFS may be able to be re-used.

(define-module (aoc y2021 day9 day9)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 exceptions)
  #:use-module ((rnrs hashtables) :version (6))	; SRFI-6 hash tables
  #:use-module ((rnrs sorting) :version (6))
  #:use-module (aoc port)
  #:use-module (aoc main)
  #:use-module (aoc util)
  #:use-module (aoc array)
  #:export (parse-line parse-lines find-min-spots risk-levels day9-part1
		       higher-or-equal-neighbors find-basin day9-part2))

;; Convert a single line from the input into a list of integers from 0-9
(define (parse-line line)
  (map (lambda (c) (char->number c)) (string->list line)))

;; Parse the input data into an array

;; This converts a list of lines like this (for a 3x3 grid):
;; '("123" "456" "789") into this: #2((1 2 3) (4 5 6) (7 8 9))
;; The array it returns can be accessed like this:
;; (array-ref #2((1 2 3) (4 5 6) (7 8 9)) 0 1)
;; This will return 6.  This is the first line (first index 0),
;; second item (second index 1)
;;
;; coordinates take this form: (row column)
;;
(define (parse-lines lines)
    (if (null? lines)
	#2()
	(let ((str-length (string-length (car lines)))
	      (num-lines (length lines))
	      (parsed-data (map parse-line lines)))
	  (list->array 2 parsed-data))))

;; Find the minimum spots in the map
(define (find-min-spots arr)
  (let ((dimensions (array-dimensions arr)))
    (let ((dst (make-array 2 (car dimensions) (cadr dimensions)))
	  (i1-max (1- (car dimensions)))
	  (i2-max (1- (cadr dimensions))))
      (let find-min-spot-helper ((i 0) (j 0) (result '()))
	(if (or (> i i1-max) (> j i2-max))
	    result
	    (let ((neighbors (find-neighbors i j i1-max i2-max)))
	      (let ((min-value
	             (apply
		      min
		      (map
		       (lambda (x) (array-ref arr (car x) (cadr x)))
		       neighbors))))
		(let ((res (if (<= min-value (array-ref arr i j))
			       result
			       (cons (list i j) result))))
		  (if (>= j i2-max)
		      (find-min-spot-helper (1+ i) 0 res)
		      (find-min-spot-helper i (1+ j) res))))))))))

;; Find the minimum spots in the map
(define (find-indexes arr)
  (let ((dimensions (array-dimensions arr)))
    (let ((dst (make-array 2 (car dimensions) (cadr dimensions)))
	  (i1-max (1- (car dimensions)))
	  (i2-max (1- (cadr dimensions))))
      (let find-indexes-helper ((i i1-max) (j i2-max) (result '()))
	(if (and (= i 0) (< j 0))
	    result
	    (if (< j 0)
		(find-indexes-helper (1- i) i2-max result)
		(find-indexes-helper i (1- j) (cons (list i j) result))))))))

;; Calculate the risk levels for the low spots
;; arr is the array
;; spots is a list of low points
(define (risk-levels arr spots)
  (fold
   (lambda (x prev) (+ x prev 1))
   0
   (map (lambda (x) (array-ref arr (car x) (cadr x))) spots)))

;; Compute part 1 for day 9
(define (day9-part1 filename)
  (let ((lines (parse-lines (get-lines filename))))
    (let ((rl (risk-levels lines (find-min-spots lines))))
      (begin
	(display (format #f "day 9 part 1 final-value: ~a\n" rl))))))


;; Day 2 code

;; Get the neigbors that are shallower or equal to the current cell
;; Given a depth value and list of neighbors, filter the neighbors to
;; those that are higher than or equal to the depth.
;; This doesn't include neighbors that have a depth of 9
(define (higher-or-equal-neighbors arr depth neighbors)
  (filter
   (lambda (neighbor)
     (let ((val (array-ref arr (first neighbor) (second neighbor))))
       (and (>= val depth) (not (= val 9)))))
   neighbors))

;; Check whether an item has been visited
(define (visited? visited item)
  (hashtable-contains? visited item))

;; Mark an item as visited
(define (mark-visited visited item)
  (hashtable-set! visited item #t))

;; Given a cave and a low-point, find all cells in the basin
;; associated with that low-point.
(define (find-basin arr low-point)
  (let* ((dimensions (array-dimensions arr))
	 (i1-max (1- (car dimensions)))
	 (i2-max (1- (cadr dimensions))))
    (let find-basin-helper ((stack (list low-point))
			    (visited (make-hashtable (lambda (x) (hash x 10000)) equal?)))
      (if (null? stack)
	  ;; Return the visited items if we're done
	  (hashtable-keys visited)
	  ;; Keep searching
	  ;; get an item from the queue and search it
	  (let ((item (car stack)))
	    (if (visited? visited item)
		(find-basin-helper (cdr stack) visited)
		;; not visited
		(let ((neighbors
		       (higher-or-equal-neighbors
			arr
			(array-ref arr (first item) (second item))
			(find-neighbors
			 (first item) (second item) i1-max i2-max))))
		  (begin
		    ;; Add to visited
		    (mark-visited visited item)
		    (find-basin-helper
		     (append neighbors (cdr stack)) visited)))))))))


;; Compute part 2 for day 9
(define (day9-part2 filename)
  (let ((lines (parse-lines (get-lines filename))))
    (display
     (format #f "day 9 part 2 final-value: ~a\n"
	     (reduce * 1
		     (take
		      (list-sort
		       (lambda (a b) (> a b))
		       (map
			(lambda (low-point) (vector-length (find-basin lines low-point)))
			(find-min-spots lines)))
		      3))))))
