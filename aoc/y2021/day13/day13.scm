;; Day 13: Transparent Origami

;; You reach another volcanically active part of the cave. It would be
;; nice if you could do some kind of thermal imaging so you could tell
;; ahead of time which caves are too hot to safely enter.

;; Fortunately, the submarine seems to be equipped with a thermal
;; camera! When you activate it, you are greeted with:

;; Congratulations on your purchase! To activate this infrared thermal
;; imaging camera system, please enter the code found on page 1 of the
;; manual.

;; Apparently, the Elves have never used this feature. To your
;; surprise, you manage to find the manual; as you go to open it, page
;; 1 falls out. It's a large sheet of <a
;; href="https://en.wikipedia.org/wiki/Transparency_(projection)"
;; target="_blank">transparent paper</a>! The transparent paper is
;; marked with random dots and includes instructions on how to fold it
;; up (your puzzle input). For example:

;; 6,10
;; 0,14
;; 9,10
;; 0,3
;; 10,4
;; 4,11
;; 6,0
;; 6,12
;; 4,1
;; 0,13
;; 10,12
;; 3,4
;; 3,0
;; 8,4
;; 1,10
;; 2,14
;; 8,10
;; 9,0

;; fold along y=7
;; fold along x=5

;; The first section is a list of dots on the transparent paper. 0,0
;; represents the top-left coordinate.  The first value, x, increases
;; to the right.  The second value, y, increases downward.  So, the
;; coordinate 3,0 is to the right of 0,0, and the coordinate 0,7 is
;; below 0,0. The coordinates in this example form the following
;; pattern, where # is a dot on the paper and . is an empty, unmarked
;; position:

;; ...#..#..#.
;; ....#......
;; ...........
;; #..........
;; ...#....#.#
;; ...........
;; ...........
;; ...........
;; ...........
;; ...........
;; .#....#.##.
;; ....#......
;; ......#...#
;; #..........
;; #.#........

;; Then, there is a list of fold instructions. Each
;; instruction indicates a line on the transparent paper and wants you
;; to fold the paper up (for horizontal y=... lines) or left (for
;; vertical x=... lines). In this example, the first fold instruction
;; is fold along y=7, which designates the line formed by all of the
;; positions where y is 7 (marked here with -):

;; ...#..#..#.
;; ....#......
;; ...........
;; #..........
;; ...#....#.#
;; ...........
;; ...........
;; -----------
;; ...........
;; ...........
;; .#....#.##.
;; ....#......
;; ......#...#
;; #..........
;; #.#........

;; Because this is a horizontal line, fold the bottom half up. Some of
;; the dots might end up overlapping after the fold is complete, but
;; dots will never appear exactly on a fold line. The result of doing
;; this fold looks like this:

;; #.##..#..#.
;; #...#......
;; ......#...#
;; #...#......
;; .#.#..#.###
;; ...........
;; ...........


;; Now, only 17 dots are visible.

;; Notice, for example, the two dots in the bottom left corner before
;; the transparent paper is folded; after the fold is complete, those
;; dots appear in the top left corner (at 0,0 and 0,1). Because the
;; paper is transparent, the dot just below them in the result (at
;; 0,3) remains visible, as it can be seen through the transparent
;; paper.

;; Also notice that some dots can end up overlapping; in this case,
;; the dots merge together and become a single dot.

;; The second fold instruction is fold along x=5, which indicates this
;; line:

;; #.##.|#..#.
;; #...#|.....
;; .....|#...#
;; #...#|.....
;; .#.#.|#.###
;; .....|.....
;; .....|.....


;; Because this is a vertical line, fold left:

;; #####
;; #...#
;; #...#
;; #...#
;; #####
;; .....
;; .....


;; The instructions made a square!

;; The transparent paper is pretty big, so for now, focus on just
;; completing the first fold. After the first fold in the example
;; above, 17 dots are visible - dots that end up overlapping after the
;; fold is completed count as a single dot.

;; How many dots are visible after completing just the first fold
;; instruction on your transparent paper?

;; -- Part Two --
;;
;; Finish folding the transparent paper according to the instructions.
;; The manual says the code is always eight capital letters.
;;
;; What code do you use to activate the infrared thermal imaging
;; camera system?


;;
;; Guile module instructions for using this module
;;
;; Here is how to use this module from the Guile REPL:
;; Assuming aoc-guile is in your module load path (GUILE_LOAD_PATH or -L argument):
;;
;; scheme@(guile-user)> (use-modules (aoc y2021 day13 day13))
;; scheme@(guile-user)> ,module aoc y2021 day13 day13
;;
;; Your scheme prompt will change to show the module path.
;; Then to run functions exported by the module:
;;
;; scheme@(aoc y2021 day13 day13)> (display-sheet (run-it *my-peg-tree* 2))
;;
;; To reload:
;;
;; scheme@(aoc y2021 day13 day13)> ,reload
;;
(define-module (aoc y2021 day13 day13)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 receive)
  #:use-module ((aoc logging) :prefix logging:)
  #:use-module (aoc port)
  ;; Export a bunch of the PEG patterns for testing
  ;; Normally you wouldn't want to do that, I wanted to show
  ;; how to test the PEGs along with the code
  #:export (transparent-paper line point-line fold-line
			      axis value number
			      ;; The below are functions that convert
			      ;; PEG trees to custom data structures
			      parse-lines
			      ;; The below are the rest of the
			      ;; functions
			      sheet-array sheet-width sheet-height
			      build-sheet
			      make-x-axis-fold-mapper
			      make-y-axis-fold-mapper
			      on-x-axis-fold-line? on-y-axis-fold-line?
			      fold-sheet day13-part1 day13-part2
			      ))

;; Initialize the logger
(logging:log-init 'DEBUG)

;; Use a Parsing Expression Grammar (PEG) parser to parse the input
;; data.
;;
;; This ties the code to Guile, I'm sorry if it's not as useful for
;; whatever you want to do with it in your life.
;;
;; NL* captures the empty-line so a separate empty-line is not needed
(define-peg-string-patterns
  "transparent-paper <- line* !.
line <- point-line / fold-line
point-line <-- value COMMA value NL*
fold-line <-- FOLD-ALONG axis EQUAL value NL*
axis <- ('x' / 'y')
value <-- number
number <-- [0-9]+
FOLD-ALONG < 'fold along '
COMMA < ','
EQUAL < '='
NL < '\n'")

;; Parse a inner value from a value PEG result
;; For now it only supports number values
(define (parse-value value)
  (let ((tag (car value))
	(v (cadr value)))
    (cond
     ((eq? 'number tag)
      (string->number v)))))

;; Parse a point from a PEG result
;; Returns the result as a pair of x and y coordinates
(define (parse-point-line line)
  (let ((x (cadr (cadr line)))
	(y (cadr (caddr line))))
    (cons (parse-value x) (parse-value y))))


;; Parse a fold line from a PEG result
;; Returns a pair of the axis as a symbol ('x or 'y)
;; and the underlying parsed value
(define (parse-fold-line line)
  (let ((axis (cadr line))
	(fold-point-value (cadr (caddr line))))
      (cons (string->symbol axis) (parse-value fold-point-value))))

;; Parse a set of lines from the PEG tree
;; Returns a list of the lines
(define (parse-lines lst)
  (fold
   (lambda (parsed-line-with-tag prev)
     (let ((tag (car parsed-line-with-tag))
	   (parsed-line (cdr parsed-line-with-tag)))
       (if (eq? tag 'point-line)
	   (cons (append (car prev) (list parsed-line-with-tag))
		 (cdr prev))
	   (cons (car prev)
		 (append (cdr prev) (list parsed-line-with-tag))))))
   (cons '() '())
   lst))

;; Get the maximum value on an axis, x or y
(define (get-max-value point-lines axis)
  (fold
   (lambda (point-line cur-max)
     (let ((points (parse-point-line point-line)))
       (let ((val (if (eq? axis 'x) (car points) (cdr points))))
	 (if (> val cur-max) val cur-max))))
   0
   point-lines))

;; Convert a list of points to a boolean array with a #t for every
;; point
(define (point-lines->array point-lines)
  (let* ((max-x (get-max-value point-lines 'x))
	 (max-y (get-max-value point-lines 'y'))
	 (dst (make-typed-array 'b #f (list 0 max-x) (list 0 max-y))))
    (fold
     (lambda (point-line prev)
       (let ((points (parse-point-line point-line)))
	 (begin
	   (array-set! prev #t (car points) (cdr points))
	   prev)))
     dst
     point-lines)))

;; Define a sheet that contains cached width and height data
(define-record-type <sheet>
  (make-sheet array width height half-width half-height)
  sheet?
  (array sheet-array)
  (width sheet-width)
  (height sheet-height)
  (half-width sheet-half-width)
  (half-height sheet-half-height))

;; Make a sheet record given an array
(define (easy-make-sheet array)
  (let* ((dimensions (array-dimensions array))
	 (width (first dimensions))
	 (height (second dimensions))
	 (half-width (floor (/ width 2)))
	 (half-height (floor (/ height 2))))
    (make-sheet array width height half-width half-height)))

;; Build a sheet from a set of parse lines (point lines)
(define (build-sheet parse-lines)
  (easy-make-sheet (point-lines->array parse-lines)))

;; Counts the visible dots in a sheet and returns it as an integer
(define (visible-dot-count sheet)
  (let* ((width (sheet-width sheet))
	 (height (sheet-height sheet))
	 (total-visible 0))
    (do ((y 0 (1+ y)))
	((>= y height))
      (begin
	(do ((x 0 (1+ x)))
	    ((>= x width))
	  (let ((val (array-ref (sheet-array sheet) x y)))
	    (if val
		(set! total-visible (1+ total-visible)))))))
    total-visible))

;; Returns a pair with the new width and new dimension
;; for a sheet that is to be folded along an axis and fold-point
;; fold-point is a single integer
(define (new-folded-sheet-dimensions sheet axis fold-point)
  (let* ((width (sheet-width sheet))
	 (height (sheet-height sheet))
	 (new-width (if (eq? axis 'x)
			(max
			  fold-point
			  (1- (- width fold-point)))
			width))
	 (new-height (if (eq? axis 'y)
			 (max
			  fold-point
			  (1- (- height fold-point)))
			 height)))
    (cons new-width new-height)))

;; We need to know how big the sheet is to know where to fold points
;; beyond the fold point
;;
;; TODO Step through this with pencil and paper
(define (make-x-axis-fold-mapper width height fold-point)
  (let* ((x-lt-offset (if (< fold-point (floor (/ width 2)))
			  (- (floor (/ width 2)) fold-point)
			  0))
	(2x-fold-point (* fold-point 2))
	(2x-fold-point-plus-offset (+ 2x-fold-point x-lt-offset)))
    (lambda (x y)
      (let ((dst-x (if (< x fold-point)
		       ;; points to the left of the fold point still
		       ;; need to be mapped, since the right hand side
		       ;; may fold to the left of them if the fold point
		       ;; is to the left of the midway point
		       (+ x x-lt-offset)
		       (- 2x-fold-point-plus-offset x)))
	               ;; (abs (- fold-point (- width x)))))
	    (dst-y y))
	(cons dst-x dst-y)))))

;; TODO Step through this with pencil and paper
(define (make-y-axis-fold-mapper width height fold-point)
  (let* ((y-lt-offset (if (< fold-point (floor (/ height 2)))
			 (- (floor (/ height 2)) fold-point)
			 0))
	(2x-fold-point (* fold-point 2))
	(2x-fold-point-plus-offset (+ 2x-fold-point y-lt-offset)))
    (lambda (x y)
      (let ((dst-x x)
	    (dst-y (if (< y fold-point)
		       ;; points below the fold point still need to be
		       ;; mapped, since the bottom side may fold above
		       ;; them if the fold point is above the midway
		       ;; point
		       (+ y y-lt-offset)
		       (+ (- 2x-fold-point y) y-lt-offset))))
		       ;; (abs (- fold-point (- height y))))))
	(cons dst-x dst-y)))))

;; Is the point (x,y) on the x-axis fold line?
(define (on-x-axis-fold-line? fold-point x y)
  (= fold-point x))

;; Is the point (x,y) on the y-axis fold line?
(define (on-y-axis-fold-line? fold-point x y)
  (= fold-point y))

;; Fold a sheet along an axis and fold point
;; axis is a symbol, either 'x or 'y
;; fold-point is a single integer indicating the line to fold on
;; If the axis is 'x, that means the fold is along a vertical line
;; If the axis is 'y, that means the fold is along a horizontal line
(define (fold-sheet sheet axis fold-point)
  (let* ((width (sheet-width sheet))
	 (height (sheet-height sheet))
	 (new-dims (new-folded-sheet-dimensions sheet axis fold-point))
	 (new-width (car new-dims))
	 (new-height (cdr new-dims))
	 (dst (easy-make-sheet
	       (make-typed-array 'b #f new-width new-height)))
	 (fold-mapper (if (eq? axis 'x)
			  (make-x-axis-fold-mapper
			   width height fold-point)
			  (make-y-axis-fold-mapper
			   width height fold-point)))
	 (on-fold-line? (if (eq? axis 'x)
			    on-x-axis-fold-line?
			    on-y-axis-fold-line?)))
    (begin
      (do ((y 0 (1+ y)))
	  ((>= y height))
	(do ((x 0 (1+ x)))
	    ((>= x width))
	  ;; Only update the destination when we're not on the
	  ;; fold-point
	  (if (not (on-fold-line? fold-point x y))
	      (let* ((dst-points (fold-mapper x y))
		     (dst-x (car dst-points))
		     (dst-y (cdr dst-points)))
		(array-set!
		 (sheet-array dst)
		 (or
		  (array-ref (sheet-array dst) dst-x dst-y)
		  (array-ref (sheet-array sheet) x y))
		 dst-x
		 dst-y)))))
      dst)))

;; Dispay a sheet using a series of periods (.) and hash marks (#)
(define (display-sheet sheet)
  (let* ((width (sheet-width sheet))
	 (height (sheet-height sheet)))
    (do ((y 0 (1+ y)))
	((>= y height))
      (begin
	(do ((x 0 (1+ x)))
	    ((>= x width))
	  (let ((val (array-ref (sheet-array sheet) x y)))
	    (if val
		(display "#")
		(display "."))))
	(display "\n")))))

;; Fold a sheet a series of times using the instructions in fold
;; lines.
;; Returns the final folded sheet.
(define (fold-it sheet fold-lines)
    (fold
     (lambda (cur-fold-line prev-sheet)
       (let* ((parsed-fold-line (parse-fold-line cur-fold-line))
	      (axis (car parsed-fold-line))
	      (fold-point (cdr parsed-fold-line)))
	 (fold-sheet prev-sheet axis fold-point)))
     sheet
     fold-lines))

;; Day 13 showing the use of optional arguments and define*
(define* (run-it peg-tree #:optional max-folds)
  (let* ((parsed (parse-lines peg-tree))
	 (point-lines (car parsed))
	 (fold-lines (cdr parsed))
	 (sheet (build-sheet point-lines))
	 (folds (if max-folds
		    (take fold-lines max-folds)
		    fold-lines)))
    (fold-it sheet folds)))

(define test-data "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5\n")

(define *my-peg-tree* (peg:tree (match-pattern transparent-paper test-data)))

;; Run day13 part1 on filename
(define (day13-part1 filename)
  (let* ((data (file-get-string-all filename))
	 (peg-tree
	  (peg:tree (match-pattern transparent-paper data))))
    (begin
      ;; Only do the first fold for part one
      (let ((sheet (run-it peg-tree 1)))
	(display (format #f "day 13 part 1 final-value: ~a\n" (visible-dot-count sheet)))))))

;; Run day13 part2 on filename
(define (day13-part2 filename)
  (let* ((data (file-get-string-all filename))
	 (peg-tree
	  (peg:tree (match-pattern transparent-paper data))))
    (begin
      ;; Only do the first fold for part one
      (let ((sheet (run-it peg-tree)))
	(display (format #f "day 13 part 2 final-value: ~a\n" (visible-dot-count sheet)))))))
