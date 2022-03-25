(define-module (aoc y2021 day4 day4)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (aoc port)
  #:use-module (aoc main)
  #:export (parse-draws parse-board-rows parse-boards
			make-board digit-boards->bitvector-boards build-board-index
			mark-board mark-boards mark-boards-list
			play-bingo play-bingo-loser unmarked-numbers-sum
			day4-part1 day4-part2))

;; Board data structure definition
;; Each board is maintained as a list of twenty-five bitvectors
;; There is one bitvector for the entire board
;;
;; A separate hash is kept to map from bingo numbers to bits in the bitvectors
;;
;; For example, if tables are only 2x2, and the following is the test data:
;;
;; 5, 1, 2, 8, 3, 8
;;
;; 2 7
;; 8 9
;;
;; 3 2
;; 6 0
;;
;; The first data structure is a marker data structure, used to store when a square is marked
;; The board's data structures would be initialized to:
;;      board0  row0 row1 col0 col1   board1  row0 row1 col0 col1
;;   #(       #(#*00 #*00 #*00 #*00)        #(#*00 #*00 #*00 #*00)      )
;; This is a row-major ordering of the boards as bitvectors in a vector
;; The second board would also be: #(#*00 #*00 ), representing 3 2, 6 0, 3 6, 2 0
;;
;; The hash table for numbers zero, one, and two
;; would be (using hybrid Python/lisp syntax): { 0: ((1 1 1) (1 3 1),
;;                                               1: (),
;;                                               2: ((0 0 0) (0 2 0) (1 0 1) (1 3 0)),
;;                                               ... }
;;
;; Where the list represents zero-based values for the location, the first number being
;; the board index, the second number being the row or column index, and the third
;; number being the bitvector index (position in that row or column)
;; There will be two entries for every cell.
;;
;; Example with 3x3 board:
;;
;; 0 1 2
;; 3 4 5
;; 6 7 8
;;
;; 0 -> 0 0, 3 0
;; 1 -> 0 1, 4 0
;; 2 -> 0 2, 5 0
;; 3 -> 1 0, 3 1
;; 4 -> 1 1, 4 1
;; 5 -> 1 2, 5 1
;; 6 -> 2 0, 3 2
;; 7 -> 2 1, 4 2
;; 8 -> 2 2, 5 2
;;
;; The algorithm steps through the drawn numbers one at a time.
;; It looks up the number in the hash table, then steps through the squares that should be
;; marked.  As it marks the square, it checks to see if the bitvector is full.
;;
;; Counting bits is a constant-time operation.


;; Parse a line of comma-separated numbers into a list of numbers
(define (parse-draws line)
  (map
   string->number
   (string-tokenize
    line
    ;;(string-filter (list->char-set char-set:digit char-set:whitespace) line)
    char-set:digit)))

(define (parse-board-rows line)
  (map
   string->number
   (string-tokenize
    line ; (string-filter (list->char-set char-set:digit char-set:whitespace) line)
    char-set:digit)))

;; Parse a list of lines containing boards into a list of boards
(define (parse-boards lines)
  (let parse-boards-helper ((l lines) (all-boards '()) (current-board '()) (new-board #t))
    (if (nil? l)
	(if (not new-board)
	    ;; add the last row board and return all the boards
	    (reverse (cons (reverse current-board) all-boards))
	    ;; (cons current-board all-boards)
	    ;; return all the boards, no partial boards in progress
	    (reverse all-boards))
	(let ((line (car l)))
	  (if (not (string-any char-set:digit line))
	      ;; empty string
	      (parse-boards-helper (cdr l) (cons (reverse current-board) all-boards) '() #t)
	      ;; board row
	      (parse-boards-helper
	       (cdr l) all-boards (cons (parse-board-rows line) current-board) #f))))))


;; Make a bitvector board
;; This is a set of bitvectors stored in a vector
;; generate a blank group of boards from a set of digit boards
;; One gotcha:
;; We need to explicity use list->vector with a set of lists of empty elements
;; because the make-vector constructor uses shared data
;; If we just created it with (make-vector (* size 2) (make-bitvector size #f))
;; Every bitvector would point to the same object, so any updates to one
;; would update all of them
(define (make-board size)
  (list->vector
   (map
    (lambda (x) (make-bitvector size #f))
    (make-list (* size 2)))))

;; generate a blank group of boards from a set of digit boards
;; One gotcha:
;; We need to explicity use list->vector with a set of lists of empty elements
;; because the make-vector constructor uses shared data
(define (digit-boards->bitvector-boards boards)
  (if (nil? boards)
      (list->vector boards)
      (let ((num-boards (length boards))
	    (board-size (length (car boards))))
	;; The boards are all stored in one vector
	(list->vector
	 ;; Each board is stored in a vector
	 ;; Each board has a set of row and column bitvectors
	 (map
	  (lambda (x) (make-board board-size))
	  (make-list num-boards))))))

;; build the hash table board index
(define (build-board-index boards)
  (let ((board-index (make-hash-table 100))
	(board-size (length (car boards))))
    (do ((num-board 0 (1+ num-board)))
	((>= num-board (length boards)))
      (let ((board (list-ref boards num-board)))
	(do ((row 0 (1+ row)))
	    ((>= row board-size))
	  (do ((col 0 (1+ col)))
	      ((>= col board-size))
	    (let ((number (list-ref (list-ref board row) col)))
	      (let* ((hash-entry (hash-ref board-index number))
		     (old-entry (if (eq? hash-entry #f) '() hash-entry)))
		(hash-set!
		 board-index
		 number
		 (cons
		  (list num-board row col)
		  (cons (list num-board (+ board-size col) row)
			old-entry)))))))))
    board-index))

;; mark the board with a draw
;; returns #f if there are no wins
;; returns a list of the boards marked, with a #f if there is a lose
;; and the board index if there is a win
;; This takes a board-index and bitvector index position list
;; The pos-list is a list of board number, row/column index, and cell index
;;
;; This doesn't mark both the row and column,
;; the caller should call it twice for each cell to update the board properly
(define (mark-board pos-list vector-boards)
  (let ((board-index (car pos-list))
	(rc-index (cadr pos-list))
	(cell-index (caddr pos-list)))
    (let* ((board (vector-ref vector-boards board-index))
	   (row-or-column (vector-ref board rc-index)))
      (begin
	(bitvector-set-bit! row-or-column cell-index)
	(if (eq? (bitvector-count row-or-column) (bitvector-length row-or-column))
	    ;; return the board index if there was a bingo
	    board-index
	    ;; return #f if there was not a bingo
	    #f)))))

;; mark all boards that match a draw
;; returns #f if there are no wins
;; returns #f if no boards won, the first board-index if any did
(define (mark-boards draw vector-boards board-table)
  (find
   (lambda (x) (not (eq? x #f)))
   (map
    (lambda (pos-list) (mark-board pos-list vector-boards))
    (hash-ref board-table draw))))

;; mark all boards that match a draw
;; returns a list of winning boards
;; One gotcha is that only boards with draws on them are returned as winners
;; previous winning boards are not returned
(define (mark-boards-list draw vector-boards board-table)
  (filter
   (lambda (x) (not (eq? x #f)))
   (let* ((board-table-result (hash-ref board-table draw))
	  (pos-lists (if board-table-result board-table-result '())))
     (map
      (lambda (pos-list) (mark-board pos-list vector-boards))
      pos-lists))))

;; Go through all the draws until there is a winner
;; Return the first winner
(define (play-bingo vector-boards board-table draws)
  (let play-bingo-helper ((d draws) (winner (list #f 0)))
    (if (nil? d)
	winner
	(let ((draw (car d)))
	  (let ((win (mark-boards draw vector-boards board-table)))
	    (if win
		(play-bingo-helper '() (list win draw))
		(play-bingo-helper (cdr d) (list #f draw))))))))

;; Go through all the draws until all boards have won
;; Return the last winner
;; Not the cleanest solution, the bitvector-position may be inefficient,
;; but it works
(define (play-bingo-loser vector-boards board-table draws)
  (let ((num-boards (vector-length vector-boards)))
    (let play-bingo-helper ((d draws)
			    (winner (list #f 0))
			    (winners (make-bitvector num-boards)))
      (if (null? d)
	  winner
	  (let ((draw (car d)))
	    (let ((win-list (mark-boards-list draw vector-boards board-table))
		  (bv-winner (bitvector-position winners #f)))
	      ;; unneeded
	      (if (not (null? win-list))
		  (begin
		    (for-each (lambda (x) (bitvector-set-bit! winners x)) win-list)
		    (if (eq? (bitvector-count winners) num-boards)
			;; All boards have won, return who the last winners were
			(play-bingo-helper '() (list (list bv-winner) draw) winners)
			(play-bingo-helper (cdr d) (list win-list draw) winners)))
		  ;; empty winner list
		  (play-bingo-helper (cdr d) (list #f draw) winners))))))))

;; sum the unmarked numbers in the board
(define (unmarked-numbers-sum vector-board digit-board)
  (let ((board-size (length digit-board))
	(sum 0))
    (do ((row 0 (1+ row)))
	((>= row board-size))
      (do ((col 0 (1+ col)))
	  ((>= col board-size))
	(if (not (bitvector-bit-set?
		  (vector-ref vector-board row)
		  col))
	    (let ((number (list-ref (list-ref digit-board row) col)))
	      (set! sum (+ sum number))))))
    sum))

;; day3-part1
(define (day4-part1 filename)
  (let ((lines (get-lines filename)))
    (let* ((draws (parse-draws (car lines)))
	   (boards (parse-boards (cdr (cdr lines))))
	   (vector-boards (digit-boards->bitvector-boards boards))
	   (board-table (build-board-index boards))
	   (winner (play-bingo vector-boards board-table draws))
	   (winner-vector-board (vector-ref vector-boards (car winner)))
	   (winner-board (list-ref boards (car winner)))
	   (final-score (unmarked-numbers-sum winner-vector-board winner-board)))
      ;; (display (format #f "draws: ~a, boards: ~a\n" draws boards))
      ;; (display (format #f "bitvector-boards: ~a\n" boards))
      ;; (display (format #f "board-table: ~a\n" board-table))
      (display (format #f "day 4 part 1 final-value: ~a\n"
		       ;; winner
		       (* final-score (cadr winner)))))))

;; Find the last winning board(s)
(define (day4-part2 filename)
  (let ((lines (get-lines filename)))
    (let* ((draws (parse-draws (car lines)))
	   (boards (parse-boards (cdr (cdr lines))))
	   (vector-boards (digit-boards->bitvector-boards boards))
	   (board-table (build-board-index boards))
	   (last-winners (play-bingo-loser vector-boards board-table draws))
	   ;; There may be multiple last winners, just pick the first item in the list
	   (last-winner (caar last-winners))
	   (winner-vector-board (vector-ref vector-boards last-winner))
	   (winner-board (list-ref boards last-winner))
	   (final-score (unmarked-numbers-sum winner-vector-board winner-board)))
      (begin
	;; (display (format #f "draws: ~a, boards: ~a\n" draws boards))
	;; (display (format #f "bitvector-boards: ~a\n" boards))
	;; (display (format #f "board-table: ~a\n" board-table))
	(display (format #f "last-winners: ~a\n" last-winners))
	(display (format #f "winner-board: ~a\n" winner-board))
	(display (format #f "day 4 part 2 last-winner: ~a, final-value: ~a\n"
       		       last-winner
       		       (* final-score (cadr last-winners))))))))

;; (day4-part1 (get-args))
