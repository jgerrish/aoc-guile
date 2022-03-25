;;
;; Test day4
;;
(define-module (aoc y2021 day4 tests day4)
  #:use-module (srfi srfi-64)
  #:use-module (aoc y2021 day4 day4))

;; Test parse-draws
(test-begin "parse-draws")
(test-equal "\"\" -> ()" '() (parse-draws ""))
(test-equal "\"1\" -> (1)" '(1) (parse-draws "1"))
(test-equal "\"1,2\" -> (1 2)" '(1 2) (parse-draws "1,2"))
(test-equal "\"1, 2\" -> (1 2)" '(1 2) (parse-draws "1, 2"))
(test-equal "\"1,2,3\" -> (1 2 3)" '(1 2 3) (parse-draws "1,2,3"))
(test-equal "\"1,2, 3\" -> (1 2 3)" '(1 2 3) (parse-draws "1,2,3"))
(test-end)

;; Test parse-board-rows
(test-begin "parse-board-rows")
(test-equal "\"\" -> ()" '() (parse-board-rows ""))
(test-equal "\"0 1\n\" -> (0 1)"
  '(0 1) (parse-board-rows "0 1\n"))
(test-equal "\"0 1 2\n\" -> (0 1 2)"
  '(0 1 2) (parse-board-rows "0 1 2\n"))
(test-end)

;; test parsing boards
(test-begin "parse-boards")
(test-equal "() -> ()" '() (parse-boards '()))
(test-equal "((\"0 1\") (\"2 3\")) -> (((0 1) (2 3)))"
  '(((0 1) (2 3))) (parse-boards '("0 1" "2 3")))
(test-end)

;; test making an empty board
(test-begin "make-board")
(test-equal "0 -> #()" #() (make-board 0))
(test-equal "1 -> #(#*0)" '#(#*0 #*0) (make-board 1))
(test-equal "2 -> #(#*00 #*00 #*00 #*00)" '#(#*00 #*00 #*00 #*00) (make-board 2))
(test-end)

(test-begin "digit-boards->bitvector-boards")
(test-equal "() -> #()" #() (digit-boards->bitvector-boards '()))
(test-equal "(((0))) -> #(#*0 #*0)" #(#(#*0 #*0)) (digit-boards->bitvector-boards '(((0)))))

;; TODO: Maybe this should fail
(test-equal "((0)) -> #f" #(#(#*0 #*0)) (digit-boards->bitvector-boards '((0))))

(test-equal "((0) (0)) -> #(#(#*0 #(#*0) #(#*0 #*0)))" #(#(#*0 #*0) #(#*0 #*0))
	    (digit-boards->bitvector-boards '((0) (0))))
(test-equal "(((0 1) (2 3))) -> #(#(#*00 #*00 #*00 #*00))" #(#(#*00 #*00 #*00 #*00)) (digit-boards->bitvector-boards '(((0 1) (2 3)))))
(test-equal
    "( ((0 1) (2 3)) ((4 5) (6 7))) -> #(#(#*00 #*00 #*00 #*00) #(#*00 #*00 #*00 #*00))"
  #(#(#*00 #*00 #*00 #*00) #(#*00 #*00 #*00 #*00))
  (digit-boards->bitvector-boards '( ((0 1) (2 3)) ((4 5) (6 7)) )))
(test-end)

;; test making the board table/index
(test-begin "build-board-index")
(let ((board-table (build-board-index '( ((0 1) (2 3)) ((4 5) (2 6)) ))))
  (test-equal "board-index 0"
    '((0 0 0) (0 2 0))
    (hash-ref board-table 0))
  (test-equal "board-index 1"
    '((0 0 1) (0 3 0))
    (hash-ref board-table 1))
  (test-equal "board-index 2"
    ;; Boards in in reverse order
    '((1 1 0) (1 2 1) (0 1 0) (0 2 1))
    (hash-ref board-table 2))
  (test-equal "board-index 3"
    '((0 1 1) (0 3 1))
    (hash-ref board-table 3))
  (test-equal "board-index 4"
    '((1 0 0) (1 2 0))
    (hash-ref board-table 4))
  (test-equal "board-index 5"
    '((1 0 1) (1 3 0))
    (hash-ref board-table 5))
  (test-equal "board-index 6"
    '((1 1 1) (1 3 1))
    (hash-ref board-table 6)))
(test-end)


(test-begin "mark-board")
(let ((boards (list->vector (list (vector
				   (make-bitvector 2)
				   (make-bitvector 2)
				   (make-bitvector 2)
				   (make-bitvector 2))))))
  (begin
    ;; mark the upper-left cell, row
    (test-equal "board-index 2x2 empty #f" #f
		(mark-board '(0 0 0) boards))
    ;; mark the upper-left cell, column
    (test-equal "board-index 2x2 empty #f" #f
		(mark-board '(0 2 0) boards))

    ;; mark the lower-left cell, row
    (test-equal "board-index 2x2 empty 0" #f
		(mark-board '(0 1 0) boards))
    ;; mark the lower-left cell, column
    ;; This should complete the first column
    (test-equal "board-index 2x2 empty 0" 0
		(mark-board '(0 2 1) boards))))
(test-end)

(test-begin "mark-boards")
;; Test marking to a win
(let* ((boards '(((0 1) (2 3))))
       (board-table (build-board-index boards))
       (vector-boards (list->vector (list (vector
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2))))))
  (begin
    (test-equal "mark 0" #f
		(mark-boards 0 vector-boards board-table))
    (test-equal "mark 0" 0
		(mark-boards 2 vector-boards board-table))))

;; Test marking a number that isn't on the boards
(let* ((boards '(((0 1) (2 3))))
       (board-table (build-board-index boards))
       (vector-boards (list->vector (list (vector
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2))))))
  (begin
    (test-equal "mark non-existent 4" #f
		(mark-boards 0 vector-boards board-table))))

(test-end)

(test-begin "mark-boards-list")
(let* ((boards '(((0 1) (2 3)) ((4 2) (5 6))))
       (board-table (build-board-index boards))
       (vector-boards (list->vector (list (vector
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2))
					  (vector
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2))))))
  (begin
    (test-equal "mark 0" '()
		(mark-boards-list 0 vector-boards board-table))
    (test-equal "mark 2" '(0)
		(mark-boards-list 2 vector-boards board-table))
    (test-equal "mark 4" '(1)
		(mark-boards-list 4 vector-boards board-table))))


;; Test marking a number not on the boards
(let* ((boards '(((0 1) (2 3)) ((4 2) (5 6))))
       (board-table (build-board-index boards))
       (vector-boards (list->vector (list (vector
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2))
					  (vector
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2))))))
  (begin
    (test-equal "mark 0" '()
		(mark-boards-list 7 vector-boards board-table))))


(let* ((boards '(((0 1) (2 3)) ((0 2) (5 6))))
       (board-table (build-board-index boards))
       (vector-boards (list->vector (list (vector
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2))
					  (vector
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2))))))
  (begin
    (test-equal "mark 0" '()
		(mark-boards-list 0 vector-boards board-table))
    (test-equal "mark 2" '(1 0)
		(mark-boards-list 2 vector-boards board-table))
    ;; One gotcha is that only boards with draws on them are returned as winners
    ;; previous winning boards are not returned
    (test-equal "mark 5" '(1)
		(mark-boards-list 5 vector-boards board-table))
    ;; Test a non-existent number
    (test-equal "mark 4" '()
		(mark-boards-list 4 vector-boards board-table))))
(test-end)


(test-begin "play-bingo")
(let* ((boards '(((0 1) (2 3))))
       (board-table (build-board-index boards))
       (vector-boards (list->vector (list (vector
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)))))
       (draws '(0 2)))
  (begin
    (test-equal "mark 0 and 2, return the winning board and draw" '(0 2)
		(play-bingo vector-boards board-table draws))))
(test-end)


(test-begin "play-bingo-loser")



(let* ((boards '(((0 1) (2 3)) ((0 2) (5 6))  ((7 8) (9 10))))
       (board-table (build-board-index boards))
       (vector-boards (list->vector (list (vector
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2))
					  (vector
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2))
					  (vector
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)))))
       (draws '(0 1 2 5 8 10)))
  (begin
    (test-equal "mark (0 1 2 5 8 10), return the winning board and draw" '((2) 10)
		(play-bingo-loser vector-boards board-table draws))))
(test-end)


(test-begin "unmarked-numbers-sum")
(let* ((boards '(((0 1) (2 3))))
       (board-table (build-board-index boards))
       (vector-boards (list->vector (list (vector
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)
					   (make-bitvector 2)))))
       (draws '(0 2)))
  (begin
    (test-equal "mark 0 and 2, return the winning board and draw" '(0 2)
		(play-bingo vector-boards board-table draws))
    (test-equal "sum: 4"
      4
      (unmarked-numbers-sum (vector-ref vector-boards 0) (car boards)))))
(test-end)
