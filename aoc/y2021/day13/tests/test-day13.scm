;;
;; Test day13
;;
(define-module (aoc y2021 day13 tests day13)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 peg)
  ;; Here we first use the :prefix in the module interface
  ;; specification to rename everything exported by day13 to have a
  ;; day13:prefix
  ;;
  ;; This lets us test PEG grammars without polluting the namespace
  ;; These Guile features should have been introduced by me in earlier
  ;; days.
  #:use-module ((aoc y2021 day13 day13) :prefix day13:))

;; Test Parsing Expression Grammars code

(test-begin "day13-peg-string-patterns-axis")
(let ((match-record (match-pattern day13:axis "a")))
  (test-equal
    "(match-pattern day13:axis \"a\") -> #f"
  #f
  match-record))
(let ((match-record (match-pattern day13:axis "x")))
  (test-equal
      "(peg:start (match-pattern day13:axis \"x\")) -> 0"
    0
    (peg:start match-record))
  (test-equal
      "(peg:end (match-pattern day13:axis \"x\")) -> 1"
    1
    (peg:end match-record))
  (test-equal
      "(peg:tree (match-pattern day13:axis \"x\")) -> \"x\""
    "x"
    (peg:tree match-record)))
(let ((match-record (match-pattern day13:axis "y")))
  (test-equal
      "(peg:start (match-pattern day13:axis \"y\")) -> 0"
    0
    (peg:start match-record))
  (test-equal
      "(peg:end (match-pattern day13:axis \"y\")) -> 1"
    1
    (peg:end match-record))
  (test-equal
      "(peg:tree (match-pattern day13:axis \"y\")) -> \"y\""
    "y"
    (peg:tree match-record)))
(test-end)

(test-begin "day13-peg-string-patterns-number")
(let ((match-record (match-pattern day13:number "a")))
  (test-equal
    "(match-pattern day13:number \"a\") -> #f"
  #f
  match-record))
(let ((match-record (match-pattern day13:number "0")))
  (test-equal
      "(peg:start (match-pattern day13:number \"0\")) -> 0"
    0
    (peg:start match-record))
  (test-equal
      "(peg:end (match-pattern day13:number \"0\")) -> 1"
    1
    (peg:end match-record))
  (test-equal
      "(peg:string (match-pattern day13:number \"0\")) -> \"0\""
    "0"
    (peg:string match-record))
  (test-equal
      "(peg:tree (match-pattern day13:number \"0\")) -> (number \"0\")"
    '(number "0")
    (peg:tree match-record)))

;; Test a 2-digit number
(let ((match-record (match-pattern day13:number "10")))
  (test-equal
      "(peg:start (match-pattern day13:number \"10\")) -> 0"
    0
    (peg:start match-record))
  (test-equal
      "(peg:end (match-pattern day13:number \"10\")) -> 2"
    2
    (peg:end match-record))
  (test-equal
      "(peg:string (match-pattern day13:number \"10\")) -> \"10\""
    "10"
    (peg:string match-record))
  (test-equal
      "(peg:tree (match-pattern day13:number \"10\")) -> (number \"10\")"
    '(number "10")
    (peg:tree match-record)))
(test-end)

(test-begin "day13-peg-string-patterns-point-line")
(let ((match-record (match-pattern day13:point-line "12,")))
  (test-equal
      "(match-pattern day13:point-line \"12,\") -> #f"
    #f
    match-record))
(let ((match-record (match-pattern day13:point-line "12,34")))
  (test-equal
      "(peg:start day13:point-line \"12,34\") -> 0"
    0
    (peg:start match-record))
  (test-equal
      "(peg:end day13:point-line \"12,34\") -> 5"
    5
    (peg:end match-record))
  (test-equal
      "(peg:string day13:point-line \"12,34\") -> \"12,34\""
    "12,34"
    (peg:string match-record))
  (test-equal
      "(peg:tree day13:point-line \"12,34\") -> (point-line (value (number \"12\")) (value (number \"34\")))"
    '(point-line (value (number "12")) (value (number "34")))
    (peg:tree match-record)))
(test-end)

(test-begin "day13-peg-string-patterns-fold-line")
(let ((match-record (match-pattern day13:fold-line "fold along a=7")))
  (test-equal
      "(match-pattern day13:number \"fold along a=7\") -> #f"
    #f
    match-record))
(let ((match-record (match-pattern day13:fold-line "fold along x=5")))
  (test-equal
      "(peg:start day13:fold-line \"fold along x=5\") -> 0"
    0
    (peg:start match-record))
  (test-equal
      "(peg:end day13:fold-line \"fold along x=5\") -> 14"
  14
  (peg:end match-record))
  (test-equal
      "(peg:string day13:fold-line \"fold along x=5\") -> \"fold along x=5\""
    "fold along x=5"
  (peg:string match-record))
  (test-equal
      "(peg:tree day13:fold-line \"fold along x=5\") -> (fold-line \"x\" (value (number \"5\")))"
    '(fold-line "x" (value (number "5")))
  (peg:tree match-record)))
(test-end)

(test-begin "day13-build-sheet")
(let* ((test-data "0,1\n1,0\n2,1\n2,2\n")
       (peg-tree
	(peg:tree (match-pattern day13:transparent-paper test-data)))
       (parsed (day13:parse-lines peg-tree))
       (point-lines (car parsed))
       (sheet (day13:build-sheet point-lines)))
  (test-equal
      "build-sheet builds proper array"
    #2b((#f #t #f) (#t #f #f) (#f #t #t))
    (day13:sheet-array sheet))
  (test-equal
      "build-sheet builds proper width"
    3
    (day13:sheet-width sheet))
  (test-equal
      "build-sheet builds proper height"
    3
    (day13:sheet-height sheet)))

(let* ((test-data "0,1\n1,0\n2,1\n")
       (peg-tree
	(peg:tree (match-pattern day13:transparent-paper test-data)))
       (parsed (day13:parse-lines peg-tree))
       (point-lines (car parsed))
       (sheet (day13:build-sheet point-lines)))
  (test-equal
      "build-sheet builds proper array"
    #2b((#f #t) (#t #f) (#f #t))
    (day13:sheet-array sheet))
  (test-equal
      "build-sheet builds proper width"
    3
    (day13:sheet-width sheet))
  (test-equal
      "build-sheet builds proper height"
    2
    (day13:sheet-height sheet)))

(test-end)


(test-begin "day13-x-axis-fold-mapper")
;; Test mapping (0, 0) on a fold along 1 on the x-axis
(test-equal
    "(0,0) folded along 1 on the x-axis maps to (0,0)"
  (cons 0 0)
  ((day13:make-x-axis-fold-mapper 3 3 1) 0 0))
(test-equal
    "(2,0) folded along 1 on the x-axis maps to (0,0)"
  (cons 0 0)
  ((day13:make-x-axis-fold-mapper 3 3 1) 2 0))
(test-equal
    "(0,2) folded along 1 on the x-axis maps to (0,2)"
  (cons 0 2)
  ((day13:make-x-axis-fold-mapper 3 3 1) 0 2))

;; Test some other maps
(test-equal
    "(0,0) folded along 1 with width 4 on the x-axis maps to (1,0)"
  (cons 1 0)
  ((day13:make-x-axis-fold-mapper 4 3 1) 0 0))

(test-equal
    "(3,0) folded along 1 with width 4 on the x-axis maps to (0,0)"
  (cons 0 0)
  ((day13:make-x-axis-fold-mapper 4 3 1) 3 0))

(test-equal
    "(2,0) folded along 1 with width 4 on the x-axis maps to (1,0)"
  (cons 1 0)
  ((day13:make-x-axis-fold-mapper 4 3 1) 2 0))

(test-end)

(test-begin "day13-y-axis-fold-mapper")
;; Test mapping (0, 0) on a fold along 1 on the y-axis
(test-equal
    "(0,0) folded along 1 on the y-axis maps to (0,0)"
  (cons 0 0)
  ((day13:make-y-axis-fold-mapper 3 3 1) 0 0))
(test-equal
    "(2,0) folded along 1 on the y-axis maps to (2,0)"
  (cons 2 0)
  ((day13:make-y-axis-fold-mapper 3 3 1) 2 0))
(test-equal
    "(0,2) folded along 1 on the y-axis maps to (0,0)"
  (cons 0 0)
  ((day13:make-y-axis-fold-mapper 3 3 1) 0 2))

;; Test some other maps
(test-equal
    "(0,0) folded along 1 with height 4 on the y-axis maps to (0,1)"
  (cons 0 1)
  ((day13:make-y-axis-fold-mapper 3 4 1) 0 0))

(test-equal
    "(0,3) folded along 1 with width 4 on the y-axis maps to (0,0)"
  (cons 0 0)
  ((day13:make-y-axis-fold-mapper 3 4 1) 0 3))

(test-equal
    "(2,0) folded along 1 with width 4 on the y-axis maps to (1,0)"
  (cons 0 1)
  ((day13:make-y-axis-fold-mapper 3 4 1) 0 2))
(test-end)

(test-begin "day13-on-x-axis-fold-line?")
(test-equal
    "(1,0) on x-axis fold line 1 is #t"
  #t
  (day13:on-x-axis-fold-line? 1 1 0))
(test-equal
    "(0,0) on x-axis fold line 1 is #f"
  #f
  (day13:on-x-axis-fold-line? 1 0 0))
(test-equal
    "(1,1) on x-axis fold line 1 is #t"
  #t
  (day13:on-x-axis-fold-line? 1 1 1))
(test-equal
    "(1,2) on x-axis fold line 1 is #t"
  #t
  (day13:on-x-axis-fold-line? 1 1 2))
(test-end)

(test-begin "day13-on-y-axis-fold-line?")
(test-equal
    "(0,1) on y-axis fold line 1 is #t"
  #t
  (day13:on-y-axis-fold-line? 1 0 1))
(test-equal
    "(0,0) on y-axis fold line 1 is #f"
  #f
  (day13:on-y-axis-fold-line? 1 0 0))
(test-equal
    "(1,1) on y-axis fold line 1 is #t"
  #t
  (day13:on-y-axis-fold-line? 1 1 1))
(test-equal
    "(2,1) on y-axis fold line 1 is #t"
  #t
  (day13:on-y-axis-fold-line? 1 2 1))
(test-end)

(test-begin "day13-fold-sheet")

;; Simple 3x3 sheet
;; .#.
;; #.#
;; ..#
(let* ((test-data "0,1\n1,0\n2,1\n2,2\n")
       (peg-tree
	(peg:tree (match-pattern day13:transparent-paper test-data)))
       (parsed (day13:parse-lines peg-tree))
       (point-lines (car parsed))
       (sheet (day13:build-sheet point-lines))
       ;; .
       ;; #
       ;; #
       (folded-sheet-x-half (day13:fold-sheet sheet 'x 1))
       ;; .##
       (folded-sheet-y-half (day13:fold-sheet sheet 'y 1)))
  (test-equal
      "build-sheet builds proper array"
    #2b((#f #t #f) (#t #f #f) (#f #t #t))
    (day13:sheet-array sheet))
  (test-equal
      "build-sheet builds proper width"
    3
    (day13:sheet-width sheet))
  (test-equal
      "build-sheet builds proper height"
    3
    (day13:sheet-height sheet))
  (begin
    (test-equal
	"fold-sheet along x builds proper array"
      #2b((#f #t #t))
      (day13:sheet-array folded-sheet-x-half))
    (test-equal
	"fold-sheet along x builds proper width"
      1
      (day13:sheet-width folded-sheet-x-half))
    (test-equal
	"fold-sheet along x builds proper height"
      3
      (day13:sheet-height folded-sheet-x-half)))

  (begin
    (test-equal
	"fold-sheet along y builds proper array"
      #2b((#f) (#t) (#t))
      (day13:sheet-array folded-sheet-y-half))
    (test-equal
	"fold-sheet along y builds proper width"
      3
      (day13:sheet-width folded-sheet-y-half))
    (test-equal
	"fold-sheet along y builds proper height"
      1
      (day13:sheet-height folded-sheet-y-half))))

;; Deal with folds less than or greater than the half-way point
;;
;; .#..
;; #.#.
;; ..#.
;; .#.#
;;
(let* ((test-data "0,1\n1,0\n2,1\n2,2\n1,3\n3,3\n")
       (peg-tree
	(peg:tree (match-pattern day13:transparent-paper test-data)))
       (parsed (day13:parse-lines peg-tree))
       (point-lines (car parsed))
       (sheet (day13:build-sheet point-lines))
       ;; ..
       ;; .#
       ;; .#
       ;; #.
       (folded-sheet-x-lt-half (day13:fold-sheet sheet 'x 1))
       ;; .#.#
       ;; .##.
       (folded-sheet-y-lt-half (day13:fold-sheet sheet 'y 1))
       ;; .#
       ;; #.
       ;; ..
       ;; .#
       (folded-sheet-x-gt-half (day13:fold-sheet sheet 'x 2))
       ;; .#..
       ;; ####
       (folded-sheet-y-gt-half (day13:fold-sheet sheet 'y 2)))
  (begin
    (test-equal
	"build-sheet builds proper array"
      #2b((#f #t #f #f) (#t #f #f #t) (#f #t #t #f) (#f #f #f #t))
      (day13:sheet-array sheet))

    (test-equal
	"fold-sheet along x lt halfway builds proper array"
      #2b((#f #f #f #t) (#f #t #t #f))
      (day13:sheet-array folded-sheet-x-lt-half))

    (test-equal
	"fold-sheet along y lt halfway builds proper array"
      #2b((#f #f) (#t #t) (#f #t) (#t #f))
      (day13:sheet-array folded-sheet-y-lt-half))))
(test-end)
