;;
;; Test day14
;;
(define-module (aoc y2021 day14 tests main)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 peg)
  #:use-module ((aoc y2021 day14 main) :prefix day14:)
  #:use-module ((aoc y2021 day14 polymer-instructions-parser) :prefix pip:))

;; Test Parsing Expression Grammars code

(test-begin "day14-peg-string-patterns-template")
(let ((match-record (peg:start (match-pattern pip:template "NNCB"))))
  (test-equal
      "(peg:start (peg:start (match-pattern pip:template \"NNCB\"))) -> 0"
    0
    match-record))
(let ((match-record (peg:start (match-pattern pip:template "NNC"))))
  (test-equal
      "(peg:start (peg:start (match-pattern pip:template \"NNC\"))) -> 0"
    0
    match-record))
(let ((match-record (peg:start (match-pattern pip:template "NNCBN"))))
  (test-equal
      "(peg:start (peg:start (match-pattern pip:template \"NNCBN\"))) -> 0"
    0
    match-record))
;; The beginning of a rule matches a template
;; So we have extra PEG rules for template and template-line
;; Same with rule and rule-line
(let ((match-record (peg:start (match-pattern pip:template "CH > B"))))
  (test-equal
      "(peg:start (match-pattern pip:template \"CH > B\")) -> 0"
    0
    match-record))
(test-end)

(test-begin "day14-peg-string-patterns-template-line")
;; The beginning of a rule matches a template
;; So we have extra PEG rules for template and template-line
;; Same with rule and rule-line
(let ((match-record (peg:start (match-pattern pip:template-line "NNCB\n"))))
  (test-equal
      "(peg:start (match-pattern pip:template-line \"NNCB\n\")) -> 0"
    0
    match-record))
(test-end)


(test-begin "day14-peg-string-patterns-pair")
(let ((match-record (peg:start (match-pattern pip:pair "HH"))))
  (test-equal
      "(peg:start (match-pattern pip:pair \"HH\")) -> 0"
    0
    match-record))
(let ((match-record (match-pattern pip:pair "H")))
  (test-equal
      "(match-pattern pip:pair \"H\") -> #f"
    #f
    match-record))
(let ((match-record (match-pattern pip:pair "")))
  (test-equal
      "(match-pattern pip:pair \"\") -> #f"
    #f
    match-record))
(test-end)

(test-begin "day14-peg-string-patterns-element")
(let ((match-record (peg:start (match-pattern pip:element "H"))))
  (test-equal
      "(peg:start (match-pattern pip:element \"HH\")) -> 0"
    0
    match-record))
(let ((match-record (peg:start (match-pattern pip:element "HH"))))
  (test-equal
      "(peg:start (match-pattern pip:element \"H\")) -> 0"
    0
    match-record))
(let ((match-record (match-pattern pip:element "")))
  (test-equal
      "(match-pattern pip:element \"\") -> #f"
    #f
    match-record))
(test-end)

(test-begin "day14-peg-string-patterns-rule")
(let ((match-record (peg:start (match-pattern pip:element "H"))))
  (test-equal
      "(peg:start (match-pattern pip:element \"HH\")) -> 0"
    0
    match-record))
(let ((match-record (peg:start (match-pattern pip:element "HH"))))
  (test-equal
      "(peg:start (match-pattern pip:element \"H\")) -> 0"
    0
    match-record))
(let ((match-record (match-pattern pip:element "")))
  (test-equal
      "(match-pattern pip:element \"\") -> #f"
    #f
    match-record))
(test-end)


(test-begin "day14-get-pairs")
(test-equal
    "(get-pairs (template \"NNCB\")) -> ((#\\C . #\\B) (#\\N . #\\C) (#\\N . #\\N))"
  '((#\C . #\B) (#\N . #\C) (#\N . #\N))
  (day14:get-pairs '(template "NNCB")))
(test-equal
    "(get-pairs (template \"NNC\")) -> ((#\\N . #\\C) (#\\N . #\\N))"
  '((#\N . #\C) (#\N . #\N))
  (day14:get-pairs '(template "NNC")))
(test-equal
    "(get-pairs (template \"NC\")) -> ((#\\N . #\\C))"
  '((#\N . #\C))
  (day14:get-pairs '(template "NC")))
(test-equal
    "(get-pairs (template \"N\")) -> ()"
  '()
  (day14:get-pairs '(template "N")))
(test-equal
    "(get-pairs (template \"\")) -> ()"
  '()
  (day14:get-pairs '(template "")))
(test-end)


(test-begin "day14-map-it")
(test-equal
    "(map-it ((template \"NNCB\") (rule (pair \"CH\") (element (cg-range \"B\")))) -> (((53 (#\\C . #\\B)) (340 (#\\N . #\\C)) (351 (#\\N . #\\N))) ((59 cg-range \"B\")))"
  '(((53 (#\C . #\B)) (340 (#\N . #\C)) (351 (#\N . #\N))) ((59 cg-range "B")))
  (day14:map-it '((template "NNCB") (rule (pair "CH") (element (cg-range "B"))))))

(test-equal
    "(map-it ((template \"NNCB\"))) -> (((53 (#\\C . #\\B)) (340 (#\\N . #\\C)) (351 (#\\N . #\\N))) ())"
  '(((53 (#\C . #\B)) (340 (#\N . #\C)) (351 (#\N . #\N))) ())
  (day14:map-it '((template "NNCB"))))

(test-equal
    "(map-it ((rule (pair \"CH\") (element (cg-range \"B\")))) -> #f"
    ;; "(map-it ((rule (pair \"CH\") (element (cg-range \"B\")))) -> (((53 (#\\C . #\\B)) (340 (#\\N . #\\C)) (351 (#\\N . #\\N))) ((59 cg-range \"B\")))"
  ;; TODO: Should this be the correct result?
  #f
  ;; '(((53 (#\C . #\B)) (340 (#\N . #\C)) (351 (#\N . #\N))) ((59 cg-range "B")))
  (day14:map-it '((rule (pair "CH") (element (cg-range "B"))))))

(test-equal
    "(map-it () -> (())"
  #f
  (day14:map-it '()))
(test-end)

(test-begin "day14-character-pair->integer")
(test-equal
    "(character-pair->integer '(#\\B . #\\C)) -> 28"
  28
  (day14:character-pair->integer '(#\B . #\C)))
(test-equal
    "(character-pair->integer '(#\\C . #\\B)) -> 53"
  53
  (day14:character-pair->integer '(#\C . #\B)))

(test-error
 "(character-pair->integer '(#\\a . #\\B)) -> &char-error"
 day14:&char-error
 (day14:character-pair->integer '(#\a . #\B)))
(test-equal
    "(character-pair->integer '(#\\a . #\\B)) -> &char-error"
  #f
  (with-exception-handler
      (lambda (exn) #f)
    (lambda () (let ((result (day14:character-pair->integer '(#\a . #\B)))) result))
    #:unwind? #t
    #:unwind-for-type day14:&char-error))

(test-error
 "(character-pair->integer '(#\\A . #\\b)) -> &char-error"
 day14:&char-error
 (day14:character-pair->integer '(#\A . #\b)))
(test-equal
    "(character-pair->integer '(#\\A . #\\b)) -> &char-error"
  #f
  (with-exception-handler
      (lambda (exn) #f)
    (lambda () (let ((result (day14:character-pair->integer '(#\A . #\b)))) result))
    #:unwind? #t
    #:unwind-for-type day14:&char-error))


(test-error
 "(character-pair->integer '(#\\@ . #\\B)) -> &char-error"
 day14:&char-error
 (day14:character-pair->integer '(#\@ . #\B)))
(test-equal
    "(character-pair->integer '(#\\@ . #\\B)) -> &char-error"
  #f
  (with-exception-handler
      (lambda (exn) #f)
    (lambda () (let ((result (day14:character-pair->integer '(#\@ . #\B)))) result))
    #:unwind? #t
    #:unwind-for-type day14:&char-error))

(test-error
 "(character-pair->integer '(#\\A . #\\@)) -> &char-error"
 day14:&char-error
 (day14:character-pair->integer '(#\A . #\@)))
(test-equal
    "(character-pair->integer '(#\\A . #\\@)) -> &char-error"
  #f
  (with-exception-handler
      (lambda (exn) #f)
    (lambda () (let ((result (day14:character-pair->integer '(#\A . #\@)))) result))
    #:unwind? #t
    #:unwind-for-type day14:&char-error))
(test-end)

(test-begin "day14-character-pairs->unique-id-map")
(test-equal
    "(character-pairs->unique-id-map '((#\\A . #\\B) (#\\B . #\\A))) -> ((1 (#\\A . #\\B)) (26 (#\\B . #\\A)))"
  '((1 (#\A . #\B)) (26 (#\B . #\A)))
  (day14:character-pairs->unique-id-map '((#\A . #\B) (#\B . #\A))))
(test-end)


(test-begin "day14-least-freq")
;; (set! *random-state* (random-state-from-platform))
(let ((lfl
       (day14:make-letter-freq-list
	#(3 12 8 14 12 9 9 11 7 0 16 1 10 14 2 7 19 17 18 16 4 2 15 10 11 17))))
  (test-equal
      "(least-freq #<<letter-freq-list> data: #(3 12 8 14 12 9 9 11 7 0 16 1 10 14 2 7 19 17 18 16 4 2 15 10 11 17)) -> (#\\L . 1)"
    '(#\L . 1)
    (day14:least-freq lfl)))

;; TODO: Test some things it was failing at before
(test-end)

(test-begin "day14-most-freq")
(let ((lfl
       (day14:make-letter-freq-list
	#(3 12 8 14 12 9 9 11 7 0 16 1 10 14 2 7 19 17 18 16 4 2 15 10 11 17))))
  (test-equal
      "(most-freq #<<letter-freq-list> data: #(3 12 8 14 12 9 9 11 7 0 16 1 10 14 2 7 19 17 18 16 4 2 15 10 11 17)) -> (#\\Q . 19)"
    '(#\Q . 19)
    (day14:most-freq lfl)))

;; TODO: Test some things it could fail at
(test-end)
