;;
;; Test day10
;;
(define-module (aoc y2021 day10 tests day10)
  #:use-module (srfi srfi-64)
  #:use-module (aoc y2021 day10 day10))

;; Test check-opening-bracket
(test-begin "check-opening-bracket")
(test-equal "( -> #t" #t (check-opening-bracket #\())
(test-equal "[ -> #t" #t (check-opening-bracket #\[))
(test-equal "{ -> #t" #t (check-opening-bracket #\{))
(test-equal "< -> #t" #t (check-opening-bracket #\<))
(test-equal "a -> #f" #f (check-opening-bracket #\a))
(test-end)

(test-begin "check-bracket")
;; Opening bracket should always pass
(test-equal "( '({)" #t (check-bracket #\( '(#\{)))
(test-equal "[ '({)" #t (check-bracket #\[ '(#\{)))
(test-equal "{ '({)" #t (check-bracket #\{ '(#\{)))
(test-equal "< '({)" #t (check-bracket #\< '(#\{)))

;; Some misc tests
(test-equal "( '({{)" #t (check-bracket #\( '(#\{ #\{)))
(test-equal "( '({})" #t (check-bracket #\( '(#\{ #\{)))
(test-equal "} '()" #f (check-bracket #\} '()))

;; Closing bracket must match immediately previous opening bracket
(test-equal ") '(()" #t (check-bracket #\) '(#\()))
(test-equal ") '(()" #t (check-bracket #\) '(#\()))
(test-equal ") '(()" #t (check-bracket #\) '(#\()))
(test-equal ") '(()" #t (check-bracket #\) '(#\()))
(test-equal "] '([)" #t (check-bracket #\] '(#\[)))
(test-equal "] '([)" #t (check-bracket #\] '(#\[)))
(test-equal "] '([)" #t (check-bracket #\] '(#\[)))
(test-equal "] '([)" #t (check-bracket #\] '(#\[)))
(test-equal "} '({)" #t (check-bracket #\} '(#\{)))
(test-equal "} '({)" #t (check-bracket #\} '(#\{)))
(test-equal "} '({)" #t (check-bracket #\} '(#\{)))
(test-equal "} '({)" #t (check-bracket #\} '(#\{)))
(test-equal "> '(<)" #t (check-bracket #\> '(#\<)))
(test-equal "> '(<)" #t (check-bracket #\> '(#\<)))
(test-equal "> '(<)" #t (check-bracket #\> '(#\<)))
(test-equal "> '(<)" #t (check-bracket #\> '(#\<)))

;; Test non-matching closing brackets
(test-equal ") '({)" #f (check-bracket #\) '(#\{)))
(test-equal "] '({)" #f (check-bracket #\] '(#\{)))
(test-equal "} '([)" #f (check-bracket #\} '(#\[)))
(test-equal "> '({)" #f (check-bracket #\> '(#\{)))
(test-end)

(test-begin "make-parse-error-with-bracket")
(let ((parse-error (make-parse-error-with-bracket #\))))
  (test-equal
      "make-parse-error-with-bracket makes exception"
    #t
    (exception? parse-error))
  (test-equal
      "make-parse-error-with-bracket has right type"
    #t
    ((exception-predicate &parse-error) parse-error))

  (test-equal
      "make-parse-error-with-bracket has bracket"
    #\)
    ((record-accessor &parse-error 'bracket) parse-error)))
(test-end)

(test-begin "add-bracket")
;; Test add a new opening bracket
(test-equal "( '() -> '(()" '(#\() (add-bracket #\( '()))
;; Test adding a proper closing bracket
(test-equal ") '(() -> '()" '() (add-bracket #\) '(#\()))
;; Test add-ing an improper closing bracket
(test-equal ") '() -> &parse-error" #f
	    (with-exception-handler
		(lambda (exn) #f)
	      (lambda () (let ((result (add-bracket #\) '()))) result))
	      #:unwind? #t
	      #:unwind-for-type &parse-error))
(test-end)

(test-begin "parse")
;; Test a correctly-formed line
(test-equal "((())) -> ()" '() (parse "((()))"))
;; Test an incorrectly-formed line
;; This doesn't test for the specific error types,
;; but it shows a basic usage of test-error
(test-error "(((} -> ()" &parse-error (parse "(((}"))
;; The SRFI-64 test-error API is a bit confusing, this also passes:
(test-error "(((} -> ()" #f (parse "(((}"))
(test-end)

(test-begin "bracket-score")
(test-equal ") -> 3" 3 (bracket-score #\)))
(test-equal "] -> 57" 57 (bracket-score #\]))
(test-equal "} -> 1197" 1197 (bracket-score #\}))
(test-equal "> -> 25137" 25137 (bracket-score #\>))
(test-equal "a -> 0" 0 (bracket-score #\a))
(test-end)

(test-begin "parse-line")
;; Test a correctly-formed closed line
(test-equal "((())) -> ()" '() (parse-line #t #f #f "((()))"))
(test-equal "((())) -> ()" '() (parse-line #f #f #f "((()))"))
(test-equal "((())) -> 0" 0 (parse-line #f #f #t "((()))"))
;; Test a "correctly-formed" non-closed line
(test-equal "((()) -> 0" 0 (parse-line #t #f #f "((())"))
(test-equal "((()) -> (()" '(#\() (parse-line #f #f #f "((())"))
;; Test an incorrectly-formed line
(test-equal "(((} -> ()" 1197 (parse-line #t #f #f "(((}"))
(test-equal "(((} -> ()" 1197 (parse-line #f #f #f "(((}"))
(test-end)


(test-begin "incomplete-bracket-score")
(test-equal ") -> 1" 1 (incomplete-bracket-score #\)))
(test-equal "] -> 2" 2 (incomplete-bracket-score #\]))
(test-equal "} -> 3" 3 (incomplete-bracket-score #\}))
(test-equal "> -> 4" 4 (incomplete-bracket-score #\>))
(test-equal "a -> 0" 0 (incomplete-bracket-score #\a))
(test-end)

(test-begin "closing-bracket")
(test-equal "( -> )" #\) (closing-bracket #\())
(test-equal "[ -> ]" #\] (closing-bracket #\[))
(test-equal "{ -> }" #\} (closing-bracket #\{))
(test-equal "< -> >" #\> (closing-bracket #\<))
(test-end)


(test-begin "median-score")
(test-equal "(1 2 3 4 5)" 3 (median-score '(1 2 3 4 5)))
(test-equal "(3 1 5 2 4)" 3 (median-score '(3 1 5 2 4)))
(test-equal "(3 1 5 2 4 6)" 4 (median-score '(3 1 5 2 4 6)))
(test-end)

(test-begin "closing-brackets-score")
(test-equal "])}> -> 294" 294 (closing-brackets-score '(#\] #\) #\} #\>)))
(test-end)

(test-begin "complete-parse")
(test-equal "() -> ()" '() (complete-parse '()))
(test-equal "([) -> (])" '(#\]) (complete-parse '(#\[)))
(test-equal "([[) -> (]])" '(#\] #\]) (complete-parse '(#\[ #\[)))
(test-equal "([[{) -> (]]})" '(#\] #\] #\}) (complete-parse '(#\[ #\[ #\{)))
(test-end)

(test-begin "parse-lines")
;; Test a correctly-formed closed line, a "correctly-formed"
;; non-closed line, and an incorrectly-formed line
(let ((lines
       '("((()))" "((())" "(((}")))
  ;; Discarding nothing, get empty parse back, the incomplete
  ;; brackets, and an improper score
  (test-equal
      "( ((())) ((()) (((}) ) #f #f -> "
    '(  ()  ( #\( )  1197  )
    (parse-lines #f #f #f lines))
  ;; Discarding invalid parses, get complete parse back and the
  ;; incomplete brackets
  (test-equal
      "( ((())) ((()) (((}) ) #f #t -> "
    '(() ( #\( ))
    (parse-lines #f #t #f lines))
  ;; Discarding incomplete parses, get back an invalid score
  (test-equal
      "( ((())) ((()) (((}) ) #t #f -> "
    '(  ()  1197  )
    (parse-lines #t #f #f lines))
  ;; Discarding both, get back an empty list
  (test-equal
      "( ((())) ((()) (((}) ) #t #t -> "
    '(  ()  )
    (parse-lines #t #t #f lines)))
(test-end)
