;; --- Day 10: Syntax Scoring ---

;; You ask the submarine to determine the best route out of the
;; deep-sea cave, but it only replies:

;; Syntax error in navigation subsystem on line: all of them
;; All of them?! The damage is worse than you thought. You bring up a
;; copy of the navigation subsystem (your puzzle input).

;; The navigation subsystem syntax is made of several lines containing
;; chunks. There are one or more chunks on each line, and chunks
;; contain zero or more other chunks. Adjacent chunks are not
;; separated by any delimiter; if one chunk stops, the next chunk (if
;; any) can immediately start. Every chunk must open and close with
;; one of four legal pairs of matching characters:

;; If a chunk opens with (, it must close with ).
;; If a chunk opens with [, it must close with ].
;; If a chunk opens with {, it must close with }.
;; If a chunk opens with <, it must close with >.
;; So, () is a legal chunk that contains no other chunks, as is
;; []. More complex but valid chunks include ([]), {()()()}, <([{}])>,
;; [<>({}){}[([])<>]], and even (((((((((()))))))))).

;; Some lines are incomplete, but others are corrupted. Find and
;; discard the corrupted lines first.

;; A corrupted line is one where a chunk closes with the wrong
;; character - that is, where the characters it opens and closes with
;; do not form one of the four legal pairs listed above.

;; Examples of corrupted chunks include (], {()()()>, (((()))}, and
;; <([]){()}[{}]). Such a chunk can appear anywhere within a line, and
;; its presence causes the whole line to be considered corrupted.

;; For example, consider the following navigation subsystem:

;; [({(<(())[]>[[{[]{<()<>>
;; [(()[<>])]({[<{<<[]>>(
;; {([(<{}[<>[]}>{[]{[(<()>
;; (((({<>}<{<{<>}{[]{[]{}
;; [[<[([]))<([[{}[[()]]]
;; [{[{({}]{}}([{[{{{}}([]
;; {<[[]]>}<{[{[{[]{()[[[]
;; [<(<(<(<{}))><([]([]()
;; <{([([[(<>()){}]>(<<{{
;; <{([{{}}[<[[[<>{}]]]>[]]
;; Some of the lines aren't corrupted, just incomplete; you can ignore
;; these lines for now. The remaining five lines are corrupted:

;; {([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
;; [[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
;; [{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
;; [<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
;; <{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.
;; Stop at the first incorrect closing character on each corrupted
;; line.

;; Did you know that syntax checkers actually have contests to see who
;; can get the high score for syntax errors in a file? It's true! To
;; calculate the syntax error score for a line, take the first illegal
;; character on the line and look it up in the following table:

;; ): 3 points.
;; ]: 57 points.
;; }: 1197 points.
;; >: 25137 points.
;; In the above example, an illegal ) was found twice (2*3 = 6
;; points), an illegal ] was found once (57 points), an illegal } was
;; found once (1197 points), and an illegal > was found once (25137
;; points). So, the total syntax error score for this file is
;; 6+57+1197+25137 = 26397 points!

;; Find the first illegal character in each corrupted line of the
;; navigation subsystem. What is the total syntax error score for
;; those errors?

;; --- Part Two ---

;; Now, discard the corrupted lines. The remaining lines are incomplete.

;; Incomplete lines don't have any incorrect characters - instead,
;; they're missing some closing characters at the end of the line. To
;; repair the navigation subsystem, you just need to figure out the
;; sequence of closing characters that complete all open chunks in the
;; line.

;; You can only use closing characters (), ], }, or >), and you must
;; add them in the correct order so that only legal pairs are formed
;; and all chunks end up closed.

;; In the example above, there are five incomplete lines:

;; [({(<(())[]>[[{[]{<()<>> - Complete by adding }}]])})].
;; [(()[<>])]({[<{<<[]>>( - Complete by adding )}>]}).
;; (((({<>}<{<{<>}{[]{[]{} - Complete by adding }}>}>)))).
;; {<[[]]>}<{[{[{[]{()[[[] - Complete by adding ]]}}]}]}>.
;; <{([{{}}[<[[[<>{}]]]>[]] - Complete by adding ])}>.
;; Did you know that autocomplete tools also have contests? It's true!
;; The score is determined by considering the completion string
;; character-by-character. Start with a total score of 0. Then, for
;; each character, multiply the total score by 5 and then increase the
;; total score by the point value given for the character in the
;; following table:

;; ): 1 point.
;; ]: 2 points.
;; }: 3 points.
;; >: 4 points.
;; So, the last completion string above - ])}> - would be scored as follows:

;; Start with a total score of 0.
;; Multiply the total score by 5 to get 0, then add the value of ] (2)
;; to get a new total score of 2.
;; Multiply the total score by 5 to get 10, then add the value of )
;; (1) to get a new total score of 11.
;; Multiply the total score by 5 to get 55, then add the value of }
;; (3) to get a new total score of 58.
;; Multiply the total score by 5 to get 290, then add the value of >
;; (4) to get a new total score of 294.
;; The five lines' completion strings have total scores as follows:

;; }}]])})] - 288957 total points.
;; )}>]}) - 5566 total points.
;; }}>}>)))) - 1480781 total points.
;; ]]}}]}]}> - 995444 total points.
;; ])}> - 294 total points.

;; Autocomplete tools are an odd bunch: the winner is found by sorting
;; all of the scores and then taking the middle score. (There will
;; always be an odd number of scores to consider.) In this example,
;; the middle score is 288957 because there are the same number of
;; scores smaller and larger than it.

;; Find the completion string for each incomplete line, score the
;; completion strings, and sort the scores. What is the middle score?


(define-module (aoc y2021 day10 day10)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 exceptions)
  #:use-module (aoc port)
  #:use-module (aoc main)
  #:export (check-opening-bracket
	    check-bracket
	    &parse-error make-parse-error-with-bracket
	    add-bracket parse parse-line parse-lines complete-parse closing-bracket
	    bracket-score incomplete-bracket-score closing-brackets-score median-score
	    day10-part1
	    day10-part2))

;; Returns true if the bracket is an opening bracket character
(define (check-opening-bracket bracket)
  (or (eq? bracket #\() (eq? bracket #\[) (eq? bracket #\{) (eq? bracket #\<)))

;; Check that a bracket is a valid addition to a parse
;; Returns true if it's valid, false if it is not valid
(define (check-bracket bracket stack)
  (if (nil? stack)
      (check-opening-bracket bracket)
      (let ((last-bracket (car stack)))
	(cond
	 ((eq? bracket #\))
	  (eq? last-bracket #\())
	 ((eq? bracket #\])
	  (eq? last-bracket #\[))
	 ((eq? bracket #\})
	  (eq? last-bracket #\{))
	 ((eq? bracket #\>)
	  (eq? last-bracket #\<))
	 (else
	  (check-opening-bracket bracket))))))

;; Create a custom exception type &parse-error that accepts an
;; argument called bracket, the bracket that caused the error
(define &parse-error
  (make-exception-type '&parse-error &exception (list 'bracket)))

;; Create an &parse-error exception Exception handling in Guile is a
;; complicated beast that leverages the continuation system of Guile.
;; In addition, exceptions can inherit from other exceptions and exceptions
;; can be built that contain a tree of exceptions
;;
;; Below is an example of using a &parse-error exception, but it can
;; be applied to your own user-created exception types This code uses
;; the newer (as of 2019) with-exception-handler and raise-exception
;; functions.
;; First, define an exception like above.  Then create a builder like
;; below with make-parse-error-with-bracket.  After that, an
;; exception can be raised with the following call:
;; (raise-exception (make-parse-error-with-bracket bracket))))
(define make-parse-error-with-bracket
  (record-constructor &parse-error))

;; Add a bracket to the parse stack
;; Return the new parse stack, throws an exception if there is a parse
;; error
(define (add-bracket bracket stack)
  (if (check-bracket bracket stack)
      ;; We're checking for an opening bracket here twice, but it's
      ;; still fast
      (if (check-opening-bracket bracket)
	  (cons bracket stack)
	  (cdr stack))
      (raise-exception
       (make-parse-error-with-bracket bracket))))

;; Parse a line
(define (parse line)
  (string-fold
   (lambda (c prev)
     (add-bracket c prev))
   '()
   line))

;; Find the score for an improper closing bracket
(define (bracket-score bracket)
  (cond
   ((eq? bracket #\)) 3)
   ((eq? bracket #\]) 57)
   ((eq? bracket #\}) 1197)
   ((eq? bracket #\>) 25137)
   (else 0)))


;; Find the score for an incomplete closing bracket
(define (incomplete-bracket-score bracket)
  (cond
   ((eq? bracket #\)) 1)
   ((eq? bracket #\]) 2)
   ((eq? bracket #\}) 3)
   ((eq? bracket #\>) 4)
   (else 0)))

;; Return the closing bracket for an opening bracket
(define (closing-bracket bracket)
  (cond
   ((eq? bracket #\() #\))
   ((eq? bracket #\[) #\])
   ((eq? bracket #\{) #\})
   ((eq? bracket #\<) #\>)))

;; Parse a line
;; This function shows how exception handlers are created
;; with-exception handler accepts two required arguments,
;;   1.  the exception handler
;;   2.  a thunk to execute under the handler
;; To catch the exception, an exception handler must be setup with
;; "with-exception-handler
;; it accepts optional arguments to control how the stack is unwound
;; and what exception types should be handled
;; We want the stack unwound, so we specify: #:unwind? #t
;; This lets us unwind the stack up a couple levels back to the
;; surrounding exception handler in parse-line, instead of add-bracket
;; where it's raised
;; In addition, we only handle &parse-error exceptions with #unwind-for-type,
;; That way stack-overflow, out-of-memory, and any other exceptions like maybe
;; signals are handled correctly.
;; Returns 0 if a line is invalid and a discard-* flag is specified
;; for that type
;;
;; The API here is that invalid parses like incomplete and corrupted
;; parses return a zero.  These results can be filtered out with a
;; higher-level function.
(define (parse-line discard-incomplete-parses?
		    discard-invalid-parses?
		    discard-complete-parses?
		    line)
  (with-exception-handler
      (lambda (exn)
	;; Handle exception
	(if discard-invalid-parses?
	    0
	    (bracket-score ((record-accessor &parse-error 'bracket) exn))))
    (lambda ()
      (let ((result (parse line)))
	(if (and discard-complete-parses? (= 0 (length result)))
	    0
	    (if (and discard-incomplete-parses?
		     (not (= 0 (length result))))
		0
		result))))
    #:unwind? #t
    #:unwind-for-type &parse-error))

;; This function calls parse-line for each line (sentence to parse) in
;; a list It also filters out items if discard-incomplete-parses? is
;; #t or discard-invalid-parses? is #t, or complete parses if
;; discard-complete-parses? is #t
(define (parse-lines discard-incomplete-parses?
		     discard-invalid-parses?
		     discard-complete-parses?
		     lines)
  (filter (lambda (x)
	    (if (number? x)
		(not (= x 0))
		#t))
	  (map
	   (lambda (line)
	     (parse-line discard-incomplete-parses?
			 discard-invalid-parses?
			 discard-complete-parses?
			 line))
	   lines)))

;; Return a list of closing brackets that correctly complete a parse
(define (complete-parse stack)
  (map closing-bracket stack))

;; Score the closing brackets in a completion
;; This returns a list where the tail is the first item found in the
;; navgiation subsystem line.  It doesn't "look" like a proper
;; stack-based parse (reversed), but it is.
;; e.g. (not using #\chars) given '([[{) it returns '(]]})
(define (closing-brackets-score closing-brackets)
  (let ((arbitary-multiplier 5))
    (reduce
     (lambda (x prev)
       (+ (* prev arbitary-multiplier) x))
     0
     (map incomplete-bracket-score closing-brackets))))

;; Find the median entry
;; For a list with an even number of entries, it uses the ceiling to
;; find the middle point
(define (median-score scores)
  (let ((scores-length (length scores)))
    (list-ref (sort scores <) (inexact->exact (round (/ scores-length 2))))))

(define (day10-part1 filename)
  (let ((lines (get-lines filename)))
    (let ((final-value (reduce + 0 (parse-lines #t #f #t lines))))
      (display
       (format #f "day 10 part 1 final-value: ~a\n" final-value)))))

(define (day10-part2 filename)
  (let ((lines (get-lines filename)))
    (let ((final-value
	   (median-score
	    (map closing-brackets-score (map complete-parse (parse-lines #f #t #t lines))))))
      (display
       (format #f "day 10 part 2 final-value: ~a\n" final-value)))))
