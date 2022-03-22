(define-module (aoc y2021 day2 day2)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (aoc port)
  #:use-module (aoc main)
  #:export (parse-commands run-commands run-commands-with-aim final-value
			   day2-part1 day2-part2))


;; Parse a list of commands
;; splits on whitespace, returning a list of (command units) lists
;; Also converts the units element to a number
(define (parse-commands lst)
  ;; convert the units to a number
  (map (lambda (x) (list (car x) (string->number (cadr x))))
       ;; parse into (command unit) lists
       (map (lambda (x) (string-tokenize x char-set:letter+digit)) lst)))


;; Run a list of commands, returning a list of horizontal position and depth
;; (define (run-commands commands)
;;   ;; The first element is the horizontal position, the second element is the depth
;;   (let run-command-helper ((l commands) (result '(0 0)))
;;     (if (nil? l)
;; 	result
;; 	(let ((command-pair (car l)))
;; 	  (let ((command (car command-pair))
;; 		(amount (cadr command-pair))
;; 		(horizontal-position (car result))
;; 		(depth (cadr result)))
;; 	    (cond
;; 	     ((equal? command "forward")
;; 	      (run-command-helper (cdr l) (list (+ horizontal-position amount) depth)))
;; 	     ((equal? command "down")
;; 	      (run-command-helper (cdr l) (list horizontal-position (+ depth amount))))
;; 	     ((equal? command "up")
;; 	      (run-command-helper (cdr l) (list horizontal-position (- depth amount))))))))))

;; version 1 using pattern matching
(define (run-commands commands)
  ;; The first element is the horizontal position, the second element is the depth
  (let run-command-helper ((l commands) (result '(0 0)))
    (if (nil? l)
	result
	(let ((command-pair (car l)))
	  (let ((command (car command-pair))
		(amount (cadr command-pair))
		(horizontal-position (car result))
		(depth (cadr result)))
	    (match command-pair
	      (("forward" amount)
	       (run-command-helper (cdr l) (list (+ horizontal-position amount) depth)))
	      (("down" amount)
	       (run-command-helper (cdr l) (list horizontal-position (+ depth amount))))
	      (("up" amount)
	       (run-command-helper (cdr l) (list horizontal-position (- depth amount))))))))))

;; version with aim 2 using pattern matching
(define (run-commands-with-aim commands)
  ;; The first element is the horizontal position, the second element is the depth,
  ;; the third is the aim
  (let run-command-helper ((l commands) (result '(0 0 0)))
    (if (nil? l)
	result
	(let ((command-pair (car l)))
	  (let ((command (car command-pair))
		(amount (cadr command-pair))
		(horizontal-position (car result))
		(depth (cadr result))
		(aim (caddr result)))
	    (match command-pair
	      (("forward" amount)
	       (run-command-helper
		(cdr l)
		(list (+ horizontal-position amount) (+ depth (* aim amount)) aim)))
	      (("down" amount)
	       (run-command-helper
		(cdr l)
		(list horizontal-position depth (+ aim amount))))
	      (("up" amount)
	       (run-command-helper
		(cdr l)
		(list horizontal-position depth (- aim amount))))))))))

;; Just multiply the values, position times depth
(define (final-value position)
  (* (car position) (cadr position)))

(define (day2-part1 filename)
  (let ((lines (get-lines filename)))
    (display
     (format #f "day 2 part 1 final-value: ~a\n"
	     (final-value (run-commands (parse-commands lines)))))))

(define (day2-part2 filename)
  (let ((lines (get-lines filename)))
    (display
     (format #f "day 2 part 2 final-value: ~a\n"
	     (final-value (run-commands-with-aim (parse-commands lines)))))))

;; (day2 (get-args))
