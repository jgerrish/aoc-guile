;; --- Day 6: Lanternfish ---

;; The sea floor is getting steeper. Maybe the sleigh keys got carried
;; this way?

;; A massive school of glowing lanternfish swims past. They must spawn
;; quickly to reach such large numbers - maybe exponentially quickly?
;; You should model their growth rate to be sure.

;; Although you know nothing about this specific species of
;; lanternfish, you make some guesses about their attributes. Surely,
;; each lanternfish creates a new lanternfish once every 7 days.

;; However, this process isn't necessarily synchronized between every
;; lanternfish - one lanternfish might have 2 days left until it
;; creates another lanternfish, while another might have 4. So, you
;; can model each fish as a single number that represents the number
;; of days until it creates a new lanternfish.

;; Furthermore, you reason, a new lanternfish would surely need
;; slightly longer before it's capable of producing more lanternfish:
;; two more days for its first cycle.

;; So, suppose you have a lanternfish with an internal timer value of
;; 3:

;;     After one day, its internal timer would become 2.
;;     After another day, its internal timer would become 1.
;;     After another day, its internal timer would become 0.
;;     After another day, its internal timer would reset to 6, and it
;;     would create a new lanternfish with an internal timer of 8.
;;     After another day, the first lanternfish would have an internal
;;     timer of 5, and the second lanternfish would have an internal
;;     timer of 7.

;; A lanternfish that creates a new fish resets its timer to 6, not 7
;; (because 0 is included as a valid timer value). The new lanternfish
;; starts with an internal timer of 8 and does not start counting down
;; until the next day.

;; Realizing what you're trying to do, the submarine automatically
;; produces a list of the ages of several hundred nearby lanternfish
;; (your puzzle input). For example, suppose you were given the
;; following list:

;; 3,4,3,1,2

;; This list means that the first fish has an internal timer of 3, the
;; second fish has an internal timer of 4, and so on until the fifth
;; fish, which has an internal timer of 2. Simulating these fish over
;; several days would proceed as follows:

;; Initial state: 3,4,3,1,2
;; After  1 day:  2,3,2,0,1
;; After  2 days: 1,2,1,6,0,8
;; After  3 days: 0,1,0,5,6,7,8
;; After  4 days: 6,0,6,4,5,6,7,8,8
;; After  5 days: 5,6,5,3,4,5,6,7,7,8
;; After  6 days: 4,5,4,2,3,4,5,6,6,7
;; After  7 days: 3,4,3,1,2,3,4,5,5,6
;; After  8 days: 2,3,2,0,1,2,3,4,4,5
;; After  9 days: 1,2,1,6,0,1,2,3,3,4,8
;; After 10 days: 0,1,0,5,6,0,1,2,2,3,7,8
;; After 11 days: 6,0,6,4,5,6,0,1,1,2,6,7,8,8,8
;; After 12 days: 5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8
;; After 13 days: 4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8
;; After 14 days: 3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8
;; After 15 days: 2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7
;; After 16 days: 1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8
;; After 17 days: 0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8
;; After 18 days: 6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8

;; Each day, a 0 becomes a 6 and adds a new 8 to the end of the list,
;; while each other number decreases by 1 if it was present at the
;; start of the day.

;; In this example, after 18 days, there are a total of 26 fish. After
;; 80 days, there would be a total of 5934.

;; Find a way to simulate lanternfish. How many lanternfish would
;; there be after 80 days?

;; --- Part Two ---

;; Suppose the lanternfish live forever and have unlimited food and
;; space.
;; Would they take over the entire ocean?

;; After 256 days in the example above, there would be a total of
;; 26984457539 lanternfish.

;; How many lanternfish would there be after 256 days?

(define-module (aoc y2021 day6 day6)
  #:use-module (srfi srfi-1)
  #:use-module (aoc util)
  #:use-module (aoc port)
  #:use-module (aoc main)
  #:export (parse-line fish-list->fish-count-vector step step-vector run-steps
		       day6-part1 day6-part2))

;; Parse line into a list of numbers
;; Lines are formatted like: "3,4,3,1,2"
;; The function returns a list of the numbers: (3 4 3 1 2)
(define (parse-line lines)
  (map string->number (string-tokenize lines char-set:digit)))


;; Generate a count vector of the number of fish of each age.
;; Accepts a list of lanternfish ages Returns a vector of length nine,
;; where each element is the number of lanternfish of that age.
(define (fish-list->fish-count-vector l)
  (let ((v (make-vector 9 0)))
    (for-each
     (lambda (x)
       (vector-set! v x (1+ (vector-ref v x))))
     l)
    v))

;; Advance one step or day
;; Accepts a list of lanternfish ages, returns a list of the next
;; generation ages.
;; This function preserves the same ordering as the examples
;; But that is not strictly required by the problem.
(define (step lst)
  (let step-helper ((l lst) (old-fish '()) (new-fish '()))
    (if (null? l)
	(append (reverse old-fish) (reverse new-fish))
	(let ((fish (car l))
	      (rst (cdr l)))
	  (if (= fish 0)
	      (step-helper rst (cons 6 old-fish) (cons 8 new-fish))
	      (step-helper rst (cons (1- fish) old-fish) new-fish))))))

;; Advance one step or day
;; Accepts a list of lanternfish ages, returns a list of the next
;; generation ages.
;; This function preserves the same ordering as the examples
;; But that is not strictly required by the problem.
(define (step lst)
  (let step-helper ((l lst) (old-fish '()) (new-fish '()))
    (if (null? l)
	(append (reverse old-fish) (reverse new-fish))
	(let ((fish (car l))
	      (rst (cdr l)))
	  (if (= fish 0)
	      (step-helper rst (cons 6 old-fish) (cons 8 new-fish))
	      (step-helper rst (cons (1- fish) old-fish) new-fish))))))


;; Advance one step or day
;; Accepts a vector of lanternfish counts.
;; The positions or indexes of the vector correspond to ages.  The
;; values in the vector are the number of lanternfish of that age.
;; Returns a vector with the new counts after a step.
(define (step-vector vector)
  (let ((run-off (vector-ref vector 0)))
    (begin
      (do ((i 0 (1+ i)))
	  ((> i 7))
	(vector-set! vector i (vector-ref vector (1+ i))))
      (vector-set! vector 8 run-off)
      (vector-set! vector 6 (+ (vector-ref vector 6) run-off))
      vector)))

;; Run multiple steps
;; Parameters:
;;   Parameter 1: A object representing the lantern fish
;;                For part one, this is a list of lantern fish
;;                For part two, this is a vector of lantern fish counts
;;   Parameter 2: And the number of steps to run
;;   Parameter 3: The step function.  Differs depending on the type of object
;;                (list or vector)
(define (run-steps lf num-steps step-function)
  (let run-steps-helper ((l lf) (n num-steps))
    (if (not (zero? n))
	(run-steps-helper (step-function l) (1- n))
	l)))

;; Compute part 1 for day 6
(define (day6-part1 filename)
  (let ((line (parse-line (car (get-lines filename)))))
    (let ((final-fish (run-steps line 80 step)))
      (let ((result (length final-fish)))
	(display (format #f "day 6 part 1 final-value: ~a\n" result))))))

;; Compute part 1 for day 6
(define (day6-part2 filename)
  (let ((vector (fish-list->fish-count-vector (parse-line (car (get-lines filename))))))
    (let ((final-fish (run-steps vector 256 step-vector)))
      (let ((result (reduce + 0 (vector->list final-fish))))
	(display (format #f "day 6 part 2 final-value: ~a\n" result))))))


;; The parts of the day that should get run
(define parts (list day6-part1 day6-part2))
