;; --- Day 7: The Treachery of Whales ---
;; A giant whale has decided your submarine is its next meal, and it's
;; much faster than you are. There's nowhere to run!

;; Suddenly, a swarm of crabs (each in its own tiny submarine - it's
;; too deep for them otherwise) zooms in to rescue you! They seem to
;; be preparing to blast a hole in the ocean floor; sensors indicate a
;; massive underground cave system just beyond where they're aiming!

;; The crab submarines all need to be aligned before they'll have
;; enough power to blast a large enough hole for your submarine to get
;; through. However, it doesn't look like they'll be aligned before
;; the whale catches you! Maybe you can help?

;; There's one major catch - crab submarines can only move
;; horizontally.

;; You quickly make a list of the horizontal position of each crab
;; (your puzzle input). Crab submarines have limited fuel, so you need
;; to find a way to make all of their horizontal positions match while
;; requiring them to spend as little fuel as possible.

;; For example, consider the following horizontal positions:

;; 16,1,2,0,4,2,7,1,2,14
;; This means there's a crab with horizontal position 16, a crab with
;; horizontal position 1, and so on.

;; Each change of 1 step in horizontal position of a single crab costs
;; 1 fuel. You could choose any horizontal position to align them all
;; on, but the one that costs the least fuel is horizontal position 2:

;; Move from 16 to 2: 14 fuel
;; Move from 1 to 2: 1 fuel
;; Move from 2 to 2: 0 fuel
;; Move from 0 to 2: 2 fuel
;; Move from 4 to 2: 2 fuel
;; Move from 2 to 2: 0 fuel
;; Move from 7 to 2: 5 fuel
;; Move from 1 to 2: 1 fuel
;; Move from 2 to 2: 0 fuel
;; Move from 14 to 2: 12 fuel
;; This costs a total of 37 fuel. This is the cheapest possible
;; outcome; more expensive outcomes include aligning at position 1 (41
;; fuel), position 3 (39 fuel), or position 10 (71 fuel).

;; Determine the horizontal position that the crabs can align to using
;; the least fuel possible. How much fuel must they spend to align to
;; that position?

;; --- Part Two ---
;; The crabs don't seem interested in your proposed solution. Perhaps
;; you misunderstand crab engineering?

;; As it turns out, crab submarine engines don't burn fuel at a
;; constant rate. Instead, each change of 1 step in horizontal
;; position costs 1 more unit of fuel than the last: the first step
;; costs 1, the second step costs 2, the third step costs 3, and so
;; on.

;; As each crab moves, moving further becomes more expensive. This
;; changes the best horizontal position to align them all on; in the
;; example above, this becomes 5:

;; Move from 16 to 5: 66 fuel
;; Move from 1 to 5: 10 fuel
;; Move from 2 to 5: 6 fuel
;; Move from 0 to 5: 15 fuel
;; Move from 4 to 5: 1 fuel
;; Move from 2 to 5: 6 fuel
;; Move from 7 to 5: 3 fuel
;; Move from 1 to 5: 10 fuel
;; Move from 2 to 5: 6 fuel
;; Move from 14 to 5: 45 fuel
;; This costs a total of 168 fuel. This is the new cheapest possible
;; outcome; the old alignment position (2) now costs 206 fuel instead.

;; Determine the horizontal position that the crabs can align to using
;; the least fuel possible so they can make you an escape route! How
;; much fuel must they spend to align to that position?
(define-module (aoc y2021 day7 day7)
  #:use-module (srfi srfi-1)
  #:use-module (aoc port)
  #:use-module (aoc main)
  #:export (parse-lines
	    get-range find-distance find-distances cost-function search-next-step
	    search-range day7-part1 day7-part2))


;; Parse lines into lists of numbers
(define (parse-lines lines)
  (map (lambda (x) (map string->number (string-tokenize x char-set:digit))) lines))

;; Get the range to test
;; This finds the min and maximum values and builds a range from that
;; This returns an ordered list of items to test
;; Heuristics can be inserted in here to re-order this list
(define (get-range numbers)
  (if (null? numbers)
      '()
      (let ((min-value (apply min numbers))
	    (max-value (apply max numbers)))
	(iota (1+ (- max-value min-value)) min-value))))

;; Find the distance between two numbers
;; Just returns the absolute value of the difference
(define (find-distance old-position new-position)
  (abs (- old-position new-position)))

;; Find the distance between each number in a list of numbers and
;; a new position.
;; This currently has no optimizations implemented.  One optimization
;; to implement is short-circuiting the search, aka pruning, if the
;; current search path exceeds the previous minimum score found, abort
;; the search.
;;
;; This could be a good place to use Scheme / Guile continuations.
;;
;; We could yield results one-at-a-time, or raise an exception and
;; return to the calling context if the minimum is exceeded
;; But to keep it simple, we'll just use loops and breaks
(define (find-distances numbers new-position)
  (map (lambda (old-position) (find-distance old-position new-position)) numbers))

;; Determine the cost function to use
;; part is the problem part (1 or 2)
;; distances is the list of the number of horizontal moves a crab
;; has to make
;; For part1, it's a simple sum of the number of moves to change
;; position: (num_moves)
;; For part2, it's sum of the first n integers: (1 + 2 + 3 + ... + num_moves)
(define (cost-function part)
    (if (= part 1)
	(lambda (distances)
	  (reduce + 0 distances))
	(lambda (distances)
	  (reduce + 0 (map (lambda (x) (/ (* x (1+ x)) 2)) distances)))))

;; Search the next step in the search space
;; This only searches a single level in horizontal space
;; Returns the cost of all the crabs moving to the level given
;; If the cost exceeds the current-lowest-cost, it immediately returns
;; the current result
(define (search-next-step numbers level current-lowest-cost cost-function)
  ;; Find the costs for the next horizontal position in the
  ;; search space.
  ;; First, find the distances (number of horizontal moves)
  ;; for each crab to move to the next horizontal position.
  (if (null? numbers)
      0
      (let search ((number (car numbers)) (lst (cdr numbers)) (current-cost 0))
	;; Find the distance between this crab and the level
	(let ((distance (find-distance number level)))
	  ;; Find the cost for that distance
	  (let ((cost (+ current-cost (cost-function (list distance)))))
	    (if (null? lst)
		cost
		(if (> cost current-lowest-cost)
		    cost
		    (search (car lst) (cdr lst) cost))))))))

;; Search through the solutions
;; numbers are the crabs' initial positions
;; range is the list of positions to search through
(define (search-range numbers range cost-function)
  ;; Search through the solutions.
  ;; number is the current horizontal position we're testing
  ;; lst is the rest of the horizontal positions to search through
  ;; best-soln is a list of the best solution found so far and the
  ;; cost for that solution
  ;;   best-soln / the value function is the total sum of the
  ;;   distances
  ;;   we set the initial value of best-soln to the maximum number
  ;;   times the number of values
  ;;
  ;; Returns a list with the item in the range that has the lowest
  ;; cost and the lowest cost itself.
  (if (or (null? numbers) (null? range))
      '()
      ;; First, build a highest possible cost / sentinel value
      (let ((highest-possible-cost
	     (cost-function (make-list (length numbers) (apply max numbers)))))
	(let search ((number (car range)) (lst (cdr range))
		     (best-soln (list (apply max numbers) highest-possible-cost)))
	  (if (null? lst)
	      best-soln
	      (let ((cost (search-next-step numbers number (cadr best-soln) cost-function)))
		(if (< cost (cadr best-soln))
		    (search (car lst) (cdr lst) (list number cost))
		    (search (car lst) (cdr lst) best-soln))))))))

;; Calculate the final value
(define (day7-part1 filename)
  (let ((lines (car (parse-lines (get-lines filename)))))
    (let ((soln (search-range lines (get-range lines) (cost-function 1))))
      (display
       (format #f "day 7 part 1 best-position: ~a, cost: ~a\n" (car soln) (cadr soln))))))

;; Calculate the final value
(define (day7-part2 filename)
  (let ((lines (car (parse-lines (get-lines filename)))))
    (let ((soln (search-range lines (get-range lines) (cost-function 2))))
      (display
       (format #f "day 7 part 1 best-position: ~a, cost: ~a\n" (car soln) (cadr soln))))))
