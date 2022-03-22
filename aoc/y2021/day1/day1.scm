(define-module (aoc y2021 day1 day1)
  #:use-module (srfi srfi-1)
  #:use-module (aoc port)
  #:use-module (aoc main)
  #:export (find-increasing count-increasing sliding-window reduce-sliding-window
			    day1-part1 day1-part2))

;; Find elements in the list that are larger than the previous element
;; Return a list of (number bool) elements
;; #t for every element that is larger than the previous
;; #f for every element that is smaller or the same
;; The list returned is reversed
;; The second item of the last element in the returned list will be false
;; Because the first element in the initial list doesn't have a precursor
(define (find-increasing lines)
  (fold
   (lambda (elem prev)
     ;; (display (format #f "elem: ~a, prev: ~a\n" elem prev))
     (let ((current-number elem))
       (if (> current-number (car (car prev)))
	   (cons (list current-number #t) prev)
	   (cons (list current-number #f) prev))))
   (list (list (car lines) #f))
   (cdr lines)))

;; Count the items that are increasing
(define (count-increasing increasing-list)
  (length (filter (lambda (x) (second x)) increasing-list)))


;; Get a set of sliding windows on a list
;; This returns a a list of lists, where each element in the list are n consecutive
;; elements in the original list
(define (sliding-window lst n)
  (reverse
   (let sliding-window-take ((l lst) (result '()))
     (if (< (length l) n)
	 result
	 (sliding-window-take (cdr l) (cons (take l n) result))))))

;; Reduce the sliding-window list of lists to a list of sums
(define (reduce-sliding-window lst)
  (map (lambda (l) (reduce + 0 l)) lst))

(define (day1-part1 filename)
  (let ((lines (get-lines filename)))
    (begin
      ;; (display (format #f "lines: ~a\n" lines))
      (display
       (format #f "day 1 part 1 final-value: ~a\n"
	       (count-increasing (find-increasing (strings-to-numbers lines))))))))

(define (day1-part2 filename)
  (let ((lines (get-lines filename)))
    (begin
      ;; (display (format #f "lines: ~a\n" lines))
      (display
       (format #f "day 1 part 2 final-value: ~a\n"
	       (count-increasing
		(find-increasing
		 (reduce-sliding-window (sliding-window (strings-to-numbers lines) 3)))))))))

;; (display (format #f "get-args: ~a" (get-args)))
; (day1 (get-args))
