(use-modules (aoc y2021 day1 day1))
(use-modules (aoc y2021 day2 day2))
(use-modules (aoc y2021 day3 day3))
(use-modules (aoc y2021 day4 day4))
(use-modules (aoc y2021 day5 day5))
(use-modules (aoc y2021 day7 day7))
(use-modules (aoc y2021 day8 day8))
(use-modules (aoc y2021 day9 day9))
(use-modules (aoc y2021 day10 day10))
(use-modules (aoc y2021 day11 day11))
(use-modules (aoc port))
(use-modules (aoc main))


;; First refactor of data-driven runner
(define days
  (list
   (list '("data" . "day1/test1.txt")
	 (cons "code" (list day1-part1 day1-part2)))
   (list '("data" . "day2/test1.txt")
	 (cons "code" (list day2-part1 day2-part2)))
   (list '("data" . "day3/test1.txt")
	 (cons "code" (list day3-part1 day3-part2)))
   (list '("data" . "day4/test1.txt")
	 (cons "code" (list day4-part1 day4-part2)))
   (list '("data" . "day5/test1.txt")
	 (cons "code" (list day5-part1 day5-part2)))
   (list '("data" . "day7/test1.txt")
	 (cons "code" (list day7-part1 day7-part2)))
   (list '("data" . "day8/test1.txt")
	 (cons "code" (list day8-part1 day8-part2)))
   (list '("data" . "day9/test1.txt")
	 (cons "code" (list day9-part1 day9-part2)))
   (list '("data" . "day10/test1.txt")
	 (cons "code" (list day10-part1 day10-part2)))
   ;; Second refactor of data-driven runner
   (list '("data" . "day11/test1.txt")
	 (cons "code" (list day11-part1 day11-part2)))))
;; Second refactor might use reflection like below
;;	 (cons "code" (module-ref (resolve-module '(aoc y2021 day11 day11)) 'parts)))))


;; Execute the days
(for-each
 (lambda (day)
   (for-each
    (lambda (part)
      (part (assoc-ref day "data")))
    (assoc-ref day "code")))
 days)
