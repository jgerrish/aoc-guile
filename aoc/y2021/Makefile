GUILE=guile --debug --no-auto-compile

# all: day1 day2 day3
all: y2021

y2021:
	GUILE_LOAD_PATH=. $(GUILE) aoc/y2021/y2021.scm

test:
	cd log && GUILE_LOAD_PATH=.. $(GUILE) ../aoc/y2021/day1/tests/test-day1.scm
	cd log && GUILE_LOAD_PATH=.. $(GUILE) ../aoc/y2021/day2/tests/test-day2.scm
	cd log && GUILE_LOAD_PATH=.. $(GUILE) ../aoc/y2021/day3/tests/test-day3.scm
	cd log && GUILE_LOAD_PATH=.. $(GUILE) ../aoc/y2021/day4/tests/test-day4.scm
	cd log && GUILE_LOAD_PATH=.. $(GUILE) ../aoc/y2021/day5/tests/test-day5.scm
	cd log && GUILE_LOAD_PATH=.. $(GUILE) ../aoc/y2021/day7/tests/test-day7.scm
	cd log && GUILE_LOAD_PATH=.. $(GUILE) ../aoc/y2021/day8/tests/test-day8.scm
	cd log && GUILE_LOAD_PATH=.. $(GUILE) ../aoc/y2021/day9/tests/test-day9.scm
	cd log && GUILE_LOAD_PATH=.. $(GUILE) ../aoc/y2021/day10/tests/test-day10.scm
	rm log/*.log
