GUILE=guile --debug --no-auto-compile

# all: day1 day2 day3
all: y2021

y2021:
	GUILE_LOAD_PATH=. $(GUILE) aoc/y2021/y2021.scm

# day1:
# 	GUILE_LOAD_PATH=. $(GUILE) aoc/y2021/day1/day1.scm -i aoc/y2021/day1/test1.txt

# day2:
# 	GUILE_LOAD_PATH=. $(GUILE) aoc/y2021/day2/day2.scm -i aoc/y2021/day2/test1.txt

# day3:
# 	GUILE_LOAD_PATH=. $(GUILE) aoc/y2021/day3/day3.scm -i aoc/y2021/day3/test1.txt

test:
	GUILE_LOAD_PATH=. $(GUILE) aoc/y2021/day1/tests/test-day1.scm
	GUILE_LOAD_PATH=. $(GUILE) aoc/y2021/day2/tests/test-day2.scm
	GUILE_LOAD_PATH=. $(GUILE) aoc/y2021/day3/tests/test-day3.scm
	GUILE_LOAD_PATH=. $(GUILE) aoc/y2021/day4/tests/test-day4.scm
