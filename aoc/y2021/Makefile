GUILE=guile --debug --no-auto-compile

all:
	GUILE_LOAD_PATH=../.. $(GUILE) y2021.scm

test:
	mkdir -p log
	cd log && GUILE_LOAD_PATH=../../.. $(GUILE) ../day1/tests/test-day1.scm
	cd log && GUILE_LOAD_PATH=../../.. $(GUILE) ../day2/tests/test-day2.scm
	cd log && GUILE_LOAD_PATH=../../.. $(GUILE) ../day3/tests/test-day3.scm
	cd log && GUILE_LOAD_PATH=../../.. $(GUILE) ../day4/tests/test-day4.scm
	cd log && GUILE_LOAD_PATH=../../.. $(GUILE) ../day5/tests/test-day5.scm
	cd log && GUILE_LOAD_PATH=../../.. $(GUILE) ../day6/tests/test-day6.scm
	cd log && GUILE_LOAD_PATH=../../.. $(GUILE) ../day7/tests/test-day7.scm
	cd log && GUILE_LOAD_PATH=../../.. $(GUILE) ../day8/tests/test-day8.scm
	cd log && GUILE_LOAD_PATH=../../.. $(GUILE) ../day9/tests/test-day9.scm
	cd log && GUILE_LOAD_PATH=../../.. $(GUILE) ../day10/tests/test-day10.scm
	cd log && GUILE_LOAD_PATH=../../.. $(GUILE) ../day11/tests/test-day11.scm
	cd log && GUILE_LOAD_PATH=../../.. $(GUILE) ../day12/tests/test-day12.scm
	cd log && GUILE_LOAD_PATH=../../.. $(GUILE) ../day13/tests/test-day13.scm
	rm log/*.log
