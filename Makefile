all: y2021 test

y2021:
	cd aoc/y2021 && $(MAKE) -f Makefile

test:
	cd aoc && $(MAKE) -f Makefile test
	cd aoc/y2021 && $(MAKE) -f Makefile test
