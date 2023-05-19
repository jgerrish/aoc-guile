GUILE=guile --debug --no-auto-compile

# Run the code regardless of the state of the directory
.PHONY: y2021

all: y2021 test

y2021:
	cd y2021 && $(MAKE) -f Makefile

test:
	mkdir -p log
	cd log && GUILE_LOAD_PATH=../.. $(GUILE) ../tests/test-array.scm
	rm log/*.log
