.PHONY: all
all: lines.py linez.py

.PHONY: lib
lib: futhark.pkg
	futhark pkg sync

lines.py: lines.fut lib
	futhark pyopencl --library $<

linez.py: linez.fut lib
	futhark pyopencl --library $<

.PHONY: run
run: lines.py linez.py
	python lines-gui.py

.PHONY: runz
runz: linez.py
	python linez-gui.py

.PHONY: clean
clean:
	rm -f lines.py *~ *.c lines.pyc linez.py linez.pyc *.h
