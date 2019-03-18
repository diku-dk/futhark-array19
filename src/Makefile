
.PHONY: all
all: smvm.c.exe

%.c.exe: %.fut
	futhark c -o $@ $<

.PHONY: clean
clean:
	rm *~ *.exe.c *.exe
