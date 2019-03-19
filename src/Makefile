DEV ?= AMD

.PHONY: all
all: smvm.c.exe primes.c.exe primes0.c.exe primes_count.c.exe primes_count0.c.exe\
     smvm.opencl.exe primes.opencl.exe primes0.opencl.exe primes_count.opencl.exe primes_count0.opencl.exe

.PHONY: run
run: primes.c.out primes0.c.out \
     primes.opencl.out primes0.opencl.out \
     primes_count.opencl.out primes_count0.opencl.out
	@echo "Times (us) for primes count (expanded) for n=$(shell cat primes_count.in):"
	@cat ./primes_count.opencl.t
	@echo "Times (us) for primes count (non-expanded) for n=$(shell cat primes_count0.in):"
	@cat ./primes_count0.opencl.t

# Execution rules
%.opencl.out: %.opencl.exe %.in
	cat $*.in | ./$< -d $(DEV) -r 10 -t $*.opencl.t > $@

%.c.out: %.c.exe %.in
	cat $*.in | ./$< -t $*.c.t > $@

# Compilation rules
%.c.exe: %.fut
	futhark c -o $@ $<

%.opencl.exe: %.fut
	futhark opencl -o $@ $<

# Cleanup
.PHONY: clean
clean:
	rm -f *~ *.exe.c *.exe *.out
