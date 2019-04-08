DEV ?= AMD

.PHONY: sync
sync:
	futhark pkg sync

.PHONY: all
all: smvm.c.exe primes.c.exe primes0.c.exe primes_count.c.exe primes_count0.c.exe \
     smvm.opencl.exe primes.opencl.exe primes0.opencl.exe primes_count.opencl.exe primes_count0.opencl.exe \
     primes.plainc.exe

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

%.plainc.out: %.plainc.exe %.in
	cat $*.in | time ./$< > $@ 2> $*.plainc.t

# Compilation rules
%.c.exe: %.fut
	futhark c -o $@ $<

%.opencl.exe: %.fut
	futhark opencl -o $@ $<

%.plainc.exe: %.c
	gcc -o $@ $< -lm -std=c99

# Cleanup
.PHONY: clean
clean:
	rm -rf *~ *.exe.c *.exe *.out *.t smvm_data

# Experiments with sparse matrix - vector multiplication
.PHONY: smvm_run
smvm_run:
#	rm -rf smvm_data
#	make smvm_data/smvm_tst1.c.out smvm_data/smvm_tst2.c.out smvm_data/smvm_tst3.c.out smvm_data/smvm_tst4.c.out smvm_data/smvm_tst5.c.out \
#	     smvm_data/smvm_tst1.opencl.out smvm_data/smvm_tst2.opencl.out smvm_data/smvm_tst3.opencl.out smvm_data/smvm_tst4.opencl.out smvm_data/smvm_tst5.opencl.out
	make smvm_data/smvm_tst1.c.tmavg smvm_data/smvm_tst2.c.tmavg smvm_data/smvm_tst3.c.tmavg smvm_data/smvm_tst4.c.tmavg smvm_data/smvm_tst5.c.tmavg \
	     smvm_data/smvm_tst1.opencl.tmavg smvm_data/smvm_tst2.opencl.tmavg smvm_data/smvm_tst3.opencl.tmavg smvm_data/smvm_tst4.opencl.tmavg smvm_data/smvm_tst5.opencl.tmavg

# SMVM Data Generation
ROWS=8000

.PHONY: smvm_report
smvm_report:
	@echo "SMVM report"
	@echo "C 1-5 (density)"
	@cat smvm_data/smvm_tst1.c.tmavg smvm_data/smvm_tst2.c.tmavg smvm_data/smvm_tst3.c.tmavg smvm_data/smvm_tst4.c.tmavg smvm_data/smvm_tst5.c.tmavg
	@echo "OpenCL 1-5 (density)"
	@cat smvm_data/smvm_tst1.opencl.tmavg smvm_data/smvm_tst2.opencl.tmavg smvm_data/smvm_tst3.opencl.tmavg smvm_data/smvm_tst4.opencl.tmavg smvm_data/smvm_tst5.opencl.tmavg

smvm_data/smvm_tst%.tmavg: smvm_data/smvm_tst%.out Makefile
	cat smvm_data/smvm_tst$*.tm | awk 'ttt += $$1  {print ttt/NR/1000}' | tail -1 > $@

smvm_data/smvm_tst%.in: smvm.c.exe smvm_data
	echo 21 $(ROWS) $* | ./$< -e gen_square_csr_and_vector > $@

smvm_data/smvm_tst%.c.out: smvm_data/smvm_tst%.in smvm.c.exe
	cat $< | ./smvm.c.exe -r 10 -t smvm_data/smvm_tst$*.c.tm -e test_smvm > $@

smvm_data/smvm_tst%.opencl.out: smvm_data/smvm_tst%.in smvm.opencl.exe
	cat $< | ./smvm.opencl.exe -d AMD -r 10 -t smvm_data/smvm_tst$*.opencl.tm -e test_smvm > $@

smvm_data: smvm.c.exe
	mkdir -p smvm_data
