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
	rm -rf *~ *.exe.c *.exe *.out *.t smvm_data/*.out smvm_data/*.tm smvm_data/*.tmavg

# SMVM Data Generation
ROWS=10000

# Experiments with sparse matrix - vector multiplication
.PHONY: smvm_prepare
smvm_prepare:
	rm -rf smvm_data
	make smvm_data/smvm_tst1.in smvm_data/smvm_tst2.in smvm_data/smvm_tst3.in smvm_data/smvm_tst4.in \
             smvm_data/smvm_tst5.in smvm_data/smvm_tst10.in smvm_data/smvm_tst15.in smvm_data/smvm_tst20.in \
             smvm_data/smvm_dense.in

# Assumes that "make smvm_prepare" has been run
.PHONY: smvm_run
smvm_run: smvm_data/smvm_tst1.c.tmavg smvm_data/smvm_tst2.c.tmavg smvm_data/smvm_tst3.c.tmavg \
          smvm_data/smvm_tst4.c.tmavg smvm_data/smvm_tst5.c.tmavg smvm_data/smvm_tst10.c.tmavg \
          smvm_data/smvm_tst15.c.tmavg smvm_data/smvm_tst20.c.tmavg \
          smvm_data/smvm_tst1.opencl.tmavg smvm_data/smvm_tst2.opencl.tmavg \
          smvm_data/smvm_tst3.opencl.tmavg smvm_data/smvm_tst4.opencl.tmavg \
          smvm_data/smvm_tst5.opencl.tmavg smvm_data/smvm_tst10.opencl.tmavg \
          smvm_data/smvm_tst15.opencl.tmavg smvm_data/smvm_tst20.opencl.tmavg \
          smvm_data/smvm_dense.c.tmavg smvm_data/smvm_dense.opencl.tmavg

# Assumes that "make smvm_run" has been run
# Run "make clean && make smvm_run && make smvm_report"
.PHONY: smvm_report
smvm_report:
	@echo "Density (%)  & Futhark C (ms)   & Futhark OpenCL (ms) \\\\\\"
	@echo "1            & `cat smvm_data/smvm_tst1.c.tmavg`         &  `cat smvm_data/smvm_tst1.opencl.tmavg` \\\\\\"
	@echo "2            & `cat smvm_data/smvm_tst2.c.tmavg`         &  `cat smvm_data/smvm_tst2.opencl.tmavg` \\\\\\"
	@echo "3            & `cat smvm_data/smvm_tst3.c.tmavg`         &  `cat smvm_data/smvm_tst3.opencl.tmavg` \\\\\\"
	@echo "4            & `cat smvm_data/smvm_tst4.c.tmavg`         &  `cat smvm_data/smvm_tst4.opencl.tmavg` \\\\\\"
	@echo "5            & `cat smvm_data/smvm_tst5.c.tmavg`         &  `cat smvm_data/smvm_tst5.opencl.tmavg` \\\\\\"
	@echo "10           & `cat smvm_data/smvm_tst10.c.tmavg`        &  `cat smvm_data/smvm_tst10.opencl.tmavg` \\\\\\"
	@echo "15           & `cat smvm_data/smvm_tst15.c.tmavg`        &  `cat smvm_data/smvm_tst15.opencl.tmavg` \\\\\\"
	@echo "20           & `cat smvm_data/smvm_tst20.c.tmavg`        &  `cat smvm_data/smvm_tst20.opencl.tmavg` \\\\\\"
	@echo "Dense        & `cat smvm_data/smvm_dense.c.tmavg`        &  `cat smvm_data/smvm_dense.opencl.tmavg` "

smvm_data/smvm_%.tmavg: smvm_data/smvm_%.out Makefile
	cat smvm_data/smvm_$*.tm | awk 'ttt += $$1  {print ttt/NR/1000}' | tail -1 > $@

smvm_data/smvm_tst%.in: smvm.c.exe smvm_data
	echo 21 $(ROWS) $* | ./$< -e gen_square_csr_and_vector -b > $@

smvm_data/smvm_dense.in: smvm.c.exe smvm_data
	echo 21 $(ROWS) | ./$< -e gen_dense -b > $@

smvm_data/smvm_tst%.c.out: smvm.c.exe
	cat smvm_data/smvm_tst$*.in | ./smvm.c.exe -r 10 -t smvm_data/smvm_tst$*.c.tm -e test_smvm > $@

smvm_data/smvm_tst%.opencl.out: smvm.opencl.exe
	cat smvm_data/smvm_tst$*.in | ./smvm.opencl.exe -d AMD -r 10 -t smvm_data/smvm_tst$*.opencl.tm -e test_smvm > $@

smvm_data/smvm_dense.c.out: smvm.c.exe
	cat smvm_data/smvm_dense.in | ./smvm.c.exe -r 10 -t smvm_data/smvm_dense.c.tm -e test_dense > $@

smvm_data/smvm_dense.opencl.out: smvm.opencl.exe
	cat smvm_data/smvm_dense.in | ./smvm.opencl.exe -d AMD -r 10 -t smvm_data/smvm_dense.opencl.tm -e test_dense > $@

smvm_data: smvm.c.exe
	mkdir -p smvm_data
