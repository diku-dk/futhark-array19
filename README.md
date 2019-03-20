# futhark-array19
Experimental repository for ARRAY '19 submission.

Build
-----

To build the non-gui executables, do the following:
````
$ cd src
$ make
````

The various executables can be executed and timed with `make run`.

Lines
-----

To run the lines and linez examples, which features the flattened
version of line drawing and triangle drawing, do as follows:
````
$ cd src/lines
$ make runz
````

(and select the graphics card you want to use).

Primes
------

The sequential plain-c version (`src/primes.c`) is currently faster on
my MacBook Pro 2015 (AMD GPU):

````
$ echo 100000000 | time ./primes_count.opencl.exe -d AMD
5761455i32
        3.03 real         0.02 user         0.81 sys
$ echo 100000000 | time ./primes.plainc.exe
5761455
        1.92 real         1.82 user         0.09 sys
````

Similar results are obtained with the `-t` option.