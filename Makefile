ifeq ($(FC), f77)
FC = gfortran
endif
FVENDOR ?= gnu95
FFLAGS ?= -O2 -std=f2018 -pedantic -Wall -Wno-uninitialized

SOURCES = test_mcc.f90 mcc.f90
VPATH = src

all: test_mcc

test: test_mcc
	./$^

test_mcc: $(SOURCES:.f90=.o)
	$(FC) $(FFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

test_mcc.o: mcc.o

f2py: libmcc

libmcc: mcc.f90
	python -m numpy.f2py -c --fcompiler=$(FVENDOR) -m $@ $<

clean:
	rm -rf *.mod *.so *.so.dSYM *.o test_mcc

distclean: clean
	rm -rf venv
