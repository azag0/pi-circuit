ifeq ($(FC), f77)
FC = gfortran
endif
F2PY ?= f2py
FVENDOR ?= gnu95
FFLAGS ?= -Og -fcheck=all -Wall -Wcharacter-truncation -Wimplicit-procedure \
		  -Wextra -std=f2008ts -pedantic -fall-intrinsics -Wno-maybe-uninitialized

SOURCES = test_mcc.f90 mcc.f90
VPATH = src

all: test_mcc

test: all
	./test_mcc

test_mcc: $(SOURCES:.f90=.o)
	$(FC) $(FFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

test_mcc.o: mcc.o

f2py: libmcc

libmcc: mcc.f90
	$(F2PY) -c --fcompiler=$(FVENDOR) -m $@ $<

clean:
	rm -rf *.mod *.so *.so.dSYM *.o test_mcc
