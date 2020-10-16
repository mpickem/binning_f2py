SHELL = /bin/bash
F2PY  = f2py
FC    = gfortran

all: binning.so

binning.so: binning.f90
	$(F2PY) -c -m binning $^ --fcompiler=gnu95 $(LIBS) -DF2PY_REPORT_ON_ARRAY_COPY=1 --quiet

%.o: %.f90
	$(FC) -c $< -o $@

.PHONY: clean
clean:
	rm -f *.so *.o *.mod *.pyc *.pyf
