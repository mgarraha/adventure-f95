FC = gfortran
FFLAGS = -fdefault-integer-8 -std=legacy -g
.SUFFIXES:
.SUFFIXES: .f90 .o

all: advent porttest

advent: port.o advn2.o wizcom.o adven.o
	$(FC) -o $@ $^

porttest: port.o advn2.o porttest.o
	$(FC) -o $@ $^

.f90.o:
	$(FC) -c $(FFLAGS) $<

clean:
	rm *.o *.mod
