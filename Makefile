FC = gfortran
FFLAGS = -fdefault-integer-8 -std=legacy -g
.SUFFIXES:
.SUFFIXES: .f95 .o

all: advent porttest

advent: port.o advn2.o wizcom.o adven.o
	$(FC) -o $@ $^

porttest: port.o advn2.o porttest.o
	$(FC) -o $@ $^

.f95.o:
	$(FC) -c $(FFLAGS) $<

clean:
	rm *.o *.mod
