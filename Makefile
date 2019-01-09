FC = gfortran
FFLAGS = -fdefault-integer-8 -g
.SUFFIXES:
.SUFFIXES: .f90 .o

all: advent porttest

advent: pdp10.o text.o advn2.o wizcom.o places.o adven.o
	$(FC) -o $@ $^

porttest: pdp10.o text.o advn2.o porttest.o
	$(FC) -o $@ $^

advn2.o: pdp10.o text.o

text.o: pdp10.o

wizcom.o: text.o advn2.o

.f90.o:
	$(FC) -c $(FFLAGS) $<

clean:
	rm *.o *.mod
