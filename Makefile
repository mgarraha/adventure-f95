FC = gfortran
FFLAGS = -fdefault-integer-8 -std=legacy -g

all: advent porttest

advent: port.o advn2.o adven.o
	$(FC) -o $@ $^

porttest: port.o advn2.o porttest.o
	$(FC) -o $@ $^

.f.o:
	$(FC) -c $(FFLAGS) $<

clean:
	rm advent porttest *.o
