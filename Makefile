FC = gfortran
FFLAGS = -g
.SUFFIXES:
.SUFFIXES: .f90 .o

all: advent porttest

advent: pdp10.o text.o words.o locations.o objects.o wizcom.o advn2.o adven.o
	$(FC) -o $@ $^

porttest: pdp10.o text.o advn2.o porttest.o
	$(FC) -o $@ $^

porttest.o: pdp10.o text.o advn2.o

adven.o: pdp10.o text.o words.o locations.o objects.o wizcom.o advn2.o

advn2.o: pdp10.o

objects.o: locations.o text.o words.o

locations.o: text.o words.o

text.o: pdp10.o advn2.o

words.o: pdp10.o advn2.o

wizcom.o: text.o advn2.o

.f90.o:
	$(FC) -c $(FFLAGS) $<

clean:
	rm *.o *.mod
