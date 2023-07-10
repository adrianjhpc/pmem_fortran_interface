SRCFINTERFACE	= ../src/pmem_interface.f90 interface_test.f90
OBJFINTERFACE	=$(SRCFINTERFACE:.f90=.o)
SRCCINTERFACE   = ../src/pmem_interface_utilities.c
OBJCINTERFACE    =$(SRCCINTERFACE:.c=.o)

CC      = gcc 
F90     = gfortran

PMDK_LIBRARY=
PMDK_INCLUDE=

LIBS   = -L${PMDK_LIBRARY} -lpmem

LDFLAGS = #-fopenmp
F90FLAGS = -g -O3 -I${PMDK_INCLUDE} #-fopenmp
F90FLAGSOMP = -g -O3 -I${PMDK_INCLUDE} #-fopenmp

%.o:%.f90	Makefile
	$(F90) -c $(F90FLAGS) $<

%.o:%.c		Makefile
	


main: interface_test 

interface_test:$(OBJCINTERFACE) $(OBJFINTERFACE) Makefile
	$(F90) $(LDFLAGS) -o $@ $(OBJFINTERFACE) $(OBJCINTERFACE) $(LIBS)

clean:
	rm -fr $(TMP) $(OBJCINTERFACE) $(OBJFINTERFACE) core interface_test 
