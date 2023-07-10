SRCFINTERFACE	= pmem_interface.f90
OBJFINTERFACE	=$(SRCFINTERFACE:.f90=.o)
SRCCINTERFACE   = pmem_interface_utilities.c
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
	$(CC) -c $(CFLAGS) $<


main: interface_test streams_memory streams_persistent_memory

interface:$(OBJCINTERFACE) $(OBJFINTERFACE) Makefile

clean:
	rm -fr $(TMP) $(OBJCINTERFACE) $(OBJFINTERFACE)
