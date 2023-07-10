SRCMYSECOND   = my_second.c
OBJMYSECOND   =$(SRCMYSECOND:.c=.o)

SRCFINTERFACE	= ../src/pmem_interface.f90 
OBJFINTERFACE	=$(SRCFINTERFACE:.f90=.o)
SRCCINTERFACE   = ../src/pmem_interface_utilities.c
OBJCINTERFACE    =$(SRCCINTERFACE:.c=.o)

SRCSTREAMS    = streams_memory.f90
OBJSTREAMS    =$(SRCSTREAMS:.f90=.o)

SRCSTREAMSPERSISTENT    = ../src/pmem_interface.f90 streams_persistent_memory.f90
OBJSTREAMSPERSISTENT    =$(SRCSTREAMSPERSISTENT:.f90=.o)

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


main: streams_memory streams_persistent_memory

streams_memory:$(OBJSTREAMS) $(OBJMYSECOND) $(OBJCINTERFACE) $(OBJFINTERFACE) Makefile
	$(F90) $(LDFLAGS) -o $@ $(OBJSTREAMS) $(OBJMYSECOND) $(LIBS)

streams_persistent_memory:$(OBJSTREAMSPERSISTENT) $(OBJMYSECOND) $(OBJCINTERFACE) $(OBJFINTERFACE) Makefile
	$(F90) $(LDFLAGS) -o $@ $(OBJSTREAMSPERSISTENT) $(OBJMYSECOND) $(OBJCINTERFACE) $(LIBS)

clean:
	rm -fr $(TMP) $(OBJCINTERFACE) $(OBJFINTERFACE) $(OBJSTREAMS) $(OBJSTREAMSPERSISTENT) $(OBJMYSECOND) core streams_memory streams_persistent_memory
