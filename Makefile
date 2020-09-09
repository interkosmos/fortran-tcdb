CC      = gcc
FC      = gfortran
AR      = ar
PREFIX  = /usr/local
CFLAGS  = -Wall
FFLAGS  = -Wall -std=f2008 -Wall -fmax-errors=1 -fcheck=all
ARFLAGS = rcs
LDFLAGS = -I$(PREFIX)/include/ -L$(PREFIX)/lib/
LDLIBS  = -ltokyocabinet -lz -lbz2 -lrt -lpthread -lm -lc
TARGET  = libfortran-tcdb.a

.PHONY: all clean examples static

all: $(TARGET) examples

$(TARGET):
	$(CC) $(CFLAGS) -fPIC $(LDFLAGS) -c src/tcversion.c
	$(FC) $(FFLAGS) -fPIC $(LDFLAGS) -c src/libc.f90
	$(FC) $(CFLAGS) -fPIC $(LDFLAGS) -c src/tcutil.f90
	$(FC) $(CFLAGS) -fPIC $(LDFLAGS) -c src/tcmisc.f90
	$(FC) $(FFLAGS) -fPIC $(LDFLAGS) -c src/tclist.f90
	$(FC) $(FFLAGS) -fPIC $(LDFLAGS) -c src/tcmdb.f90
	$(FC) $(FFLAGS) -fPIC $(LDFLAGS) -c src/tchdb.f90
	$(AR) $(ARFLAGS) $(TARGET) *.o

examples: $(TARGET)
	$(FC) $(FFLAGS) $(LDFLAGS) -o hdb examples/hdb/hdb.f90 $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o mdb examples/mdb/mdb.f90 $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o list examples/list/list.f90 $(TARGET) $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o tcv examples/tcv/tcv.f90 $(TARGET) $(LDLIBS)

static: $(TARGET)

clean:
	rm $(TARGET) *.mod *.o hdb mdb list tcv
