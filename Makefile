CC      = gcc9
FC      = gfortran9
RPATH   = -Wl,-rpath=/usr/local/lib/gcc9/
CFLAGS  = -Wall
FFLAGS  = -Wall $(RPATH) -std=f2008 -Wall -fmax-errors=1 -fcheck=all
LDFLAGS = -I/usr/local/include/ -L/usr/local/lib/
LDLIBS  = -ltokyocabinet -lz -lbz2 -lrt -lpthread -lm -lc

.PHONY: all clean examples

all: tcdb examples

examples:
	$(FC) $(FFLAGS) $(LDFLAGS) -O1 -o hdb examples/hdb/hdb.f90 libc.o tchdb.o $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -O1 -o mdb examples/mdb/mdb.f90 libc.o tcmdb.o $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -O1 -o list examples/list/list.f90 libc.o tclist.o $(LDLIBS)
	$(FC) $(FFLAGS) $(LDFLAGS) -O1 -o tcv examples/tcv/tcv.f90 libc.o tcversion.o tcutil.o $(LDLIBS)

tcdb:
	$(CC) $(CFLAGS) $(LDFLAGS) -c src/tcversion.c
	$(FC) $(FFLAGS) $(LDFLAGS) -c src/libc.f90
	$(FC) $(CFLAGS) $(LDFLAGS) -c src/tcutil.f90
	$(FC) $(CFLAGS) $(LDFLAGS) -c src/tcmisc.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c src/tclist.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c src/tcmdb.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c src/tchdb.f90

clean:
	rm *.mod *.o hdb mdb list tcv
