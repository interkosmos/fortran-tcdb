FC      = gfortran8
RPATH   = -Wl,-rpath=/usr/local/lib/gcc8/
FFLAGS  =  $(RPATH) -std=f2008 -Wall -fmax-errors=1 -fcheck=all
LDFLAGS = -I/usr/local/include/ -L/usr/local/lib/
LDLIBS  = -ltokyocabinet -lz -lbz2 -lrt -lpthread -lm -lc

SOURCE  = tcdb.f90
TARGET  = tcdb.o

.PHONY: all clean examples

all: $(TARGET) examples

tcdb: $(TARGET)

examples:
	$(FC) $(FFLAGS) $(LDFLAGS) -O1 -o hdb examples/hdb/hdb.f90 $(TARGET) $(LDLIBS)

$(TARGET):
	$(FC) $(FFLAGS) $(LDFLAGS) -c src/$(SOURCE)

clean:
	rm *.mod $(TARGET) hdb
