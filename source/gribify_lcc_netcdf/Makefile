PROGRAM=gribify_lcc_netcdf
CC=gcc
G2CDIR=g2clib-1.4.0
HDFDIR=hdf5
NETCDFDIR=netcdf
CFLAGS=-Wall -I./$(G2CDIR) -I./$(HDFDIR)/include -I./$(NETCDFDIR)/include
OBJS=$(PROGRAM).o
LIBS=-Lhdf5/lib -Lnetcdf/lib -lm -ljasper -lpng -lnetcdf $(G2CDIR)/libgrib2c.a

$(PROGRAM):$(OBJS)
	$(CC) -o $@ $< $(LIBS)
