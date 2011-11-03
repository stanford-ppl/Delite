CC = /opt/intel/bin/icc
LDFLAGS = -c -g -O3 -Wall -I../../
SOURCES = rbm.cpp
OBJECTS = $(SOURCES:.cpp=.o)
EXECUTABLE = rbm

MKL_INCLUDE_PATH = /opt/intel/mkl/include#/opt/intel/mkl/10.2.5.035/include
#MKL_LIBRARY_PATH = /opt/intel/mkl/lib/intel64:/opt/intel/lib/intel64#/opt/intel/mkl/10.2.5.035/lib/em64t:/opt/intel/lib/intel64

MKL_LIBS = -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lmkl_mc3 -lmkl_def  
#  -lmkl_scalapack_lp64 \
#  -lmkl_intel_lp64 \
#  -lmkl_blacs_openmpi_lp64 \
#  -lmkl_sequential \
#  -lmkl_core \
#  -lpthread

all: $(EXECUTABLE)

$(EXECUTABLE): 
	$(CC) -o $@ -I$(MKL_INCLUDE_PATH) rbm.cpp -L/opt/intel/mkl/lib/intel64 -L/opt/intel/lib/intel64 $(MKL_LIBS)

$(OBJECTS): $(HEADERS)

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.o *~ $(EXECUTABLE)
