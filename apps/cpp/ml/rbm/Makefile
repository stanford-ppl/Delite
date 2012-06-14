CC = /opt/intel/bin/icc
LDFLAGS = -c -g -O3 -Wall -I../../
SOURCES = rbm.cpp
OBJECTS = $(SOURCES:.cpp=.o)
EXECUTABLE = rbm

MKL_INCLUDE_PATH = /opt/intel/mkl/include
MKL_LIBS = -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread -lmkl_mc3 -lmkl_def  

all: $(EXECUTABLE)

$(EXECUTABLE): 
	$(CC) -o $@ -I$(MKL_INCLUDE_PATH) rbm.cpp -L/opt/intel/mkl/lib/intel64 -L/opt/intel/lib/intel64 $(MKL_LIBS)

$(OBJECTS): $(HEADERS)

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.o *~ $(EXECUTABLE)
