CC=g++
CFLAGS=-c -g -O3 -Wall -I../../ 
LDFLAGS= -O3 -larmadillo
HEADERS= ../OptiML.hpp
SOURCES=svm.cpp 
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=svm

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS) 
	       $(CC) $(LDFLAGS) $(OBJECTS) -o $@

$(OBJECTS): $(HEADERS)

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.o *~ $(EXECUTABLE)