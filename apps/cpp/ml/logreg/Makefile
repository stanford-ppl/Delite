CC=g++
CFLAGS=-c -O3 -Wall -I../../ -fopenmp 
LDFLAGS= -O3 -fopenmp
HEADERS= ../OptiML.hpp
SOURCES=logreg.cpp 
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=logreg

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS) 
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

$(OBJECTS): $(HEADERS)

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.o *~ $(EXECUTABLE)
