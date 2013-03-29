CC=g++
CFLAGS=-fopenmp -pthread -c -O3 -Wall -I../../ 
LDFLAGS= -O3 -fopenmp -pthread
HEADERS= ../OptiML.hpp
SOURCES=kmeans.cpp 
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=kmeans

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS) 
	       $(CC) $(LDFLAGS) $(OBJECTS) -o $@

$(OBJECTS): $(HEADERS)

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.o *~ $(EXECUTABLE)
