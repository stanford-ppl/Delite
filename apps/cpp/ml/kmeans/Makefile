CC=g++
CFLAGS=-c -O3 -Wall -I../../ 
LDFLAGS= -O3
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
