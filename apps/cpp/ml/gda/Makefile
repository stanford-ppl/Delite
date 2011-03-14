CC=g++
CFLAGS=-c -O3 -Wall -I../../ -I../../../../../../boost/boost_1_46_0
LDFLAGS= -O3
HEADERS= ../OptiML.hpp
SOURCES=GDA.cpp 
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=GDA

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS) 
	       $(CC) $(LDFLAGS) $(OBJECTS) -o $@

$(OBJECTS): $(HEADERS)

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.o *~ $(EXECUTABLE)