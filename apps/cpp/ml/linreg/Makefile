CC=g++
CFLAGS=-c -O3 -Wall -I../../ 
LDFLAGS= -O3 -larmadillo
HEADERS= ../OptiML.hpp
SOURCES=linreg.cpp 
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=linreg

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS) 
	       $(CC) $(LDFLAGS) $(OBJECTS) -o $@

$(OBJECTS): $(HEADERS)

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.o *~ $(EXECUTABLE)