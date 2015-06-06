CC=g++
CFLAGS=-c -std=gnu++11 -O3 -Wall -I../../ -fopenmp 
LDFLAGS= -O3 -fopenmp
SOURCES=gene_wrangler.cpp 
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=gene_wrangler

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS) 
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

$(OBJECTS): 

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.o *~ $(EXECUTABLE)
