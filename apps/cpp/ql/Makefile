CC=g++
CFLAGS= -O3 -Wall -fopenmp -shared -fPIC -std=c++0x -I/usr/lib/jvm/default-java/include -I/usr/lib/jvm/default-java/include/linux  
CPP_SOURCES=q1.cpp 
SCALA_SOURCES=q1.scala Table.scala

all: q1.so Table.class Query1.class

Table.class: $(SCALA_SOURCES)
	scalac $(SCALA_SOURCES)

Query1.class: 
	scalac $(SCALA_SOURCES)

q1.so: $(CPP_SOURCES)
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f *.so *.class
