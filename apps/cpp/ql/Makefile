CC=g++
CFLAGS= -O3 -Wall -fopenmp -shared -fPIC -std=c++0x -I/home/kjbrown/java/jdk1.7.0_b147_x86_64/include -I/home/kjbrown/java/jdk1.7.0_b147_x86_64/include/linux  
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
