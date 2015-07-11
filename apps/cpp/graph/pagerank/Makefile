

all: pagerank
.PHONY: all

pagerank: pagerank.cpp ../graph.cpp ../graph.hpp ../timer.hpp
	g++ -std=gnu++11 -O3 -fopenmp -o pagerank ../graph.cpp pagerank.cpp

