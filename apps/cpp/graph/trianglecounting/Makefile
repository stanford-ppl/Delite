

all: trianglecounting
.PHONY: all

trianglecounting: trianglecounting.cpp ../graph.cpp ../graph.hpp ../timer.hpp
	g++ -std=gnu++11 -O3 -fopenmp -o trianglecounting ../graph.cpp trianglecounting.cpp

