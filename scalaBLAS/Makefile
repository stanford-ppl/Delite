all:
	scalac scalaBLAS.scala
	javah scalaBLAS$$
	icc -O3 -I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/linux -I/usr/local/intel/mkl/include -L/usr/local/intel/mkl/lib/em64t -L/usr/local/intel/lib/intel64 -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lmkl_mc3 -lmkl_def -lgfortran -fPIC -o scalaBLAS.so -shared scalaBLAS.c
