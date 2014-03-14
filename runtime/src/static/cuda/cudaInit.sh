nvcc -I$JAVA_HOME/include/ -I$JAVA_HOME/include/linux/ -arch compute_20 -code sm_20 -shared -Xcompiler '-fPIC' -o cudaInit.so cudaInit.cu
