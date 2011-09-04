g++ -I/usr/local/cuda/include/ -shared -fPIC openclBLAS.cpp -o openclBLAS.so
g++ -I$JAVA_HOME/include -I$JAVA_HOME/include/linux -shared -fPIC openclInit.cpp -o openclInit.so
