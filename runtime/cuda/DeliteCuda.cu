#include <cuda_runtime.h>
#include <stdio.h>
#include <list>
#include <queue>
#include <assert.h>
#include <stdint.h>
#include "CudaIOBuffer.h"


using namespace std;


//extern cudaStream_t h2dStream;
//extern cudaStream_t d2hStream;







void DeliteCudaMalloc(void **ptr, int size) {
	cudaMalloc(ptr, size);
}

void DeliteCudaMemcpyHtoDAsync(void *dptr, void *sptr, int size) {
  //cudaMemcpyAsync(dptr, sptr, size, cudaMemcpyHostToDevice, h2dStream);

}

void DeliteCudaMemcpyDtoHAsync(void *dptr, void *sptr, int size) {
  //cudaMemcpyAsync(dptr, sptr, size, cudaMemcpyDeviceToHost, d2hStream);
  //cudaStreamSynchronize(d2hStream);
}






