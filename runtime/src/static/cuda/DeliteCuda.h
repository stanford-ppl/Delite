#ifndef _DELITE_CUDA_H_
#define _DELITE_CUDA_H_

#include <list>
#include <map>
#include <queue>
#include <iostream>
#include <utility>
#include <cuda_runtime.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>

extern size_t cudaHeapSize;

// Second element in pair<void*,bool> indicates that void* points to GPU device memory,
// so should not call free() on it.
struct FreeItem {
    cudaEvent_t event;
    std::list< std::pair<void*,bool> >* keys;
};

extern cudaStream_t h2dStream;
extern cudaStream_t d2hStream;
extern cudaStream_t kernelStream;

extern std::list<void*>* lastAlloc;
extern std::queue<FreeItem>* freeList;
extern std::map<void*,std::list<void*>*>* cudaMemoryMap;

extern void addEvent(cudaStream_t fromStream, cudaStream_t toStream);
extern cudaEvent_t addHostEvent(cudaStream_t stream);
extern void freeCudaMemory(FreeItem item);
extern void DeliteCudaCheckGC(void);
extern void DeliteCudaGC(void);
extern void DeliteCudaMalloc(void** ptr, size_t size);
extern void tempCudaMemInit(double tempMemRate);
extern void tempCudaMemFree(void);
extern void tempCudaMemReset(void);
extern size_t tempCudaMemAvailable(void);
extern void DeliteCudaMallocTemp(void** ptr, size_t size);
extern void hostInit();
extern void DeliteCudaMallocHost(void** ptr, size_t size);
extern void cudaHostMemFree(void);
extern void DeliteCudaMemcpyHtoDAsync(void* dptr, void* sptr, size_t size);
extern void DeliteCudaMemcpyDtoHAsync(void* dptr, void* sptr, size_t size);
extern void DeliteCudaMemcpyDtoDAsync(void *dptr, void* sptr, size_t size);
extern void DeliteCudaMemset(void *ptr, int value, size_t count);
extern void DeliteCudaCheckError(void);

extern void DeliteCudaProfInit(void);
extern void DeliteCudaTic(char *name);
extern void DeliteCudaToc(char *name);
extern void DeliteCudaTic(void);
extern void DeliteCudaToc(void);

//TODO: Remove this from here
__global__ void kernel_offset(int *key, int *idx, int *offset, int size);
#endif
