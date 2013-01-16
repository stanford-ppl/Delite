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

using namespace std;

extern size_t cudaHeapSize;

// Second element in pair<void*,bool> indicates that void* points to GPU device memory,
// so should not call free() on it.
struct FreeItem {
    cudaEvent_t event;
    list< pair<void*,bool> >* keys;
};

extern cudaStream_t h2dStream;
extern cudaStream_t d2hStream;
extern cudaStream_t kernelStream;
extern char *tempCudaMem;
extern size_t tempCudaMemSize;

extern list<void*>* lastAlloc;
extern queue<FreeItem>* freeList;
extern map<void*,list<void*>*>* cudaMemoryMap;

extern void addEvent(cudaStream_t fromStream, cudaStream_t toStream);
extern cudaEvent_t addHostEvent(cudaStream_t stream);
extern void freeCudaMemory(FreeItem item);
extern void DeliteCudaMalloc(void** ptr, size_t size);
extern void tempCudaMemInit(void);
extern void hostInit();
extern void DeliteCudaMallocHost(void** ptr, size_t size);
extern void DeliteCudaMemcpyHtoDAsync(void* dptr, void* sptr, size_t size);
extern void DeliteCudaMemcpyDtoHAsync(void* dptr, void* sptr, size_t size);
extern void DeliteCudaMemcpyDtoDAsync(void *dptr, void* sptr, size_t size);
extern void DeliteCudaMemset(void *ptr, int value, size_t count);

#endif
