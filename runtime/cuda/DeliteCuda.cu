#ifndef _DELITE_CUDA_
#define _DELITE_CUDA_

#include "DeliteCuda.h"

using namespace std;

list<void*>* lastAlloc = new list<void*>();
queue<FreeItem>* freeList = new queue<FreeItem>();
map<void*,list<void*>*>* cudaMemoryMap = new map<void*,list<void*>*>();

void addEvent(cudaStream_t fromStream, cudaStream_t toStream) {
  cudaEvent_t event;
  cudaEventCreateWithFlags(&event, cudaEventDisableTiming);
  cudaEventRecord(event, fromStream);
  cudaStreamWaitEvent(toStream, event, 0);
  cudaEventDestroy(event);
}

cudaEvent_t addHostEvent(cudaStream_t stream) {
  cudaEvent_t event;
  cudaEventCreateWithFlags(&event, cudaEventDisableTiming | cudaEventBlockingSync);
  cudaEventRecord(event, stream);
  return event;
}

void freeCudaMemory(FreeItem item) {
    list< pair<void*,bool> >::iterator iter;
    for (iter = item.keys->begin(); iter != item.keys->end(); iter++) {
        //cout << "object ref: " << (long) *iter << endl;
        if(cudaMemoryMap->find((*iter).first) != cudaMemoryMap->end()) {
        	list<void*>* freePtrList = cudaMemoryMap->find((*iter).first)->second;
       		list<void*>::iterator iter2;
        	for (iter2 = freePtrList->begin(); iter2 != freePtrList->end(); iter2++) {
            	void* freePtr = *iter2;
            	cudaFree(freePtr);
            	//if (cudaFree(freePtr) != cudaSuccess)
            	//    cout << "bad free pointer: " << (long) freePtr << endl;
            	//else
                	//cout << "freed successfully: " << (long) freePtr << endl;
       		}
        	cudaMemoryMap->erase((*iter).first);
        	delete freePtrList;
        	if(!((*iter).second)) free((*iter).first);
		}
    }
    delete item.keys;
}

void DeliteCudaMalloc(void** ptr, size_t size) {

  while (freeList->size() != 0) {
    FreeItem item = freeList->front();
    if (cudaEventQuery(item.event) != cudaSuccess) {
      break;
    }
    freeList->pop();
    cudaEventDestroy(item.event);
    freeCudaMemory(item);
  }

  while (cudaMalloc(ptr, size) != cudaSuccess) {
    if (freeList->size() == 0) {
      cout << "FATAL: Insufficient device memory" << endl;
      exit(-1);
    }
    FreeItem item = freeList->front();
    freeList->pop();

    while (cudaEventQuery(item.event) != cudaSuccess) {
      cudaEventSynchronize(item.event);
    }
    cudaEventDestroy(item.event);
    freeCudaMemory(item);
  }
  lastAlloc->push_back(*ptr);
}

size_t cudaHeapSize = 1024*1204;
char* bufferStart = 0;
size_t bufferSize = 5368709120/4;
char* bufferEnd;
char* bufferCurrent;


/* Implementations for temporary memory management */
char *tempCudaMemPtr;
size_t tempCudaMemOffset;
size_t tempCudaMemSize;

void tempCudaMemInit(double tempMemRate) {
  size_t free, total;
  cudaMemGetInfo(&free, &total);
  tempCudaMemSize = total * tempMemRate;
  tempCudaMemOffset = 0;
  if(cudaMalloc(&tempCudaMemPtr, tempCudaMemSize) != cudaSuccess) {
    cout << "FATAL (tempCudaMemInit): Insufficient device memory for tempCudaMem" << endl;
    exit(-1);
  }
  //cout << "Free:" << free << endl;
  //cout << "Total:" << total << endl;
  //cout << "tempMemSize:" << tempCudaMemSize << endl;
}

void tempCudaMemReset(void) {
  tempCudaMemOffset = 0;
}

size_t tempCudaMemAvailable(void) {
  return (tempCudaMemSize - tempCudaMemOffset);
}

void DeliteCudaMallocTemp(void** ptr, size_t size) {
  if(tempCudaMemOffset + size > tempCudaMemSize) {
    cout << "FATAL(DeliteCudaMallocTemp): Insufficient device memory for tempCudaMem" << endl;
    exit(-1);
  }
  else {
    *ptr = tempCudaMemPtr + tempCudaMemOffset;
    tempCudaMemOffset += size;
  }
}

void hostInit() {
	cudaHostAlloc(&bufferStart, bufferSize, cudaHostAllocDefault);
	bufferEnd = bufferStart + bufferSize;
	bufferCurrent = bufferStart;
}

void DeliteCudaMallocHost(void** ptr, size_t size) {
	if (bufferStart == 0) hostInit();
	if ((bufferCurrent + size) > bufferEnd)
		bufferCurrent = bufferStart;
	*ptr = bufferCurrent;
	bufferCurrent += size;
}

void DeliteCudaMemcpyHtoDAsync(void* dptr, void* sptr, size_t size) {
	cudaMemcpyAsync(dptr, sptr, size, cudaMemcpyHostToDevice, h2dStream);
}

void DeliteCudaMemcpyDtoHAsync(void* dptr, void* sptr, size_t size) {
	cudaMemcpyAsync(dptr, sptr, size, cudaMemcpyDeviceToHost, d2hStream);
	cudaStreamSynchronize(d2hStream);
}

void DeliteCudaMemcpyDtoDAsync(void *dptr, void* sptr, size_t size) {
	cudaMemcpyAsync(dptr, sptr, size, cudaMemcpyDeviceToDevice, kernelStream);
}

void DeliteCudaMemset(void *ptr, int value, size_t count) {
	cudaMemset(ptr,value,count);
}

#endif
