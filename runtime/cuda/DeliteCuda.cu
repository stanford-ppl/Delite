#include <cuda_runtime.h>
#include <list>
#include <map>
#include <queue>
#include <iostream>

using namespace std;

extern cudaStream_t h2dStream;
extern cudaStream_t d2hStream;

void* lastValue;

struct FreeItem {
	cudaEvent_t event;
	list<void*>* keys;
};

queue<FreeItem>* freeList = new queue<FreeItem>();

map<void*,void*>* cudaMemoryMap = new map<void*,void*>();


void DeliteCudaMalloc(void** ptr, size_t size) {
	size_t free;
	size_t total;
	cudaMemGetInfo(&free, &total);
	while (free < size) {
		if (freeList->size() == 0) {
			cout << "Insufficient device memory" << endl;
			exit(-1);
		}
		FreeItem item = freeList->front();
        	freeList->pop();
		while (cudaEventQuery(item.event) != cudaSuccess)
			cudaEventSynchronize(item.event);
		list<void*>::iterator iter;
		for (iter = item.keys->begin(); iter != item.keys->end(); iter++) {
			cudaFree(cudaMemoryMap->find(*iter)->second);
		}
		cudaMemGetInfo(&free, &total);
	}
	cudaMalloc(ptr, size);
	lastValue = *ptr;
}

/* void DeliteCudaMalloc(void** ptr, int size) {
        while (freeList->size() > 0) {
		FreeItem item = freeList->front();
 	        freeList->pop();
		while (cudaEventQuery(item.event) != cudaSuccess)
			cudaEventSynchronize(item.event);
		list<void*>::iterator iter;
		for (iter = item.keys->begin(); iter != item.keys->end(); iter++) {
			void* freePtr = cudaMemoryMap->find(*iter)->second;
			if (cudaSuccess != cudaFree(freePtr))
				cout << "bad free pointer" << endl;
			else
				cout << "freed successfully: " << (long) freePtr << endl;
		}
	}
	if (cudaSuccess != cudaMalloc(ptr, size))
		cout << "malloc failed" << endl;
	else
		cout << "allocated successfully: " << (long) *ptr << endl;
	lastValue = *ptr;
} */

char* bufferStart = 0;
size_t bufferSize = 10737418240;
char* bufferEnd;
char* bufferCurrent;

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

void DeliteCudaMemcpyDtoHAsync(void* dptr, void* sptr, int size) {
	cudaMemcpyAsync(dptr, sptr, size, cudaMemcpyDeviceToHost, d2hStream);
	cudaStreamSynchronize(d2hStream);
}
