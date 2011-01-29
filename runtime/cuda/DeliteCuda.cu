#include <cuda_runtime.h>
#include <list>
#include <map>
#include <queue>
#include <iostream>

using namespace std;

extern cudaStream_t h2dStream;
extern cudaStream_t d2hStream;

list<void*>* lastAlloc = new list<void*>();

struct FreeItem {
    cudaEvent_t event;
    list<void*>* keys;
};

queue<FreeItem>* freeList = new queue<FreeItem>();

map<void*,list<void*>*>* cudaMemoryMap = new map<void*,list<void*>*>();

/*
int mallocInit = 0;
void *retPtr;
void DeliteCudaMalloc(void** ptr, size_t size) {
	if(mallocInit == 0) {
		cudaMalloc(&retPtr,2000*2000*8);
		mallocInit = 1;
	}
	*ptr = retPtr;
}
*/

/*
char* devBufferStart = 0;
size_t devBufferSize = 1024*1024*1024;
char* devBufferEnd;
char* devBufferCurrent;

void devInit() {
	cudaMalloc(&devBufferStart, devBufferSize);
	devBufferEnd = devBufferStart + devBufferSize;
	devBufferCurrent = devBufferStart;
}
void DeliteCudaMalloc(void** ptr, size_t size) {
	if (devBufferStart == 0) devInit();
	if ((devBufferCurrent + size) > devBufferEnd)
		devBufferCurrent = devBufferStart;
	*ptr = devBufferCurrent;
	devBufferCurrent += size;
}
*/

void DeliteCudaMalloc(void** ptr, size_t size) {
	size_t freeAmt;
	size_t totalAmt;
	cudaMemGetInfo(&freeAmt, &totalAmt);
	while (freeAmt < size) {
		if (freeList->size() == 0) {
			cout << "FATAL: Insufficient device memory" << endl;
			exit(-1);
		}
		FreeItem item = freeList->front();
        	freeList->pop();

		while (cudaEventQuery(item.event) != cudaSuccess)
			cudaEventSynchronize(item.event);
		cudaEventDestroy(item.event);
		
		list<void*>::iterator iter;
		for (iter = item.keys->begin(); iter != item.keys->end(); iter++) {
			list<void*>* freePtrList = cudaMemoryMap->find(*iter)->second;
			list<void*>::iterator iter2;
			for (iter2 = freePtrList->begin(); iter2 != freePtrList->end(); iter2++) {
				cudaFree(*iter2);	
			}
			cudaMemoryMap->erase(*iter);
			delete freePtrList;
			free(*iter);
		}
		delete item.keys;
		cudaMemGetInfo(&freeAmt, &totalAmt);
	}
	
	cudaMalloc(ptr, size);
	lastAlloc->push_back(*ptr);
}

/* //this version frees memory eagerly; useful for debugging
void DeliteCudaMalloc(void** ptr, size_t size) {
    while (freeList->size() > 0) {
	    FreeItem item = freeList->front();
 	        freeList->pop();
		
		while (cudaEventQuery(item.event) != cudaSuccess)
			cudaEventSynchronize(item.event);
		cudaEventDestroy(item.event);
		
		list<void*>::iterator iter;
		for (iter = item.keys->begin(); iter != item.keys->end(); iter++) {
			//cout << "object ref: " << (long) *iter << endl;
			list<void*>* freePtrList = cudaMemoryMap->find(*iter)->second;	
			list<void*>::iterator iter2;
			for (iter2 = freePtrList->begin(); iter2 != freePtrList->end(); iter2++) {
				void* freePtr = *iter2;
				if (cudaFree(freePtr) != cudaSuccess)
					cout << "bad free pointer: " << (long) freePtr << endl;
				//else
					//cout << "freed successfully: " << (long) freePtr << endl;
			}
			cudaMemoryMap->erase(*iter);
			delete freePtrList;
			free(*iter);
		}
		delete item.keys;
	}

	if (cudaMalloc(ptr, size) != cudaSuccess) {
		cout << "FATAL: cuda malloc failed unexpectedly" << endl;
		exit(-1);
	}
	//else
		//cout << "allocated successfully: " << (long) *ptr << endl;
	
	lastAlloc->push_back(*ptr);
}
*/

char* bufferStart = 0;
size_t bufferSize = 10737418240;
char* bufferEnd;
char* bufferCurrent;

void hostInit() {
	cudaHostAlloc(&bufferStart, bufferSize, cudaHostAllocDefault);
	bufferEnd = bufferStart + bufferSize;
	bufferCurrent = bufferStart;
}

/*
void DeliteCudaMallocHost(void** ptr, size_t size) {
	if (bufferStart == 0) hostInit();
	*ptr = bufferCurrent;
}
*/
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

void DeliteCudaMemset(void *ptr, int value, size_t count) {
	cudaMemset(ptr,value,count);
}
