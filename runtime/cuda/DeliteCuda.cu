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

void freeCudaMemory(FreeItem item) {
    list<void*>::iterator iter;
    for (iter = item.keys->begin(); iter != item.keys->end(); iter++) {
        //cout << "object ref: " << (long) *iter << endl;
        if(cudaMemoryMap->find(*iter) != cudaMemoryMap->end()) {
        	list<void*>* freePtrList = cudaMemoryMap->find(*iter)->second;
       		list<void*>::iterator iter2;
        	for (iter2 = freePtrList->begin(); iter2 != freePtrList->end(); iter2++) {
            	void* freePtr = *iter2;
            	cudaFree(freePtr);
            	//if (cudaFree(freePtr) != cudaSuccess)
            	//    cout << "bad free pointer: " << (long) freePtr << endl;
            	//else
                	//cout << "freed successfully: " << (long) freePtr << endl;
       		}
        	cudaMemoryMap->erase(*iter);
        	delete freePtrList;
        	free(*iter);
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

/*
void DeliteCudaMalloc(void** ptr, size_t size) {
    while (freeList->size() > 0) {
	    FreeItem item = freeList->front();
 	        freeList->pop();
		
		while (cudaEventQuery(item.event) != cudaSuccess) {
			cudaEventSynchronize(item.event);
		}
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
				else
					cout << "freed successfully: " << (long) freePtr << endl;
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
	else
		cout << "allocated successfully: " << (long) *ptr << endl;
	
	lastAlloc->push_back(*ptr);
} */

char* bufferStart = 0;
size_t bufferSize = 5368709120/4;
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

void DeliteCudaMemcpyDtoHAsync(void* dptr, void* sptr, size_t size) {
	cudaMemcpyAsync(dptr, sptr, size, cudaMemcpyDeviceToHost, d2hStream);
	cudaStreamSynchronize(d2hStream);
}

void DeliteCudaMemcpyDtoDAsync(void *dptr, void* sptr, size_t size) {
	cudaMemcpyAsync(dptr, sptr, size, cudaMemcpyDeviceToDevice, h2dStream);
}

void DeliteCudaMemset(void *ptr, int value, size_t count) {
	cudaMemset(ptr,value,count);
}
