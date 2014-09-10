#ifndef _DELITE_CUDA_
#define _DELITE_CUDA_

#include "DeliteCuda.h"

std::list<void*>* lastAlloc = new std::list<void*>();
std::queue<FreeItem>* freeList = new std::queue<FreeItem>();
std::map<void*,std::list<void*>*>* cudaMemoryMap = new std::map<void*,std::list<void*>*>();

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
  std::list< std::pair<void*,bool> >::iterator iter;
  for (iter = item.keys->begin(); iter != item.keys->end(); iter++) {
    //std::cout << "object ref: " << (long) *iter << std::endl;
    if(cudaMemoryMap->find((*iter).first) != cudaMemoryMap->end()) {
      std::list<void*>* freePtrList = cudaMemoryMap->find((*iter).first)->second;
      std::list<void*>::iterator iter2;
      for (iter2 = freePtrList->begin(); iter2 != freePtrList->end(); iter2++) {
        void* freePtr = *iter2;
        cudaFree(freePtr);
        //if (cudaFree(freePtr) != cudaSuccess)
        //    std::cout << "bad free pointer: " << (long) freePtr << std::endl;
        //else
        //std::cout << "freed successfully: " << (long) freePtr << std::endl;
      }
      cudaMemoryMap->erase((*iter).first);
      delete freePtrList;
      if(!((*iter).second)) free((*iter).first);
    }
  }
  delete item.keys;
}

// collects cuda memory allocated for kernels completed at the moment
void DeliteCudaGC(void) {
  while (freeList->size() != 0) {
    FreeItem item = freeList->front();
    if (cudaEventQuery(item.event) != cudaSuccess) {
      break;
    }
    freeList->pop();
    cudaEventDestroy(item.event);
    freeCudaMemory(item);
  }
}

// collects all garbages and checks no remaining allocations left
void DeliteCudaCheckGC(void) {
  DeliteCudaGC();
  if(freeList->size() != 0) 
    std::cout << "WARNING: memory not collectd : count " << freeList->size() << std::endl;
}

// allocates a chunk of cuda device memory
// run GC before allocation
void DeliteCudaMalloc(void** ptr, size_t size) {

  DeliteCudaGC();

  while (cudaMalloc(ptr, size) != cudaSuccess) {
    if (freeList->size() == 0) {
      std::cout << "FATAL: Insufficient device memory" << std::endl;
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

/* Implementations for temporary memory management */
#define CUDAMEM_ALIGNMENT 64
char *tempCudaMemPtr;
size_t tempCudaMemOffset;
size_t tempCudaMemSize;

// initialize cuda temporary device memory
void tempCudaMemInit(double tempMemRate) {
  DeliteCudaProfInit();
  size_t free, total;
  cudaMemGetInfo(&free, &total);
  tempCudaMemSize = total * tempMemRate;
  //std::cout << "initializing cuda temp mem.." << std::endl;
  //std::cout << "Free:" << free << std::endl;
  //std::cout << "Total:" << total << std::endl;
  //std::cout << "tempMemSize:" << tempCudaMemSize << std::endl;
  tempCudaMemOffset = 0;
  if(cudaMalloc(&tempCudaMemPtr, tempCudaMemSize) != cudaSuccess) {
    std::cout << "FATAL (tempCudaMemInit): Insufficient device memory for tempCudaMem" << std::endl;
    exit(-1);
  }
  //std::cout << "finished temp init" << std::endl;
}

// free cuda temporary memory
void tempCudaMemFree(void) {
  if(cudaFree(tempCudaMemPtr) != cudaSuccess) {
    std::cout << "FATAL (tempCudaMemFree): Failed to free temporary memory" << std::endl;
    exit(-1);
  }
}

// reset cuda temporary memory (called by each multiloop)
void tempCudaMemReset(void) {
  tempCudaMemOffset = 0;
}

// return the size of available temporary memory
size_t tempCudaMemAvailable(void) {
  return (tempCudaMemSize - tempCudaMemOffset - CUDAMEM_ALIGNMENT);
}

// allocates cuda device memory from temporary space
void DeliteCudaMallocTemp(void** ptr, size_t size) {
  size_t alignedSize = CUDAMEM_ALIGNMENT * (1 + size / CUDAMEM_ALIGNMENT);
  if(tempCudaMemOffset + alignedSize > tempCudaMemSize) {
    std::cout << "FATAL(DeliteCudaMallocTemp): Insufficient device memory for tempCudaMem" << std::endl;
    exit(-1);
  }
  else {
    *ptr = tempCudaMemPtr + tempCudaMemOffset;
    tempCudaMemOffset += alignedSize;
  }
}

// variables for cuda host memory
char* bufferStart = 0;
char* bufferEnd;
char* bufferCurrent;

// initialize cuda host memory (page-std::mapped system memory for asynchronous copy)
void hostInit() {
  size_t free, total;
  cudaMemGetInfo(&free, &total);
  // allocate the host memory as much as the device memory (make it parameter?)
  cudaHostAlloc(&bufferStart, total, cudaHostAllocDefault);
  bufferEnd = bufferStart + total;
  bufferCurrent = bufferStart;
}

// free cuda host memory
void cudaHostMemFree(void) {
  cudaFreeHost(bufferStart);
  bufferStart = NULL;
}

// allocate cuda host memory
void DeliteCudaMallocHost(void** ptr, size_t size) {
  size_t alignedSize = CUDAMEM_ALIGNMENT * (1 + size / CUDAMEM_ALIGNMENT);
  if (bufferStart == 0) hostInit();
  if ((bufferCurrent + alignedSize) > bufferEnd)
    bufferCurrent = bufferStart;
  *ptr = bufferCurrent;
  bufferCurrent += alignedSize;
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

void DeliteCudaCheckError(void) {
  cudaDeviceSynchronize();
  if (cudaPeekAtLastError() != cudaSuccess) {
    std::cout << "DeliteCuda execution failed: " << cudaGetErrorString(cudaPeekAtLastError()) << std::endl;
    exit(-1);
  }
}

struct timeval start_t;
struct timeval end_t;
void DeliteCudaTic(void) {
  cudaDeviceSynchronize();
  gettimeofday(&start_t,NULL);
}

void DeliteCudaToc(void) {
  cudaDeviceSynchronize();
  gettimeofday(&end_t,NULL);
  double exetime = (end_t.tv_sec*1000000+end_t.tv_usec) - (start_t.tv_sec*1000000+start_t.tv_usec);
  std::cout << "DeliteCudaTimer(static) " << " " << (exetime)/1000.0 << " ms" << std::endl;
}

//std::map<string,double> *cudaTimerMap = new std::map<string,double>();

char **ticName;
double *ticStart;
int ticIdx;

void DeliteCudaProfInit(void) {
  ticName = new char*[1024];
  ticStart = new double[1024];
  ticIdx = 0;
}

void DeliteCudaTic(char *name) {
  struct timeval t;

  cudaDeviceSynchronize();
  gettimeofday(&t,NULL);
  ticStart[ticIdx] = t.tv_sec*1000000+t.tv_usec; 
  ticName[ticIdx] = name;
  ticIdx += 1;
  //cudaTimerMap->insert(pair<string,double>(name, t.tv_sec*1000000+t.tv_usec));
}

void DeliteCudaToc(char *name) {
  struct timeval t;
  //std::map<string,double>::iterator it = cudaTimerMap->find(name);
  //double start = (it==cudaTimerMap->end()) ? 0 : it->second;
  //if(start != 0) cudaTimerMap->erase(it);
  cudaDeviceSynchronize();
  gettimeofday(&t,NULL);
  double end = t.tv_sec*1000000+t.tv_usec;
  ticIdx -= 1;
  std::cout << "DeliteCudaTimer " << ticName[ticIdx] << " " << (end-ticStart[ticIdx])/1000.0 << " ms" << std::endl;
}

// TODO: Remove this kernel from here by generate it 
__global__ void kernel_offset(int *key, int *idx, int *offset, int size) {

  int idxX = threadIdx.x + blockIdx.x*blockDim.x;

  if(idxX == 0) {
    offset[1] = 0;
  }
  else if(idxX < size) {
    int keyVal = key[idxX];
    int keyValPrev = key[idxX-1]; 
    if(keyVal != keyValPrev) {
      offset[keyVal+1] = idxX;
    }
  }
  if(idxX == size-1) {
    int keyVal = key[idxX];
    offset[0] = keyVal+1;
    offset[keyVal+2] = size;
  }
}

#endif
