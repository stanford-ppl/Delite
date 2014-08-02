#ifndef __DELITE_MEMORY_H__
#define __DELITE_MEMORY_H__

#include <memory>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>

//#define DHEAP_DEBUG(...) fprintf(stderr, "[DEBUG-DeliteHeap] "); fprintf(stderr, __VA_ARGS__)
#define DHEAP_DEBUG(...)

void DeliteHeapInit(int idx, int numThreads, size_t heapSize);
void DeliteHeapClear(int idx, int numThreads);
char *DeliteHeapAlloc(size_t sz, int idx);

class DeliteMemory {
public:
  void* operator new(size_t sz) {
    DHEAP_DEBUG("Non-local allocation with size %d\n", sz);
    return malloc(sz);
  }
  
  void* operator new(size_t sz, int heapIdx) {
    DHEAP_DEBUG("Allocation from Idx %d with size %d\n", heapIdx, sz);
    return DeliteHeapAlloc(sz, heapIdx);
  }

  /*
  void operator delete(void*) {
  }
  */
};

#endif
