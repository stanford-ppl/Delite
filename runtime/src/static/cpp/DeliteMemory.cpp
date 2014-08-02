#include "DeliteMemory.h"

char **DeliteHeap;
size_t *DeliteHeapOffset;

pthread_mutex_t heapInitLock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t heapInitCond = PTHREAD_COND_INITIALIZER;
int heapInitCnt = 0;

// align thread-local variables with 64 bytes to avoid false sharing
const int paddingShift = 6;

void delite_barrier(int numThreads) {
    pthread_mutex_lock(&heapInitLock);
    heapInitCnt += 1;
    if (heapInitCnt < numThreads) {
      pthread_cond_wait(&heapInitCond, &heapInitLock);
    }
    if (heapInitCnt == numThreads) {
      pthread_cond_broadcast(&heapInitCond);
      heapInitCnt = 0;
    }
    pthread_mutex_unlock(&heapInitLock);
}

void DeliteHeapInit(int idx, int numThreads, size_t heapSize) {
  if (heapSize == 0) {
    int64_t pages = sysconf(_SC_PHYS_PAGES);
    int64_t page_size = sysconf(_SC_PAGE_SIZE);
    heapSize = pages * page_size / 4; // use 25% of physical memory as heap
  }

  if (idx == 0) {
    DeliteHeap = new char*[numThreads << paddingShift];
    DeliteHeapOffset = new size_t[numThreads << paddingShift];
  }
  char *ptr = (char*)malloc(heapSize/numThreads);
  memset(ptr, 0, heapSize/numThreads);

  if (numThreads > 1) {
    delite_barrier(numThreads);
  }

  // Now all the threads allocated and initialized their own heap
  DeliteHeap[idx << paddingShift] = ptr;
  DeliteHeapOffset[idx << paddingShift] = 0;
  DHEAP_DEBUG("finished heap initialization for resource %d, size: %lld\n", idx, heapSize/numThreads);
}

void DeliteHeapClear(int idx, int numThreads) {
  size_t heapUsage = DeliteHeapOffset[idx << paddingShift];
  if (numThreads > 1) {
    delite_barrier(numThreads);
  }
  delete[] DeliteHeap[idx << paddingShift];
  if (idx == 0) {
    delete[] DeliteHeap;
    delete[] DeliteHeapOffset;
  }
  DHEAP_DEBUG("finished heap clear for resource %d, used: %lld\n", idx, heapUsage);
}

char *DeliteHeapAlloc(size_t sz, int idx) {
  char *ptr = DeliteHeap[idx << paddingShift] + DeliteHeapOffset[idx << paddingShift];
  DHEAP_DEBUG("DeliteHeapAlloc called for idx %d with size %d\n", idx, sz);
  //TODO: Need alignment for each type (passed as paramter)?
  size_t alignedSize = (sz+0x0008) & 0xFFFFFFF8;
  DeliteHeapOffset[idx << paddingShift] += alignedSize;
  return ptr;
}

void *operator new(size_t sz, const resourceInfo_t &resourceInfo) {
  return DeliteHeapAlloc(sz, resourceInfo.thread_id);
}

void *operator new[](size_t sz, const resourceInfo_t &resourceInfo) {
  return DeliteHeapAlloc(sz, resourceInfo.thread_id);
}
