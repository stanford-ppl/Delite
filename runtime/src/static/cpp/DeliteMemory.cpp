#include "DeliteMemory.h"

char **DeliteHeap;
size_t *DeliteHeapOffset;

pthread_mutex_t heapInitLock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t heapInitCond = PTHREAD_COND_INITIALIZER;
int heapInitCnt = 0;

// align thread-local variables with 64 bytes to avoid false sharing
const int paddingShift = 6;

void delite_barrier(int cnt) {
  if (cnt > 1) {
    pthread_mutex_lock(&heapInitLock);
    heapInitCnt += 1;
    if (heapInitCnt < cnt) {
      pthread_cond_wait(&heapInitCond, &heapInitLock);
    }
    if (heapInitCnt == cnt) {
      pthread_cond_broadcast(&heapInitCond);
      heapInitCnt = 0;
    }
    pthread_mutex_unlock(&heapInitLock);
  }
}

// idx: the resource (thread) index for initialization
// numThreads: the total number of threads (max number of possible thread ids)
// numLiveThreads: the actual number of threads that has some work scheduled (they're calling this init)
// heapSize: total heap size (aggregation for all the threads)
void DeliteHeapInit(int idx, int numThreads, int numLiveThreads, int initializer, size_t heapSize) {
  cpu_set_t cpuset;
  pthread_t thread = pthread_self();

  CPU_ZERO(&cpuset);
  CPU_SET(idx, &cpuset);
  if(pthread_setaffinity_np(thread, sizeof(cpu_set_t), &cpuset) != 0) {
    printf("[WARNING] pthread_setaffinity_np failed for thread %d\n", idx);
  }

  if (heapSize == 0) {
    int64_t pages = sysconf(_SC_PHYS_PAGES);
    int64_t page_size = sysconf(_SC_PAGE_SIZE);
    heapSize = pages * page_size / 4; // use 25% of physical memory as heap
  }

  if (idx == initializer) {
    DeliteHeap = new char*[numThreads << paddingShift];
    DeliteHeapOffset = new size_t[numThreads << paddingShift];
  }
  size_t localHeapSize = heapSize / numLiveThreads;
  char *ptr = (char*)malloc(localHeapSize);
  memset(ptr, 0, localHeapSize);

  delite_barrier(numLiveThreads);

  // Now all the threads allocated and initialized their own heap
  DeliteHeap[idx << paddingShift] = ptr;
  DeliteHeapOffset[idx << paddingShift] = 0;
  DHEAP_DEBUG("finished heap initialization for resource %d, size: %lld\n", idx, localHeapSize);
}

void DeliteHeapClear(int idx, int numThreads, int numLiveThreads, int finalizer) {
  size_t heapUsage = DeliteHeapOffset[idx << paddingShift];
  delite_barrier(numLiveThreads);
  delete[] DeliteHeap[idx << paddingShift];
  if (idx == finalizer) {
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
