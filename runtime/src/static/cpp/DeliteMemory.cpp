#include "DeliteMemory.h"

std::list<char *> **DeliteHeapBlockList;  // list of allocated heap blocks for each thread
char **DeliteHeapCurrentBlock;            // current heap block pointer for each thread
size_t *DeliteHeapOffset;                 // offset of the current heap block for each thread
size_t DeliteHeapBlockSize;               // size of a heap block

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
    DHEAP_DEBUG("[WARNING] pthread_setaffinity_np failed for thread %d\n", idx);
  }

  if (heapSize == 0) {
    int64_t pages = sysconf(_SC_PHYS_PAGES);
    int64_t page_size = sysconf(_SC_PAGE_SIZE);
    heapSize = pages * page_size / 8; // TODO: what is the proper size for each heap block?
  }

  if (idx == initializer) {
    DeliteHeapBlockList = new std::list<char*>*[numThreads << paddingShift];
    DeliteHeapCurrentBlock = new char*[numThreads << paddingShift];
    DeliteHeapOffset = new size_t[numThreads << paddingShift];
  }
  DeliteHeapBlockSize = heapSize / numLiveThreads;
  char *ptr = new char[DeliteHeapBlockSize];
  memset(ptr, 0, DeliteHeapBlockSize);

  delite_barrier(numLiveThreads);

  // Now all the threads allocated and initialized their own heap
  DeliteHeapBlockList[idx << paddingShift] = new std::list<char*>(1,ptr);
  DeliteHeapCurrentBlock[idx << paddingShift] = ptr;
  DeliteHeapOffset[idx << paddingShift] = 0;
  DHEAP_DEBUG("finished heap initialization for resource %d, size: %lld\n", idx, DeliteHeapBlockSize);
}

void DeliteHeapClear(int idx, int numThreads, int numLiveThreads, int finalizer) {
  std::list<char*> *blocklist = DeliteHeapBlockList[idx << paddingShift];
  size_t heapUsage = DeliteHeapOffset[idx << paddingShift] + (blocklist->size() - 1) * DeliteHeapBlockSize;
  delite_barrier(numLiveThreads);
  for (std::list<char*>::iterator iter = blocklist->begin(); iter != blocklist->end(); iter++) {
    delete[] *iter;
  }
  if (idx == finalizer) {
    delete[] DeliteHeapBlockList;
    delete[] DeliteHeapCurrentBlock;
    delete[] DeliteHeapOffset;
  }
  DHEAP_DEBUG("finished heap clear for resource %d, used: %lld\n", idx, heapUsage);
}

char *DeliteHeapAlloc(size_t sz, int idx) {
  //TODO: Need alignment for each type (passed as paramter)?
  size_t alignedSize = (sz+0x0008) & 0xFFFFFFF8;
  size_t currentOffset = DeliteHeapOffset[idx << paddingShift];
  size_t newOffset = currentOffset + alignedSize;
  // allocate a new heap block if more space is needed
  if (newOffset > DeliteHeapBlockSize) {
    currentOffset = 0;
    newOffset = alignedSize;
    char *newBlock = new char[DeliteHeapBlockSize];
    DeliteHeapBlockList[idx << paddingShift]->push_back(newBlock);
    DeliteHeapCurrentBlock[idx << paddingShift] = newBlock;
    DHEAP_DEBUG("Used all available heap for thread %d. Allocating more memory.\n", idx);
  }
  char *ptr = DeliteHeapCurrentBlock[idx << paddingShift] + currentOffset; 
  DHEAP_DEBUG("DeliteHeapAlloc called for idx %d with size %d\n", idx, sz);
  DeliteHeapOffset[idx << paddingShift] = newOffset;
  return ptr;
}

void *operator new(size_t sz, const resourceInfo_t &resourceInfo) {
  return DeliteHeapAlloc(sz, resourceInfo.thread_id);
}

void *operator new[](size_t sz, const resourceInfo_t &resourceInfo) {
  return DeliteHeapAlloc(sz, resourceInfo.thread_id);
}
