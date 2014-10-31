#include "DeliteMemory.h"

#define DEFAULT_MAX_HEAP 32ULL*1024*1024*1024
#define DEFAULT_BLOCK_SIZE 1024*1024 //TODO: how to select this?

std::list<char *> **DeliteHeapBlockList;  // list of allocated heap blocks for each thread
char **DeliteHeapCurrentBlock;            // current heap block pointer for each thread
size_t *DeliteHeapCurrentBlockSize;       // remaining size of the current heap block for each thread

pthread_mutex_t barrier_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t barrier_cond = PTHREAD_COND_INITIALIZER;
unsigned int arrivedCount = 0;

// align thread-local variables with 64 bytes to avoid false sharing
#define PADDING_SHIFT 6

void delite_barrier(unsigned int count) {
  if (count > 1) {
    pthread_mutex_lock(&barrier_lock);
    arrivedCount += 1;
    if (arrivedCount >= count) {
      arrivedCount = 0;
      pthread_cond_broadcast(&barrier_cond);
    } else {
      pthread_cond_wait(&barrier_cond, &barrier_lock);
    }
    pthread_mutex_unlock(&barrier_lock);
  }
}

// idx: the resource (thread) index for initialization
// numThreads: the total number of threads (max number of possible thread ids)
// numLiveThreads: the actual number of threads that has some work scheduled (they're calling this init)
// heapSize: total heap size (aggregation for all the threads)
void DeliteHeapInit(int idx, int numThreads, int numLiveThreads, int initializer, size_t heapSize) {
  if (heapSize == 0) {
    int64_t memSize = 2ULL*1024*1024*1024; //FIXME
    #ifdef _SC_PHYS_PAGES
      int64_t pages = sysconf(_SC_PHYS_PAGES);
      int64_t page_size = sysconf(_SC_PAGE_SIZE);
      memSize = pages * page_size;
    #endif
    heapSize = memSize / 2; // use 50% of physical memory as heap by default
    if (heapSize > DEFAULT_MAX_HEAP) heapSize = DEFAULT_MAX_HEAP;
  }

  size_t padIdx = idx << PADDING_SHIFT;
  size_t padSize = numThreads << PADDING_SHIFT;
  if (idx == initializer) {
    DeliteHeapBlockList = new std::list<char*>*[padSize];
    DeliteHeapCurrentBlock = new char*[padSize];
    DeliteHeapCurrentBlockSize = new size_t[padSize];
  }

  delite_barrier(numLiveThreads);

  DeliteHeapBlockList[padIdx] = new std::list<char*>();
  DeliteHeapCurrentBlock[padIdx] = NULL;
  DeliteHeapCurrentBlockSize[padIdx] = 0;
  DHEAP_DEBUG("finished heap initialization for resource %d\n", idx);
}

void DeliteHeapAllocBlock(size_t minSize, int idx) {
  size_t padIdx = idx << paddingShift;
  size_t blockSize = std::max(minSize, DEFAULT_BLOCK_SIZE);
  char *newBlock = new char[blockSize];
  memset(newBlock, 0, blockSize);
  DeliteHeapBlockList[padIdx]->push_back(newBlock);
  DeliteHeapCurrentBlock[padIdx] = newBlock;
  DeliteHeapCurrentBlockSize[padIdx] = blockSize;
  DHEAP_DEBUG("Insufficient heap space for thread %d. Allocating %lld bytes.\n", idx, blockSize);
}

char *DeliteHeapAlloc(size_t sz, int idx) {
  //TODO: Need alignment for each type (passed as paramter)?
  DHEAP_DEBUG("DeliteHeapAlloc called for idx %d with size %d\n", idx, sz);
  size_t padIdx = idx << PADDING_SHIFT;
  size_t alignedSize = (sz+0x0008) & 0xFFFFFFF8;
  
  size_t blockSize = DeliteHeapCurrentBlockSize[padIdx];
  // allocate a new heap block if more space is needed
  if (alignedSize > blockSize) {
    DeliteHeapAllocBlock(alignedSize, idx);
  }

  char* ptr = DeliteHeapCurrentBlock[padIdx];
  DeliteHeapCurrentBlock[padIdx] += alignedSize;
  DeliteHeapCurrentBlockSize[padIdx] -= alignedSize;
  return ptr;
}

void *operator new(size_t sz, const resourceInfo_t &resourceInfo) {
  return DeliteHeapAlloc(sz, resourceInfo.threadId);
}

void *operator new[](size_t sz, const resourceInfo_t &resourceInfo) {
  return DeliteHeapAlloc(sz, resourceInfo.threadId);
}

void DeliteHeapClear(int idx, int numThreads, int numLiveThreads, int finalizer) {
  delite_barrier(numLiveThreads); //first wait for all threads to finish working
  
  size_t padIdx = idx << paddingShift;
  std::list<char*> *blocklist = DeliteHeapBlockList[padIdx];
  size_t heapUsage = blocklist->size();
  for (std::list<char*>::iterator iter = blocklist->begin(); iter != blocklist->end(); iter++) {
    delete[] *iter;
  }
  if (idx == finalizer) {
    delete[] DeliteHeapBlockList;
    delete[] DeliteHeapCurrentBlock;
    delete[] DeliteHeapCurrentBlockSize;
  }

  DHEAP_DEBUG("finished heap clear for resource %d, used %d blocks (%dMB blocks).\n", idx, heapUsage, DEFAULT_BLOCK_SIZE/1024/1024);
}
