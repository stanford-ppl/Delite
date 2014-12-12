#include "DeliteMemory.h"

#define DEFAULT_BLOCK_SIZE 1UL*1024*1024

size_t defaultBlockSize;
std::list<char *> **DeliteHeapBlockList = NULL;  // list of allocated heap blocks for each thread
char **DeliteHeapCurrentBlock;            // current heap block pointer for each thread
size_t *DeliteHeapCurrentBlockSize;       // remaining size of the current heap block for each thread

char **DeliteHeapSavedBlock;
size_t *DeliteHeapSavedBlockSize;

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

void DeliteHeapInit(int numThreads, size_t heapSize) {
  if (heapSize != 0) {
    defaultBlockSize = heapSize / numThreads;
  } else {
    defaultBlockSize = DEFAULT_BLOCK_SIZE;
  }

  size_t padSize = numThreads << PADDING_SHIFT;
  DeliteHeapBlockList = new std::list<char*>*[padSize];
  DeliteHeapCurrentBlock = new char*[padSize];
  DeliteHeapCurrentBlockSize = new size_t[padSize];
  DeliteHeapSavedBlock = new char*[padSize];
  DeliteHeapSavedBlockSize = new size_t[padSize];

  for (int i=0; i<numThreads; i++) {
    size_t padIdx = i << PADDING_SHIFT;
    DeliteHeapBlockList[padIdx] = new std::list<char*>();
    DeliteHeapCurrentBlock[padIdx] = NULL;
    DeliteHeapCurrentBlockSize[padIdx] = 0;
    DHEAP_DEBUG("finished heap initialization for resource %d\n", i);
  }
}

void DeliteHeapAllocBlock(size_t minSize, int idx) {
  size_t padIdx = idx << PADDING_SHIFT;
  size_t blockSize = std::max(minSize, defaultBlockSize);
  char *newBlock = new char[blockSize];
  if (!newBlock) {
    fprintf(stderr, "[ERROR]: Out of memory!");
    exit(-1);
  }
  memset(newBlock, 0, blockSize);
  DeliteHeapBlockList[padIdx]->push_back(newBlock);
  DeliteHeapCurrentBlock[padIdx] = newBlock;
  DeliteHeapCurrentBlockSize[padIdx] = blockSize;
  DHEAP_DEBUG("Insufficient existing heap space for thread %d. Allocating %lld MB.\n", idx, blockSize/1024/1024);
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

void *operator new(size_t sz, const resourceInfo_t *resourceInfo) {
  return DeliteHeapAlloc(sz, resourceInfo->threadId);
}

void *operator new[](size_t sz, const resourceInfo_t *resourceInfo) {
  return DeliteHeapAlloc(sz, resourceInfo->threadId);
}

void DeliteHeapClear(int numThreads) {
  for (int i=0; i<numThreads; i++) {
    size_t padIdx = i << PADDING_SHIFT;
    std::list<char*> *blocklist = DeliteHeapBlockList[padIdx];
    size_t heapUsage = blocklist->size();
    for (std::list<char*>::iterator iter = blocklist->begin(); iter != blocklist->end(); iter++) {
      delete[] *iter;
    }
    delete blocklist;

    DHEAP_DEBUG("finished heap clear for resource %d, used %d blocks (%lld MB blocks).\n", i, heapUsage, defaultBlockSize/1024/1024);
  }

  delete[] DeliteHeapBlockList;
  delete[] DeliteHeapCurrentBlock;
  delete[] DeliteHeapCurrentBlockSize;
  delete[] DeliteHeapSavedBlock;
  delete[] DeliteHeapSavedBlockSize;
}

/* saves the current position in the heap */
void DeliteHeapMark(int idx) {
  size_t padIdx = idx << PADDING_SHIFT;
  DeliteHeapSavedBlock[padIdx] = DeliteHeapCurrentBlock[padIdx];
  DeliteHeapSavedBlockSize[padIdx] = DeliteHeapCurrentBlockSize[padIdx];
}

/* resets the heap to the previous mark */
void DeliteHeapReset(int idx) {
  //FIXME: this leaks all blocks allocated between the mark and the current position!
  size_t padIdx = idx << PADDING_SHIFT;
  DHEAP_DEBUG("reseting heap for resource %d, used %lld MB.\n", idx, (DeliteHeapSavedBlockSize[padIdx]-DeliteHeapCurrentBlockSize[padIdx])/1024/1024);
  DeliteHeapCurrentBlock[padIdx] = DeliteHeapSavedBlock[padIdx];
  DeliteHeapCurrentBlockSize[padIdx] = DeliteHeapSavedBlockSize[padIdx];
}
