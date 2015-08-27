#include "DeliteMemory.h"

#include <iostream>

#define DEFAULT_BLOCK_SIZE 1UL*1024*1024

typedef std::list< std::pair<char *, size_t> > blockList; 
                                                                          

size_t defaultBlockSize;
blockList **DeliteHeapBlockList = NULL;          // list of allocated heap blocks for each thread
char **DeliteHeapCurrentBlock;                   // current heap block pointer for each thread
size_t *DeliteHeapCurrentBlockSize;              // remaining size of the current heap block for each thread
blockList::iterator *DeliteHeapCurrentBlockPtr;  //points to the currently in-use block in the list (if any)

char **DeliteHeapSavedBlock;
size_t *DeliteHeapSavedBlockSize;
blockList::iterator *DeliteHeapSavedBlockPtr;

// Temporary Heap for loops
blockList **TempHeapBlockList = NULL;          // list of allocated heap blocks for each thread used for temporary loop storage
char **TempHeapCurrentBlock;                   // current heap block pointer for each thread
size_t *TempHeapCurrentBlockSize;              // remaining size of the current heap block for each thread
blockList::iterator *TempHeapCurrentBlockPtr;  //points to the currently in-use block in the list (if any)

bool *UseTempHeap; // Flag to use the temporary heap. Set when a loop is entered


pthread_mutex_t barrier_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t barrier_cond = PTHREAD_COND_INITIALIZER;
unsigned int arrivedCount = 0;

#define PADDING_SHIFT 6 // align thread-local variables with 64 bytes to avoid false sharing
#define ALIGN_SIZE 8 //align all logical allocations to 8 byte boundaries within the block 

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
  DeliteHeapBlockList = new blockList*[padSize];
  DeliteHeapCurrentBlock = new char*[padSize];
  DeliteHeapCurrentBlockSize = new size_t[padSize];
  DeliteHeapCurrentBlockPtr = new blockList::iterator[padSize];
  
  DeliteHeapSavedBlock = new char*[padSize];
  DeliteHeapSavedBlockSize = new size_t[padSize];
  DeliteHeapSavedBlockPtr = new blockList::iterator[padSize];

  TempHeapBlockList = new blockList*[padSize];
  TempHeapCurrentBlock = new char*[padSize];
  TempHeapCurrentBlockSize = new size_t[padSize];
  TempHeapCurrentBlockPtr = new blockList::iterator[padSize];

  UseTempHeap = new bool[padSize];

  for (int i=0; i<numThreads; i++) {
    size_t padIdx = i << PADDING_SHIFT;
    DeliteHeapBlockList[padIdx] = new blockList();
    DeliteHeapCurrentBlock[padIdx] = NULL;
    DeliteHeapCurrentBlockSize[padIdx] = 0;
    DeliteHeapCurrentBlockPtr[padIdx] = DeliteHeapBlockList[padIdx]->begin();

    TempHeapBlockList[padIdx] = new blockList();
    TempHeapCurrentBlock[padIdx] = NULL;
    TempHeapCurrentBlockSize[padIdx] = 0;
    TempHeapCurrentBlockPtr[padIdx] = TempHeapBlockList[padIdx]->begin();

    UseTempHeap[padIdx] = false;

    DHEAP_DEBUG("finished heap initialization for resource %d\n", i);
  }
}

void DeliteHeapAllocBlock(size_t realSize, size_t minSize, int idx, bool initialize) {
  size_t padIdx = idx << PADDING_SHIFT;

  blockList **CurrHeapBlockList = DeliteHeapBlockList;
  char **CurrHeapCurrentBlock = DeliteHeapCurrentBlock;                   
  size_t *CurrHeapCurrentBlockSize = DeliteHeapCurrentBlockSize;
  blockList::iterator *CurrHeapCurrentBlockPtr = DeliteHeapCurrentBlockPtr;

  if(UseTempHeap[padIdx]){ // If in a loop
    CurrHeapBlockList = TempHeapBlockList;
    CurrHeapCurrentBlock = TempHeapCurrentBlock; 
    CurrHeapCurrentBlockSize = TempHeapCurrentBlockSize;
    CurrHeapCurrentBlockPtr = TempHeapCurrentBlockPtr;
  }


  size_t blockSize = std::max(minSize, defaultBlockSize);
  blockList::iterator candidateBlock = ++CurrHeapCurrentBlockPtr[padIdx]; //check the next block
  char *newBlock;

  if (candidateBlock == CurrHeapBlockList[padIdx]->end() || candidateBlock->second < blockSize) { //need a fresh block
    newBlock = new char[blockSize];
    if (!newBlock) {
      fprintf(stderr, "[ERROR]: Out of memory!");
      exit(-1);
    }
    CurrHeapBlockList[padIdx]->insert(candidateBlock, std::pair<char*, size_t>(newBlock, blockSize));
    --CurrHeapCurrentBlockPtr[padIdx];
    DHEAP_DEBUG("Insufficient existing heap space for thread %d. Allocating %lld MB.\n", idx, blockSize/1024/1024);
  } else { //large enough block already exists
    newBlock = candidateBlock->first;
    blockSize = candidateBlock->second;
  }

  if (initialize) memset(newBlock, 0, blockSize);
  CurrHeapCurrentBlock[padIdx] = newBlock;
  CurrHeapCurrentBlockSize[padIdx] = blockSize;
}

//size_t totalAllocated = 0;
char *DeliteHeapAlloc(size_t sz, int idx, bool initialize) {
  //TODO: Need alignment for each type (passed as parameter)?
  DHEAP_DEBUG("DeliteHeapAlloc called for idx %d with size %d\n", idx, sz);
  std::cout << "Alloc idx: " << idx << ": Size: " << sz << std::endl;
  size_t padIdx = idx << PADDING_SHIFT;
  size_t alignedSize = (sz+ALIGN_SIZE-1) & ~(ALIGN_SIZE-1);
  //totalAllocated += alignedSize;

  // Add to either the temp or perm heap based on where allocation is from
  char **CurrHeapCurrentBlock = DeliteHeapCurrentBlock;                   
  size_t *CurrHeapCurrentBlockSize = DeliteHeapCurrentBlockSize;
  if(UseTempHeap[padIdx]){ // If in a loop
    std::cout << "Using Temp Heap" << std::endl;
    CurrHeapCurrentBlock = TempHeapCurrentBlock; 
    CurrHeapCurrentBlockSize = TempHeapCurrentBlockSize;
  }

  size_t blockSize = CurrHeapCurrentBlockSize[padIdx];
  // obtain a new heap block if more space is needed
  if (alignedSize > blockSize) {
    DeliteHeapAllocBlock(sz, alignedSize, idx, initialize);
  }

  char* ptr = CurrHeapCurrentBlock[padIdx];
  CurrHeapCurrentBlock[padIdx] += alignedSize;
  CurrHeapCurrentBlockSize[padIdx] -= alignedSize;
  return ptr;
}

void *operator new(size_t sz, const resourceInfo_t *resourceInfo) {
  return DeliteHeapAlloc(sz, resourceInfo->threadId, true); //FIXME: init!
}

void *operator new[](size_t sz, const resourceInfo_t *resourceInfo) {
  return DeliteHeapAlloc(sz, resourceInfo->threadId, true);
}

void DeliteHeapClear(int numThreads) {
  for (int i=0; i<numThreads; i++) {
    size_t padIdx = i << PADDING_SHIFT;
    blockList *localList = DeliteHeapBlockList[padIdx];
    size_t numBlocks = localList->size();
    size_t heapUsage = 0;
    for (blockList::iterator iter = localList->begin(); iter != localList->end(); iter++) {
      heapUsage += iter->second;
      delete[] iter->first;
    }
    delete localList;
    //fprintf(stderr, "finished heap clear for resource %d, used %f MB (%ld blocks).\n", i, heapUsage*1.0/1024/1024, numBlocks);
    //fprintf(stderr, "logically allocated %f MB.\n", totalAllocated*1.0/1024/1024);


    // Clear temporary heap
    localList = TempHeapBlockList[padIdx];
    numBlocks = localList->size();
    heapUsage = 0;
    for (blockList::iterator iter = localList->begin(); iter != localList->end(); iter++) {
      heapUsage += iter->second;
      delete[] iter->first;
    }
    delete localList;
    //fprintf(stderr, "finished heap clear for resource %d, used %f MB (%ld blocks).\n", i, heapUsage*1.0/1024/1024, numBlocks);
    //fprintf(stderr, "logically allocated %f MB.\n", totalAllocated*1.0/1024/1024);
  }

  delete[] DeliteHeapBlockList;
  delete[] DeliteHeapCurrentBlock;
  delete[] DeliteHeapCurrentBlockSize;
  delete[] DeliteHeapCurrentBlockPtr;
  delete[] DeliteHeapSavedBlock;
  delete[] DeliteHeapSavedBlockSize;
  delete[] DeliteHeapSavedBlockPtr;

  delete[] TempHeapBlockList;
  delete[] TempHeapCurrentBlock;
  delete[] TempHeapCurrentBlockSize;
  delete[] TempHeapCurrentBlockPtr;

  delete[] UseTempHeap;
}

/* saves the current position in the heap */
//int marks = 0;
void DeliteHeapMark(int idx) {
  // if (marks < 100) {
  //   fprintf(stderr, "heap before %d mark: %f\n", marks, totalAllocated*1.0/1024/1024);
  //   marks++;
  // }

  size_t padIdx = idx << PADDING_SHIFT;
  DeliteHeapSavedBlock[padIdx] = DeliteHeapCurrentBlock[padIdx];
  DeliteHeapSavedBlockSize[padIdx] = DeliteHeapCurrentBlockSize[padIdx];
  DeliteHeapSavedBlockPtr[padIdx] = DeliteHeapCurrentBlockPtr[padIdx];
}

/* resets the heap to the previous mark */
void DeliteHeapReset(int idx) {
  // size_t padIdx = idx << PADDING_SHIFT;
  // DeliteHeapCurrentBlock[padIdx] = DeliteHeapSavedBlock[padIdx];
  // DeliteHeapCurrentBlockSize[padIdx] = DeliteHeapSavedBlockSize[padIdx];
  // DeliteHeapCurrentBlockPtr[padIdx] = DeliteHeapSavedBlockPtr[padIdx];


  size_t padIdx = idx << PADDING_SHIFT;
  

  //printf("Address before is %p\n", (void *)TempHeapCurrentBlock[padIdx]);


  // Reset temp heap to point to the first block in the list
  blockList::iterator firstBlock = TempHeapBlockList[padIdx]->begin();
  if(firstBlock !=  TempHeapBlockList[padIdx]->end()){
    std::cout << "RESETTING temp heap" << std::endl;
    TempHeapCurrentBlock[padIdx] = firstBlock->first;
    //std::cout << " Current size: " << TempHeapCurrentBlockSize[padIdx] << std::endl;
    TempHeapCurrentBlockSize[padIdx] = firstBlock->second;
    //std::cout << " New Size: " << firstBlock->second << std::endl;
    TempHeapCurrentBlockPtr[padIdx] = firstBlock;
  }

  //printf("Address of start is %p\n", (void *)TempHeapCurrentBlock[padIdx]);



}

void StartTempHeap(int threadId){
  size_t padIdx = threadId << PADDING_SHIFT;
  UseTempHeap[padIdx] = true;
  std::cout << "Starting temp heap. idx: " << threadId << std::endl;
}

void StopTempHeap(int threadId){
  size_t padIdx = threadId << PADDING_SHIFT;
  UseTempHeap[padIdx] = false;
  std::cout << "Stopping temp heap. idx: " << threadId << std::endl;

}


void CompactPermHeap(int idx /*Also need root set*/){

}


