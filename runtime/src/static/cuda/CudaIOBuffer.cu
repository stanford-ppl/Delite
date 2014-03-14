#include <assert.h>
#include <stdio.h>
#include "CudaIOBuffer.h"

enum LEVEL { NONE, LOW, MEDIUM, HIGH };

#define NONE

byte * CudaIOBuffer::allocate(size_t size) {
  //this assert is triggered if you call allocate multiple times violating your contract.
  assert(pendingAlloc == NULL);
  
  while(true) {
    if(size > remainingCapacity()) {
      //need to free some stuff
#ifdef DEBUG
      printf("[%lu] requested, need to free some stuff\n", size);
#endif


      //get an allocation
      if(allocations.empty()) {
	//panic, cannot allocate enout
	printf("[DELITE][CUDA][PANIC]IO buffer cannot satify application requirements\n");
	exit(-1);
      }
      AllocationInfo * alloc = allocations.front();
      allocations.pop();
      //wait for the event here
      assert(cudaEventSynchronize(alloc->event) == cudaSuccess);
      assert(cudaEventDestroy(alloc->event) == cudaSuccess);
#ifdef DEBUG
      printf("alloc->ptr[%p]size[%lu] head[%p] free[%p] used[%p]\n", alloc->ptr,alloc->size, head, free, used);
#endif

      //need to verify if I will have to wrap
      if(free + size > head + capacity) {
	//need to wrap
#ifdef DEBUG
	printf("not enough space left, need to wrap\n");
#endif

	//check that if I need to free up anything beyond the freePtr at this stage
	if(alloc->ptr >= free) {
	  assert(alloc->ptr == used);
	  used += alloc->size;	  
#ifdef DEBUG
	  printf("freeing[removing-excess] alloc->ptr[%p][%lu] used[%p]\n", alloc->ptr, alloc->size, used);
#endif
	  delete alloc;
	  //just check if I need to wrap the used ptr, I will need to do so if the next alloc is pointing to head
	  alloc = allocations.front();
#ifdef DEBUG
	  if(alloc != NULL)
	    printf("peeking[removing-excess] alloc->ptr[%p][%lu] used[%p]\n", alloc->ptr, alloc->size, used);
#endif
  	  if(alloc == NULL) {
	    //no more allocations to free, so I should just reset everything and ccntinue loop
	    used = head;
	    free = head;
#ifdef DEBUG
	    printf("resetting buffer free[%p] used[%p]\n", free, used);
#endif
	  } else if(alloc->ptr == head) { 
	    // need to wrap the used ptr
	    used = head;
#ifdef DEBUG
	    printf("wrapping used free[%p] used[%p]\n", free, used);
#endif
	  } else {
	    // still need to free up some remaining used alloc
	  }
	  //either way go through the logic again
	  continue;
	}

	
	//still have an alloc, it better be at the front of the queue
	assert(alloc->ptr == head);
	used = head + alloc->size;
	free = head;
#ifdef DEBUG
	printf("freeing[simple-wrap] alloc->ptr[%p][%lu] used[%p]\n", alloc->ptr, alloc->size, used);
#endif
	delete alloc;
	if(allocations.empty()) {
	  //freed everything reset
#ifdef DEBUG
	  printf("resetting[simple-wrap]\n");
#endif
	  used = head;
	}
	//try again
	continue;
      }
      
      //here, everything should be normal and my usedPtr should be in front of my freePtr
      assert(used >= free);
      //and my used ptr need to equal my allocation ptr
      assert(alloc->ptr == used);
      //free just one thing
      used += alloc->size;
#ifdef DEBUG
      printf("freeing[simple] alloc->ptr[%p][%lu] used[%p]\n", alloc->ptr, alloc->size, used);
#endif
      delete alloc;
      //verify that where the next used allocation is at, and reset the used ptr, sometimes there is nothing left
      alloc = allocations.front();
      if(alloc == NULL) {
	free = head;
	used = head;
#ifdef DEBUG
	printf("resetting[simple-empty] free[%p] used[%p]\n", free, used);
#endif
      } else if (alloc->ptr != used) {
	//in this instance there was nothing else to free in front of me, used should be at head
	assert(alloc->ptr == head);
	used = head;
#ifdef DEBUG
	printf("resetting[simple-used] free[%p] used[%p]\n", free, used);
#endif
      }
      continue;

    }
    else {
#ifdef DEBUG
      printf("[%p]allocating %zu bytes in buffer %zu remaining.\n", free, size, remainingCapacity());
#endif
      //printf("head[%p]|free[%p]|used[%p]|wrap[%p]|size[%zu]\n", inputBuffer.head, inputBuffer.freePtr, inputBuffer.usedPtr, inputBuffer.wrapPtr, inputBuffer.size);
      //check if I am at the end of the buffer
      AllocationInfo* ai = new AllocationInfo();
      ai->ptr = free;
      free += size;
      ai->size = size;
      allocations.push(ai);
      pendingAlloc = ai;
      //done so exit
      return ai->ptr;
    }
  }
  
}

size_t CudaIOBuffer::remainingCapacity() {
  //there are a couple of distinct cases

  //buffer somewhat full
  if(free > used) {
    return capacity - (free - head);
  }

  if(free < used)
    return used - free;
  
  //buffer empty: this should occur when both ptrs equal each other and equal the kernel buffer head
  if(free == head && used == head)
    return capacity;
  
  //buffer full: this occur when ptrs equal each other but not the kernel buffer head, or when the nextPtr is
  if(free == used || free == head + capacity)
    return 0;

  //should not hit this case
  assert(0);
  return 0;
}

CudaIOBuffer::CudaIOBuffer(size_t size, cudaMemcpyKind kind, cudaStream_t stream) {
  cudaHostAlloc(&head, size, cudaHostAllocDefault);
  if(head == NULL) {
    printf("[DELITE][CUDA][ERROR]Could not allocate requested space for buffer.\n");
    exit(-1);
  }
  capacity = size;
  free = head;
  used = head;
  allocations = std::queue<AllocationInfo *>();
  pendingAlloc = NULL;
  direction =  kind;
  cudaStream = stream;
}

void CudaIOBuffer::copyAsync(byte* dst, byte* src, size_t size) {
  if(direction == cudaMemcpyHostToDevice) 
    assert(src == pendingAlloc->ptr);
  else 
    assert(dst == pendingAlloc->ptr);    
  assert(cudaMemcpyAsync(dst, src, size, direction, cudaStream) == cudaSuccess);
  assert(cudaEventCreateWithFlags(&(pendingAlloc->event), cudaEventBlockingSync) == cudaSuccess);
  assert(cudaEventRecord(pendingAlloc->event, cudaStream) == cudaSuccess);
  pendingAlloc = NULL;
}




int main() {
  
  cudaStream_t stream;
  assert(cudaStreamCreate(&stream) == cudaSuccess);
  CudaIOBuffer buffer = CudaIOBuffer(4096*2, cudaMemcpyHostToDevice, stream);
  byte * dptr;
  cudaMalloc(&dptr, 5000000);
  for(int i = 0; i < 100; i++) {
    int k;
    if( i > 85) 
      k = 1024;
    else if (i > 65)
      k = 7011;
    else if (i > 35 )
      k = 666;
    else if (i > 13 )
      k = 512;
    else k = 1024;

    byte * ptr = buffer.allocate(k);
    buffer.copyAsync(dptr, ptr, k);
    dptr += k;
  }
}
