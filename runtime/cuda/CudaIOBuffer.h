#ifndef CUDAIOBUFFER_H
#define CUDAIOBUFFER_H

#include <queue>


typedef unsigned char byte;

// Free items for our Cuda Allocator
class AllocationInfo {
public:
  cudaEvent_t event;
  size_t size;
  byte * ptr;
}; 


class CudaIOBuffer {
 public:
  byte *head;
  size_t capacity;
  byte *free;
  byte *used;
  
  
  CudaIOBuffer(size_t size, cudaMemcpyKind kind, cudaStream_t stream);
  byte * allocate(size_t size);
  void copyAsync(byte* dst, byte* src, size_t size);
  size_t remainingCapacity();
  
 private:
  std::queue<AllocationInfo *> allocations;
  AllocationInfo * pendingAlloc;
  cudaMemcpyKind direction;
  cudaStream_t cudaStream;
  
};


#endif
