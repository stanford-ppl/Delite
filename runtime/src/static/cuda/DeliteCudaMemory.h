#ifndef __DELITE_CUDA_MEMORY_H__
#define __DELITE_CUDA_MEMORY_H__

class DeliteCudaMemory {
public:
  virtual __host__ void incRefCnt(void) = 0;
  virtual __host__ void decRefCnt(void) = 0;
};
#endif
