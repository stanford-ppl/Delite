#ifndef _MATIMPL_H_
#define _MATIMPL_H_

#include "VecImpl.h"

template <class T, int R, int C>
class Mat {
public:
  T data[R*C];

  // Accessor Functions
  __host__ __device__ T apply(int idxR, int idxC) {
    return data[idxR*C+idxC];
  }
__host__ __device__ void update(int idxR, int idxC, T newVal) {
    data[idxR*C+idxC] = newVal;
  }

  // DeliteCollection
  __host__ __device__ int dcSize() {
      return R * C;
  }
  __host__ __device__ T dcApply(int idx) {
      return data[idx];
  }
  __host__ __device__ void dcUpdate(int idx, T value) {
      data[idx] = value;
  }

  // Vector Update
  __host__ __device__ void vectorUpdate(int idx, Vec<T,C> vec) {
      for(int i=0; i<C; i++) 
          data[idx*C+i] = vec.data[i];
  }
};

#endif
