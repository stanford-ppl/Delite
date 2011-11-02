#ifndef _MATIMPL_H_
#define _MATIMPL_H_

#include "VecImpl.h"

template <class T, int C>
class MatRow {
public:
  T data[C];

  // Accessor Functions
  __host__ __device__ T apply(int idx) {
    return data[idx];
  }
  __host__ __device__ void update(int idx, T newVal) {
    data[idx] = newVal;
  }
  __host__ __device__ T dcApply(int idx) {
    return data[idx];
  }
  __host__ __device__ void dcUpdate(int idx, T newVal) {
    data[idx] = newVal;
  }
};


template <class T, int R>
class MatCol {
public:
  T data[R];

  // Accessor Functions
  __host__ __device__ T apply(int idx) {
    return data[idx];
  }
  __host__ __device__ void update(int idx, T newVal) {
    data[idx] = newVal;
  }
  __host__ __device__ T dcApply(int idx) {
    return data[idx];
  }
  __host__ __device__ void dcUpdate(int idx, T newVal) {
    data[idx] = newVal;
  }
};


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

  // Get Column or Row
  __host__ __device__ MatCol<T,R> col(int idxC) {
      MatCol<T,R> ret;
      for(int i=0; i<R; i++) ret.data[i] = data[i*R+idxC];
      return ret;
  }
  __host__ __device__ MatRow<T,C> row(int idxR) {
      MatRow<T,C> ret;
      for(int i=0; i<C; i++) ret.data[i] = data[idxR*C+i];
      return ret;
  }

};

#endif
