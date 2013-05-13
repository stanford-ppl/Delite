#ifndef _CUDA_LIST_H_
#define _CUDA_LIST_H_

#include "DeliteCuda.h"

#include <stdlib.h>

template <class T>
class cudaList {
public:
    T *data;
    int length;

    // Constructor
    __host__ __device__ cudaList(void) {
      length = 0;
      data = NULL;
    }

    __device__ cudaList(int _length, T *ptr, int idx) {
      length = _length;
      data = ptr + idx*_length;
    }

    __host__ cudaList(int _length) {
        length = _length;
        DeliteCudaMalloc((void**)&data,length*sizeof(T));
    }

    __host__ __device__ cudaList(int _length, T *_data) {
        length = _length;
        data = _data;
    }

    __host__ __device__ T apply(int idx) {
        return data[idx];
    }

    __host__ __device__ void update(int idx, T value) {
        data[idx] = value;
    }

    // DeliteCoolection
    __host__ __device__ int size() {
        return length;
    }

    __host__ __device__ T dcApply(int idx) {
        return data[idx];
    }

    __host__ __device__ void dcUpdate(int idx, T value) {
        data[idx] = value;
    }
    
};

#endif
