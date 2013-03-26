#ifndef _LIST_H_
#define _LIST_H_

#include "DeliteCuda.h"

#include <stdlib.h>

template <class T>
class List {
public:
    T *data;
    int length;

    // Constructor
    __host__ __device__ List(void) {
      length = 0;
      data = NULL;
    }

    __device__ List(int _length, T *ptr, int idx) {
      length = _length;
      data = ptr + idx*_length;
    }

    __host__ List(int _length) {
        length = _length;
        DeliteCudaMalloc((void**)&data,length*sizeof(T));
    }

    __host__ __device__ List(int _length, T *_data) {
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
