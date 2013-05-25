#ifndef _CUDA_REF_H_
#define _CUDA_REF_H_

template <class T>
class cudaRef {
public:
    T data;

    __host__ __device__ cudaRef(void) {
      data = NULL;
    }

    __host__ __device__ cudaRef(T _data) {
      data = _data;
    }

    __host__ __device__ T get(void) {
      return data;
    }

    __host__ __device__ void set(T newVal) {
        data = newVal;
    }
};

#endif
