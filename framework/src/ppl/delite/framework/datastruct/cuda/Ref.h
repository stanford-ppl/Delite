#ifndef _DELITEREF_H_
#define _DELITEREF_H_

template <class T>
class Ref {
public:
    T data;

    __host__ __device__ Ref(void) {
      data = NULL;
    }

    __host__ __device__ Ref(T _data) {
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
