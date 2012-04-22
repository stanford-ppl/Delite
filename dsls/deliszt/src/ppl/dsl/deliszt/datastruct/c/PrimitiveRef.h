#ifndef _PRIMITIVEREF_H_
#define _PRIMITIVEREF_H_

template <class T>
class PrimitiveRef {
public:
  T value;

  __device__ __host__ T get() {
    return value;
  }

  __device__ __host__ void set(T newVal) {
    value = newVal;
  }
};

#endif
