#ifndef _CUDAHOST_REF_H_
#define _CUDAHOST_REF_H_

#include "cppRef.h"
#include "cudaRef.h"

template <class T>
class HostcudaRef {
public:
  cppRef<T> *host;
  cudaRef<T> *device;
};

#endif
