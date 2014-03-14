#ifndef _CUDAHOST_LIST_H_
#define _CUDAHOST_LIST_H_

#include "cppList.h"
#include "cudaList.h"

template <class T>
class HostcudaList {
public:
  cppList<T> *host;
  cudaList<T> *device;
};

#endif
