#ifndef _CUDAHOST_DELITEARRAY_H_
#define _CUDAHOST_DELITEARRAY_H_

#include "cppDeliteArray.h"
#include "cudaDeliteArray.h"

template <class T>
class HostcudaDeliteArray {
public:
	cppDeliteArray<T> *host;
	cudaDeliteArray<T> *device;
};

#endif
