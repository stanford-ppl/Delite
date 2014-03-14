#ifndef _DELITE_OPENCL_H_
#define _DELITE_OPENCL_H_

#include <list>
#include <map>
#include <queue>
#include <iostream>
#include <utility>
#include <CL/cl.h>

using namespace std;

// Second element in pair<void*,bool> indicates that void* points to GPU device memory,
// so should not call free() on it.
struct FreeItem {
    //cudaEvent_t event;
    list< pair<void*,bool> >* keys;      // List of pointers to the DSL Objects (e.g. pointer to DoubleVector object on the host side)
};

extern cl_context context;
extern cl_command_queue command_queue;
extern list<cl_mem>* lastAlloc;
extern queue<FreeItem>* freeList;
extern map<void*,list<cl_mem>*>* clMemoryMap;

extern void hostInit();
extern void DeliteOpenCLMemcpyHtoDAsync(cl_mem dest, void* sptr, size_t size);
extern void DeliteOpenCLMemcpyDtoHAsync(void* dptr, cl_mem src, size_t size);
extern void DeliteOpenCLMemcpyDtoDAsync(cl_mem dst, cl_mem src, size_t size);
extern void freeCLMemory(FreeItem item);
extern cl_mem DeliteOpenCLMallocImage(size_t width, size_t height);
extern cl_mem DeliteOpenCLMalloc(size_t size);
extern cl_mem DeliteOpenCLHostMalloc(size_t size, void *ptr);
//TODO: Implement this
//extern void DeliteCudaMemcpyDtoDAsync(void *dptr, void* sptr, size_t size);

#endif

