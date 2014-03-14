#include "DeliteOpenCL.h"

using namespace std;

list<cl_mem> *lastAlloc = new list<cl_mem>();
queue<FreeItem> *freeList = new queue<FreeItem>();
map<void*,list<cl_mem>*> *clMemoryMap = new map<void*,list<cl_mem>*>();

void hostInit() {
	printf("hostInit() is called.\n");

}

/* allocate a buffer and returns the cl_mem object */
// TODO: Add the flags argument for the allocation flags
/*
cl_mem DeliteOpenCLMalloc(size_t size) {
  //printf("Called DeliteOpenCLMallloc for size %d\n", size);
  cl_int ret = CL_SUCCESS;
  cl_mem buf = clCreateBuffer(context,CL_MEM_READ_WRITE,size,NULL,&ret);
  if(ret != CL_SUCCESS)
    printf("ERROR: DeliteOpenCLMalloc failed (%d)!\n",size);
  return buf;
}
*/

/*
cl_mem DeliteOpenCLHostMalloc(size_t size, void *ptr) {
  //printf("Called DeliteOpenCLMallloc for size %d\n", size);
  cl_int ret = CL_SUCCESS;
  cl_mem buf = clCreateBuffer(context,CL_MEM_READ_ONLY | CL_MEM_USE_HOST_PTR,size,ptr,&ret);
  if(ret != CL_SUCCESS)
    printf("ERROR: DeliteOpenCLMalloc failed (%d)!\n",size);
  return buf;
}
*/

void DeliteOpenCLMemcpyHtoDAsync(cl_mem dest, void* sptr, size_t size) {
  //printf("Copy H to D is called\n");
  cl_event event;
  cl_int ret = clEnqueueWriteBuffer(command_queue, dest, CL_TRUE, 0, size, sptr, 0, NULL, &event);
#ifdef PROFILE_ENABLE
  clFinish(command_queue);
  clWaitForEvents(1, &event); 
  cl_ulong start;
  cl_ulong end;
  clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_START, sizeof(cl_ulong), &start, NULL);
  clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(cl_ulong), &end, NULL);
  float exeTime = (end - start) * 1.0e-6f;
  printf("Transfer Time (H to D) %d: %f [ms]\n", size, exeTime);
#endif
  if(ret != CL_SUCCESS)
  printf("ERROR: DeliteOpenCLMemcpyHtoDAsync failed!\n");
  clReleaseEvent(event);
}

void DeliteOpenCLMemcpyDtoHAsync(void* dptr, cl_mem src, size_t size) {
  //printf("Copy D to H is called\n");
  cl_event event;
  cl_int ret = clEnqueueReadBuffer(command_queue, src, CL_TRUE, 0, size, dptr, 0, NULL, &event);
#ifdef PROFILE_ENABLE
  clFinish(command_queue);
  clWaitForEvents(1, &event); 
  cl_ulong start;
  cl_ulong end;
  clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_START, sizeof(cl_ulong), &start, NULL);
  clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(cl_ulong), &end, NULL);
  float exeTime = (end - start) * 1.0e-6f;
  printf("Transfer Time (D to H) %d: %f [ms]\n", size, exeTime);
#endif
  if(ret != CL_SUCCESS)
  printf("ERROR: DeliteOpenCLMemcpyDtoHAsync failed!\n");
  //TODO: Always call clEnqueueReadBuffer with blocking?
  clReleaseEvent(event);
}

void DeliteOpenCLMemcpyDtoDAsync(cl_mem dst, cl_mem src, size_t size) {
  cl_event event;
  cl_int ret = clEnqueueCopyBuffer(command_queue, src, dst, 0, 0, size, 0, NULL, &event);
	if(ret != CL_SUCCESS)
  printf("ERROR: DeliteOpenCLMemcpyDtoDAsync failed!\n");
  clReleaseEvent(event);
}


void freeCLMemory(FreeItem item) {
    list< pair<void*,bool> >::iterator iter;
    for (iter = item.keys->begin(); iter != item.keys->end(); iter++) {
      //cout << "object ref: " << (long) *iter << endl;
      if(clMemoryMap->find((*iter).first) != clMemoryMap->end()) {
        list<cl_mem>* freePtrList = clMemoryMap->find((*iter).first)->second;
        list<cl_mem>::iterator iter2;
        for (iter2 = freePtrList->begin(); iter2 != freePtrList->end(); iter2++) {
          clReleaseMemObject(*iter2);
        }
        clMemoryMap->erase((*iter).first);
        delete freePtrList;
       	if(!((*iter).second)) free((*iter).first);
      }
    }
    delete item.keys;
}


cl_mem DeliteOpenCLMallocImage(size_t width, size_t height) {

  size_t size = width * height;
  cl_int ret = CL_SUCCESS;
  cl_image_format imgFormat;
  imgFormat.image_channel_order = CL_LUMINANCE;
  imgFormat.image_channel_data_type = CL_FLOAT;

  //First try to free everything that is freeable
  while (freeList->size() != 0) {
    FreeItem item = freeList->front();
    //if (cudaEventQuery(item.event) != cudaSuccess) {
    //  break;
    //}
    freeList->pop();
    //cudaEventDestroy(item.event);
    freeCLMemory(item);
  }

  //cl_mem buf = clCreateBuffer(context,CL_MEM_READ_WRITE,size,NULL,&ret);
  cl_mem buf = clCreateImage2D(context,CL_MEM_READ_ONLY,&imgFormat,width,height,0,NULL,&ret);

  while (ret != CL_SUCCESS) {
    //TODO: Without using the events, at this point the freeList is always empty
    if (freeList->size() == 0) {
      cout << "FATAL: Insufficient device memory" << endl;
      exit(-1);
    }
    FreeItem item = freeList->front();
    freeList->pop();

    //while (cudaEventQuery(item.event) != cudaSuccess) {
    //  cudaEventSynchronize(item.event);
    //}
    //cudaEventDestroy(item.event);
    freeCLMemory(item);
    buf = clCreateImage2D(context,CL_MEM_READ_ONLY,&imgFormat,width,height,0,NULL,&ret);
  }
  lastAlloc->push_back(buf);
  //if(ret != CL_SUCCESS) printf("ERROR: DeliteOpenCLMalloc failed (%d)!\n",size);
  return buf;
}

cl_mem DeliteOpenCLMalloc(size_t size) {
  //printf("alloc %d\n",size);

  cl_int ret = CL_SUCCESS;

  //First try to free everything that is freeable
  while (freeList->size() != 0) {
    FreeItem item = freeList->front();
    //if (cudaEventQuery(item.event) != cudaSuccess) {
    //  break;
    //}
    freeList->pop();
    //cudaEventDestroy(item.event);
    freeCLMemory(item);
  }

  cl_mem buf = clCreateBuffer(context,CL_MEM_READ_WRITE,size,NULL,&ret);
  while (ret != CL_SUCCESS) {
    //TODO: Without using the events, at this point the freeList is always empty
    if (freeList->size() == 0) {
      cout << "FATAL: Insufficient device memory" << endl;
      exit(-1);
    }
    FreeItem item = freeList->front();
    freeList->pop();

    //while (cudaEventQuery(item.event) != cudaSuccess) {
    //  cudaEventSynchronize(item.event);
    //}
    //cudaEventDestroy(item.event);
    freeCLMemory(item);
    buf = clCreateBuffer(context,CL_MEM_READ_WRITE,size,NULL,&ret);
  }
  lastAlloc->push_back(buf);
  //if(ret != CL_SUCCESS) printf("ERROR: DeliteOpenCLMalloc failed (%d)!\n",size);
  return buf;
}

cl_mem DeliteOpenCLHostMalloc(size_t size, void *ptr) {

  cl_int ret = CL_SUCCESS;

  //First try to free everything that is freeable
  while (freeList->size() != 0) {
    FreeItem item = freeList->front();
    //if (cudaEventQuery(item.event) != cudaSuccess) {
    //  break;
    //}
    freeList->pop();
    //cudaEventDestroy(item.event);
    freeCLMemory(item);
  }

  cl_mem buf = clCreateBuffer(context,CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR,size,ptr,&ret);
  while (ret != CL_SUCCESS) {
    //TODO: how could it be possible to have more free things despite former while loop?
    if (freeList->size() == 0) {
      cout << "FATAL: Insufficient device memory" << endl;
      exit(-1);
    }
    FreeItem item = freeList->front();
    freeList->pop();

    //while (cudaEventQuery(item.event) != cudaSuccess) {
    //  cudaEventSynchronize(item.event);
    //}
    //cudaEventDestroy(item.event);
    freeCLMemory(item);
    cl_mem buf = clCreateBuffer(context,CL_MEM_READ_WRITE | CL_MEM_USE_HOST_PTR,size,ptr,&ret);
  }
  lastAlloc->push_back(buf);
  //if(ret != CL_SUCCESS) printf("ERROR: DeliteOpenCLMalloc failed (%d)!\n",size);
  return buf;
}

