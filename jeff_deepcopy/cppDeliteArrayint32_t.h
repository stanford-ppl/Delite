#ifndef __cppDeliteArrayint32_t__
#define __cppDeliteArrayint32_t__

#include "DeliteNamespaces.h"
#include "DeliteMemory.h"
#ifdef __DELITE_CPP_NUMA__
#include <numa.h>
#endif

class cppDeliteArrayint32_t : public DeliteMemory {
public:
  int32_t  *data;
  int length;
  bool permanent = false;

  cppDeliteArrayint32_t(int _length, resourceInfo_t *resourceInfo): data((int32_t  *)(new (resourceInfo) int32_t [_length])), length(_length) { }

  cppDeliteArrayint32_t(int _length): data((int32_t  *)(new int32_t [_length])), length(_length) { }

  cppDeliteArrayint32_t(int32_t  *_data, int _length) {
    data = _data;
    length = _length;
  }

  cppDeliteArrayint32_t(int32_t  *_data, int _length, bool _permanent) {
    data = _data;
    length = _length;
    permanent = _permanent;
  }

  int32_t  apply(int idx) {
    return data[idx];
  }

  void update(int idx, int32_t  val) {
    data[idx] = val;
  }

  void print(void) {
    printf("length is %d\n", length);
  }

  bool equals(cppDeliteArrayint32_t *to) {
    return this == this;
  }

  uint32_t hashcode(void) {
    return (uintptr_t)this;
  }

#ifdef DELITE_GC
  void *deepCopy(const resourceInfo_t *resourceInfo, int idx) {
    // If already in perm heap
    if(permanent){
      return this;
    }

    // Creating new array in perm memory and returning new cppDeliteArrayint32_t
    int32_t  *old = data;
    StopTempHeap(resourceInfo->threadId);
    data = (int32_t *)DeliteHeapAlloc(length * sizeof(data[0]), resourceInfo->threadId, true);
    StartTempHeap(resourceInfo->threadId);
    memcpy(data, old, length * sizeof(old[0]));
    return new cppDeliteArrayint32_t(data, length, true);
  }
#endif

};

struct cppDeliteArrayint32_tD {
  void operator()(cppDeliteArrayint32_t *p) {
    //printf("cppDeliteArrayint32_t: deleting %p\n",p);
    delete[] p->data;
  }

/*
#ifdef __DELITE_CPP_NUMA__
  const bool isNuma;
  int32_t  **wrapper;
  size_t numGhostCells; // constant for all internal arrays
  size_t *starts;
  size_t *ends;
  size_t numChunks;

  cppDeliteArrayint32_t(int _length, resourceInfo_t *resourceInfo): data((int32_t  *)(new (resourceInfo) int32_t [_length])), length(_length), isNuma(false) { }

  cppDeliteArrayint32_t(int _length): data((int32_t  *)(new int32_t [_length])), length(_length), isNuma(false) { }

  cppDeliteArrayint32_t(int32_t  *_data, size_t _length): data(_data), length(_length), isNuma(false) { }

  cppDeliteArrayint32_t(size_t _length, size_t _numGhostCells, size_t _numChunks) : data(NULL), length(_length), isNuma(true) {
    //FIXME: transfer functions rely on data field
    numGhostCells = _numGhostCells;
    numChunks = _numChunks;
    wrapper = (int32_t  **)malloc(numChunks*sizeof(int32_t *));
    starts = (size_t *)malloc(numChunks*sizeof(size_t)); //TODO: custom partitioning
    ends = (size_t *)malloc(numChunks*sizeof(size_t));
    for (int sid = 0; sid < numChunks; sid++) {
      starts[sid] = std::max(length * sid / numChunks - numGhostCells, (size_t)0);
      ends[sid] = std::min(length * (sid+1) / numChunks + numGhostCells, length);
      allocInternal(sid, ends[sid]-starts[sid]);
    }
  }

  void allocInternal(int socketId, size_t length) {
    wrapper[socketId] = (int32_t *)numa_alloc_onnode(length*sizeof(int32_t ), socketId);
  }

  int32_t  apply(size_t idx) {
    if (isNuma) {
      for (size_t sid = 0; sid < numChunks; sid++) {
        if (idx < ends[sid]) return wrapper[sid][idx-starts[sid]]; //read from first location found
      }
      assert(false); //throw runtime_exception
    }
    else
      return data[idx];
  }

  void update(size_t idx, int32_t  val) {
    if (isNuma) {
      for (size_t sid = 0; sid < numChunks; sid++) {
        size_t offset = starts[sid];
        if (idx >= offset && idx < ends[sid]) wrapper[sid][idx-offset] = val; //update all ghosts
      }
    }
    else
      data[idx] = val;
  }

  //read locally if available, else remotely
  int32_t  applyAt(size_t idx, size_t sid) {
    //size_t sid = config->threadToSocket(tid);
    size_t offset = starts[sid];
    if (idx >= offset && idx < ends[sid]) return wrapper[sid][idx-offset];
    return apply(idx);
  }

  int32_t  unsafe_apply(size_t socketId, size_t idx) {
    return wrapper[socketId][idx];
  }

  //update locally, ghosts need to be explicitly synchronized
  void updateAt(size_t idx, int32_t  value, size_t sid) {
    //size_t sid = config->threadToSocket(tid);
    size_t offset = starts[sid];
    if (idx >= offset && idx < ends[sid]) wrapper[sid][idx-offset] = value;
    //else throw runtime_exception
  }

  void unsafe_update(size_t socketId, size_t idx, int32_t  value) {
    wrapper[socketId][idx] = value;
  }
#endif
*/
};

#endif
