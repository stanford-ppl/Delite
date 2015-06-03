#include <map>
#include <vector>
#include <string>
#include <iostream>
#include <sys/time.h>
#include <jni.h>
#include <stdint.h>
#include <fstream>
#include <stack>
#include <sstream>
#include <unistd.h>
#include <cuda.h>

#ifndef DELITE_NUM_CUDA
#define DELITE_NUM_CUDA 1
#endif

typedef struct {
  struct timeval start;
} cudatimer_t;

#ifndef __DELITE_CPP_PROFILER_H__

class BufferedFileWriter {

  public:
    BufferedFileWriter(const char* fileName);
    void writeTimer(std::string kernel, long start, double duration, int32_t level, int32_t tid, bool isMultiLoop);
    void close();

  private:
    std::ofstream fs; 
};

#endif

void InitDeliteCudaTimer(int32_t tid, int32_t lowestCudaTid);
void DeliteCudaTimerStart(int32_t tid, std::string name);
void DeliteCudaTimerStop(int32_t tid, std::string name, bool isMultiLoop = false);
void DeliteCudaTimerClose(int32_t tid, int32_t rid, JNIEnv* env);
