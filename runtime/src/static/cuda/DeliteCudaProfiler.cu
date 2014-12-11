#include "DeliteCudaProfiler.h"

std::map<std::string,std::vector<cudatimer_t>*> **timermaps = new std::map<std::string,std::vector<cudatimer_t>*>*[DELITE_NUM_CUDA];

int64_t microseconds(struct timeval t) {
  return t.tv_sec * 1000000L + t.tv_usec;
}

int64_t nanoseconds(struct timeval t) {
  return microseconds(t) * 1000;
}

void InitDeliteCudaTimer(int32_t tid) {
  timermaps[tid] = new std::map<std::string,std::vector<cudatimer_t>*>();
}

void DeliteCudaTimerStart(int32_t tid, std::string name) {
  struct timeval start;

  std::map<std::string,std::vector<cudatimer_t>*> *timermap = timermaps[tid];
  std::map<std::string,std::vector<cudatimer_t>*>::iterator it = timermap->find(name);

  cudaDeviceSynchronize();
  gettimeofday(&start,NULL);
  cudatimer_t tpair = {start,-1};

  if (it == timermap->end()) {
    std::vector<cudatimer_t> *v = new std::vector<cudatimer_t>();
    v->push_back(tpair);
    timermap->insert(std::pair<std::string,std::vector<cudatimer_t>*>(name,v));
  }
  else {
    it->second->push_back(tpair);
  }
}

void DeliteCudaTimerStop(int32_t tid, std::string name) {
  struct timeval stop;
  cudaDeviceSynchronize();
  gettimeofday(&stop,NULL);

  std::map<std::string,std::vector<cudatimer_t>*> *timermap = timermaps[tid];
  timermap->find(name)->second->back().end = stop;
}

void DeliteCudaTimerDump(int32_t tid, int32_t rid, JNIEnv* env) {
  std::map<std::string,std::vector<cudatimer_t>*> *timermap = timermaps[tid];
  std::map<std::string,std::vector<cudatimer_t>*>::iterator it;

  jclass cls = env->FindClass("ppl/delite/runtime/profiler/PerformanceTimer");
  jmethodID mid = env->GetStaticMethodID(cls,"addTiming","(Ljava/lang/String;IJJ)V");

  for (it = timermap->begin(); it != timermap->end(); ++it) {
    std::vector<cudatimer_t> *v = it->second;
    std::vector<cudatimer_t>::iterator vit;
    for(vit = v->begin(); vit != v->end(); vit++) {
      jstring component = env->NewStringUTF(it->first.c_str());
      jlong startTime = microseconds(vit->start);
      jlong endTime = microseconds(vit->end);
      env->CallStaticVoidMethod(cls,mid,component,rid,startTime,endTime); 
      env->DeleteLocalRef(component);
    }
  }
}
