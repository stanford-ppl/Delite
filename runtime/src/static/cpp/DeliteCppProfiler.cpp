#include "DeliteCppProfiler.h"

std::vector<std::map<std::string,std::vector<cpptimer_t>*>* > *timermaps;

int64_t nanoseconds(struct timeval t) {
  return 1000 * (t.tv_sec * 1000000L + t.tv_usec);
}

void InitDeliteCppTimer(int32_t numThreads) {
  timermaps = new std::vector<std::map<std::string,std::vector<cpptimer_t>*>* >;
  for (int32_t i=0; i<numThreads; i++) {
    timermaps->push_back(new std::map<std::string,std::vector<cpptimer_t>*>());
  }
}

void DeliteCppTimerStart(int32_t tid, string _name, bool isKernel) {
  struct timeval start;

  std::string name = std::string(_name.c_str());
  std::map<std::string,std::vector<cpptimer_t>*> *timermap = timermaps->at(tid);
  std::map<std::string,std::vector<cpptimer_t>*>::iterator it = timermap->find(name);

  gettimeofday(&start,NULL);
  cpptimer_t tpair = {start,-1, isKernel};

  if (it == timermap->end()) {
    std::vector<cpptimer_t> *v = new std::vector<cpptimer_t>();
    v->push_back(tpair);
    timermap->insert(std::pair<std::string,std::vector<cpptimer_t>*>(name,v));
  }
  else {
    it->second->push_back(tpair);
  }
}

void DeliteCppTimerStop(int32_t tid, string _name) {
  struct timeval stop;
  gettimeofday(&stop,NULL);

  std::string name = std::string(_name.c_str());
  for(int i=0; i<timermaps->size(); i++) {
    std::map<std::string,std::vector<cpptimer_t>*> *timermap = timermaps->at(i);
    if(timermap->find(name) != timermap->end()) {
      timermap->find(name)->second->back().end = stop;
      break;
    }
  }
}

#ifndef __DELITE_CPP_STANDALONE__
void DeliteCppTimerDump(int32_t offset, JNIEnv* env) {
  jclass cls = env->FindClass("ppl/delite/runtime/profiler/PerformanceTimer");
  jmethodID mid = env->GetStaticMethodID(cls,"addTiming","(Ljava/lang/String;IJJZ)V");
#else
void DeliteCppTimerDump() {
#endif

  for (int32_t tid=0; tid<timermaps->size(); tid++) {
    std::map<std::string,std::vector<cpptimer_t>*> *timermap = timermaps->at(tid);
    std::map<std::string,std::vector<cpptimer_t>*>::iterator it;
    for (it = timermap->begin(); it != timermap->end(); ++it) {
      std::vector<cpptimer_t> *v = it->second;
      std::vector<cpptimer_t>::iterator vit;
      for(vit = v->begin(); vit != v->end(); vit++) {
        #ifndef __DELITE_CPP_STANDALONE__
        jstring component = env->NewStringUTF(it->first.c_str());
        jlong startTime = nanoseconds(vit->start);
        jlong endTime = nanoseconds(vit->end);
        jboolean isKernel = vit->isKernel;
        env->CallStaticVoidMethod(cls,mid,component,offset+tid,startTime,endTime,isKernel);
        env->DeleteLocalRef(component);
        #else
        std::cout << it->first << " " << nanoseconds(vit->start) << " -> " << nanoseconds(vit->end) << std::endl;
        #endif
      }
    }
  }
}
