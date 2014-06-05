#include "DeliteCppProfiler.h"

#ifndef DELITE_CPP
#define DELITE_CPP 0
#endif

map<string,vector<cpptimer_t>*> **timermaps = new map<string,vector<cpptimer_t>*>*[DELITE_CPP];

int64_t microseconds(struct timeval t) {
  return t.tv_sec * 1000000L + t.tv_usec;
}

void InitDeliteCppTimer(int32_t tid) {
  timermaps[tid] = new map<string,vector<cpptimer_t>*>();
}

void DeliteCppTimerStart(int32_t tid, string name) {
  struct timeval start;

  map<string,vector<cpptimer_t>*> *timermap = timermaps[tid];  
  map<string,vector<cpptimer_t>*>::iterator it = timermap->find(name);

  gettimeofday(&start,NULL);
  cpptimer_t tpair = {start,-1};

  if (it == timermap->end()) {
    vector<cpptimer_t> *v = new vector<cpptimer_t>();
    v->push_back(tpair);
    timermap->insert(pair<string,vector<cpptimer_t>*>(name,v));
  }
  else {
    it->second->push_back(tpair);
  }
}

void DeliteCppTimerStop(int32_t tid, string name) {
  struct timeval stop;
  gettimeofday(&stop,NULL);

  map<string,vector<cpptimer_t>*> *timermap = timermaps[tid];  
  timermap->find(name)->second->back().end = stop;
}

void DeliteCppTimerDump(int32_t tid, int32_t rid, JNIEnv* env) {
  //cout << "dumping stats for thread " << tid << endl;
  map<string,vector<cpptimer_t>*> *timermap = timermaps[tid];  
  map<string,vector<cpptimer_t>*>::iterator it;

#ifndef __DELITE_CPP_STANDALONE__
  jclass cls = env->FindClass("ppl/delite/runtime/profiler/PerformanceTimer");
  jmethodID mid = env->GetStaticMethodID(cls,"addTiming","(Ljava/lang/String;IJJ)V");
#endif

  for (it = timermap->begin(); it != timermap->end(); ++it) {
    vector<cpptimer_t> *v = it->second;
    vector<cpptimer_t>::iterator vit;
    for(vit = v->begin(); vit != v->end(); vit++) {
#ifndef __DELITE_CPP_STANDALONE__
      jstring component = env->NewStringUTF(it->first.c_str());
      jlong startTime = microseconds(vit->start);
      jlong endTime = microseconds(vit->end);
      env->CallStaticVoidMethod(cls,mid,component,rid,startTime,endTime); 
      env->DeleteLocalRef(component);
#else
      std::cout << it->first << " " << microseconds(vit->start) << " -> " << microseconds(vit->end) << endl;
#endif
    }
  }
}
