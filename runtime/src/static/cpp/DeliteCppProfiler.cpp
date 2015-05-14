#include "DeliteCppProfiler.h"

std::vector< std::map<std::string,std::vector<cpptimer_t>*>* > *timermaps;
std::vector< std::stack< cpptimer_t > > kernelCallStacks;
std::vector< FILE* > outputFiles;
std::vector< std::map<std::string, std::vector<PCMStats*>*>* > *memoryAccessMaps;
std::vector< std::vector<cpparray_layout_info>* > *arrayStartAddresses;

std::string profileFilePrefix("/home/jithinpt/cache_instrumentation/hyperdsl/published/SimpleVector/profile/profile_cpp_t_");

int64_t milliseconds(struct timeval t) {
  return (t.tv_sec * 1000000L + t.tv_usec) / 1000;
}

void InitDeliteCppTimer(int32_t numThreads) {
  memoryAccessMaps = new std::vector< std::map< std::string, std::vector<PCMStats*>* >* >;
  arrayStartAddresses = new std::vector< std::vector<cpparray_layout_info>* >;

  for (int32_t i = 0; i < numThreads; i++) {
    memoryAccessMaps->push_back(new std::map< std::string, std::vector<PCMStats*>* >());
	arrayStartAddresses->push_back(new std::vector<cpparray_layout_info>());	

    std::stack< cpptimer_t > s;
    kernelCallStacks.push_back(s);

    std::stringstream ss;
    ss << profileFilePrefix << i << ".csv";
	FILE* f = fopen(ss.str().c_str(), "w");
	outputFiles.push_back(f);
  }
}

void DeliteCppTimerStart(int32_t tid, string name, bool isKernel) {
  struct timeval start;
  gettimeofday(&start,NULL);

  cpptimer_t timer = {name.c_str(), start, -1, isKernel};
  kernelCallStacks[tid].push(timer);
}

void DeliteCppTimerStop(int32_t tid, string _name) {
  struct timeval t;
  gettimeofday(&t,NULL);

  cpptimer_t timer = kernelCallStacks[tid].top();

  int64_t start = milliseconds(timer.start);
  int64_t stop = milliseconds(t);
  int64_t duration = stop - start;
  fprintf(outputFiles[tid], "%s,%ld,%ld\n", timer.name, start, duration);

  kernelCallStacks[tid].pop();
}

void DeliteCppTimerClose() {
  std::vector< FILE* >::iterator it;
  for (it = outputFiles.begin(); it != outputFiles.end(); it++) {
	fclose(*it);
  }
}

uint64_t estimateSizeOfArray(unsigned long length, std::string elemType) {
  unsigned int elemSize = 0;
  if ( (elemType == "boolean") || (elemType == "byte") ) {
  	elemSize = 8;
  } else if ( (elemType == "char") || (elemType == "short") ) {
	elemSize = 16;
  } else if ( (elemType == "int") || (elemType == "float") ) {
	elemSize = 32;
  } else {
	elemSize = 64;
  }

  return length * elemSize;
}

void DeliteLogArrayAllocation(int32_t tid, void* startAddr, int32_t length, std::string elemType, std::string sourceContext) {
  cpparray_layout_info l = { startAddr, estimateSizeOfArray(length, elemType), sourceContext };
  arrayStartAddresses->at(tid)->push_back(l);
}

void printArrayAddressRanges() {
  std::ofstream outFile;
  outFile.open("dataStructures.csv", std::ofstream::out);

  std::vector< std::vector<cpparray_layout_info>* >::iterator it;
  int numThreads = arrayStartAddresses->size();
  for (int32_t tid = 0; tid < numThreads; tid++) {
	std::vector<cpparray_layout_info>* v = arrayStartAddresses->at(tid);
    std::vector<cpparray_layout_info>::iterator it;
    for (it = v->begin(); it != v->end(); it++) {
	  uint64_t s = (uint64_t) it->startAddr;
      outFile << "0x" << std::hex << s << "," << std::dec << it->size << "," << it->sourceContext << std::endl;
    }

	std::cout << std::endl;
  }

  outFile.close();
}

void DeliteUpdateMemoryAccessStats( int32_t tid, std::string sourceContext, PCMStats* stats ) {
    std::map< std::string, std::vector<PCMStats*>* >* scToMemoryAccessStats = memoryAccessMaps->at(tid);
    std::map< std::string, std::vector<PCMStats*>* >::iterator it = scToMemoryAccessStats->find( sourceContext );
    if (it == scToMemoryAccessStats->end()) {
        std::vector<PCMStats*>* v = new std::vector<PCMStats*>();
        v->push_back(stats);
        scToMemoryAccessStats->insert( std::pair< std::string, std::vector<PCMStats*>* >( sourceContext, v));
    } else {
        it->second->push_back( stats );
    }
}

void DeliteSendMemoryAccessStatsToJVM( int32_t offset, JNIEnv* env ) {
    #ifndef __DELITE_CPP_STANDALONE__

    jclass cls = env->FindClass("ppl/delite/runtime/profiler/MemoryProfiler");
    jmethodID mid = env->GetStaticMethodID(cls, "addMemoryAccessStats", "(Ljava/lang/String;IDIDID)V");

    if (mid == NULL) {
        std::cout << "Could not find method" << std::endl;
        return;
    }

    #endif 

    for (int32_t tid=0; tid < memoryAccessMaps->size(); tid++) {
        std::map< std::string, std::vector<PCMStats*>*> * scToMemoryAccessStats = memoryAccessMaps->at(tid);
        std::map< std::string, std::vector<PCMStats*>*>::iterator it;

        for (it = scToMemoryAccessStats->begin(); it != scToMemoryAccessStats->end(); it++) {
            std::string sc = it->first;
            std::vector<PCMStats*>* statsVector = it->second;
            PCMStats* stats = statsVector->at(0); // HACK: First question is: Do we need a vector

            #ifndef __DELITE_CPP_STANDALONE__

            jstring sourceContext = env->NewStringUTF(sc.c_str());
            jint l2Misses = stats->l2Misses;
            jint l3Misses = stats->l3Misses;
            jdouble l2CacheHitRatio = stats->l2CacheHitRatio;
            jdouble l3CacheHitRatio = stats->l3CacheHitRatio;
            jdouble bytesReadFromMC = stats->bytesReadFromMC;
            env->CallStaticVoidMethod(cls, mid, sourceContext, offset+tid, l2CacheHitRatio, l2Misses, l3CacheHitRatio, l3Misses, bytesReadFromMC);
            env->DeleteLocalRef(sourceContext);

            #endif
        }
    }

	printArrayAddressRanges();
}
