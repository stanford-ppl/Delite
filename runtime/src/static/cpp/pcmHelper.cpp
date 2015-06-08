
#include "pcmHelper.h"
#include <iostream>

int lowestCppThreadId = 0;

void pcmInit(int _lowestCppTid) {
    lowestCppThreadId = _lowestCppTid;

    #ifdef DELITE_ENABLE_PCM
	std::cout << "Initializing PCM" << std::endl;
	PCM* m = PCM::getInstance();
	m->disableJKTWorkaround();
  
	switch( m->program() ) { 
		case PCM::Success:
			std::cout << "PCM Initialized" << std::endl;
			return;
  
		case PCM::PMUBusy:
			std::cout << "PCM::PMU Busy!" << std::endl;
			m->resetPMU();
			return;
  
		default:
			return;
	}
    #endif
}

#ifdef DELITE_ENABLE_PCM

PCMStats* getPCMStats(CoreCounterState& before, CoreCounterState& after) {
	struct PCMStats* stats = new PCMStats();
	stats->l2CacheHitRatio = getL2CacheHitRatio( before, after );
	stats->l3CacheHitRatio = getL3CacheHitRatio( before, after );
	stats->l2Misses = getL2CacheMisses( before, after );
	stats->l3Misses = getL3CacheMisses( before, after );

	return stats;
}

CoreCounterState getCoreCounterState(int32_t tid) {                                       
    PCM * inst = PCM::getInstance();
    CoreCounterState result;
    if (inst) result = inst->getCoreCounterState(lowestCppThreadId + tid); // HACK
    return result;
}

#endif

void printPCMStats(PCMStats* stats) {
	std::cout
		 << "L2 Hit Ratio: " << stats->l2CacheHitRatio << std::endl
		 << "L2 Misses	 : " << stats->l2Misses << std::endl
		 << "L3 Hit Ratio: " << stats->l3CacheHitRatio << std::endl
		 << "L3 Misses	 : " << stats->l3Misses << std::endl;
}

void pcmCleanup() {
    #ifdef DELITE_ENABLE_PCM
	PCM::getInstance()->cleanup();
    #endif
}
