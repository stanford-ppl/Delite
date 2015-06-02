
#include "cpucounters.h"
#include "pcmHelper.h"
#include <iostream>

PCM* pcmInit() {
	std::cout << "Calling pcmInit()" << std::endl;
	PCM* m = PCM::getInstance();
  	m->disableJKTWorkaround();
  
  	switch( m->program() ) { 
      	case PCM::Success:
			std::cout << "PCM Initialized" << std::endl;
			break;
  
      	case PCM::PMUBusy:
         	std::cout << "PCM::PMU Busy!" << std::endl;
         	m->resetPMU();
         	return NULL;
  
      	default:
			return NULL;
  	}

	return m;
}

PCMStats* getPCMStats(CoreCounterState& before, CoreCounterState& after) {
	struct PCMStats* stats = new PCMStats();
	stats->l2CacheHitRatio = getL2CacheHitRatio( before, after );
	stats->l3CacheHitRatio = getL3CacheHitRatio( before, after );
	stats->l2Misses = getL2CacheMisses( before, after );
	stats->l3Misses = getL3CacheMisses( before, after );

	return stats;
}

void printPCMStats(PCMStats* stats) {
	std::cout
		 << "L2 Hit Ratio: " << stats->l2CacheHitRatio << std::endl
		 << "L2 Misses	 : " << stats->l2Misses << std::endl
		 << "L3 Hit Ratio: " << stats->l3CacheHitRatio << std::endl
		 << "L3 Misses	 : " << stats->l3Misses << std::endl;
}

void pcmCleanup() {
	PCM::getInstance()->cleanup();
}
