
#ifndef PCM_HELPER_H
#define PCM_HELPER_H

#include "cpucounters.h"

struct PCMStats {
	double l2CacheHitRatio;
	double l3CacheHitRatio;
	unsigned int l2Misses;
	unsigned int l3Misses;

	PCMStats() :
		l2CacheHitRatio(0.0),
		l3CacheHitRatio(0.0),
		l2Misses(0),
		l3Misses(0)
	{ }
};

PCM* pcmInit();

PCMStats* getPCMStats(CoreCounterState& before, CoreCounterState& after);

void printPCMStats(PCMStats* stats);

void pcmCleanup();

#endif
