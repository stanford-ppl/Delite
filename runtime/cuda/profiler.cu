#include <sys/time.h>

struct timeval myprofiler_start, myprofiler_end;

void printTime(void) {
	printf("Time : %ld\n", ((myprofiler_end.tv_sec * 1000000 + myprofiler_end.tv_usec) - (myprofiler_start.tv_sec * 1000000 + myprofiler_start.tv_usec)));
}

void mytic(void) {
	cudaDeviceSynchronize();
	gettimeofday(&myprofiler_start,NULL);
}

void mytoc(void) {
	cudaDeviceSynchronize();
gettimeofday(&myprofiler_end,NULL);
printTime();
}

