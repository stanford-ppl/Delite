#ifndef DELITE_CUDA_PROFILER_INCLUDED
#define DELITE_CUDA_PROFILER_INCLUDED

#include <sys/time.h>

struct timeval myprofiler_start, myprofiler_end;
void printTime(void) {
  printf("Time : %ld [us]\n", ((myprofiler_end.tv_sec * 1000000 + myprofiler_end.tv_usec) - (myprofiler_start.tv_sec * 1000000 + myprofiler_start.tv_usec)));
}
void printTime(char *str) {
  printf("%s : Time : %ld [us]\n", str, ((myprofiler_end.tv_sec * 1000000 + myprofiler_end.tv_usec) - (myprofiler_start.tv_sec * 1000000 + myprofiler_start.tv_usec)));
}
/*
void printTime(char *str) {
  sprintf(str, "%s : Time : %ld [us]\n", str, ((myprofiler_end.tv_sec * 1000000 + myprofiler_end.tv_usec) - (myprofiler_start.tv_sec * 1000000 + myprofiler_start.tv_usec)));
}
*/
void mytic(void) {
  cudaDeviceSynchronize();
  gettimeofday(&myprofiler_start,NULL);
}

void mytoc(void) {
  cudaDeviceSynchronize();
  gettimeofday(&myprofiler_end,NULL);
  printTime();
}

void mytoc(char *str) {
  cudaDeviceSynchronize();
  gettimeofday(&myprofiler_end,NULL);
  printTime(str);
}

#endif
