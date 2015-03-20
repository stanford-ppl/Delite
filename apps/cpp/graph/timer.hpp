#pragma once

#include <cstdio>
#include <time.h>
#include <sys/time.h>

class timer {
private:
  timeval start;
  timeval end;

public:
  void tic() {
    gettimeofday(&start,NULL);
  }

  double toc(const char* msg, bool dump = false) {
    gettimeofday(&end, NULL);
    uint64_t total_usec = (end.tv_sec * 1000000L + end.tv_usec) - (start.tv_sec * 1000000L + start.tv_usec);
    double total_sec = total_usec * 1e-6;
    if(msg != NULL) {
      fprintf(stderr, "timer: %f sec for %s\n", total_sec, msg);
    }

    if(dump) {
      const char* timer_path = getenv("TIMER_PATH");
      if(timer_path != NULL) {
        FILE* timer_file = fopen(timer_path, "a");
        if(timer_file != NULL) {
          fprintf(timer_file, "%f\n", total_sec);
          fclose(timer_file);
        }
        else {
          fprintf(stderr, "warning: unable to open timing file \"%s\"\n", timer_path);
        }
      }
    }

    return total_sec;
  }
};

class autotimer {
private:
  timer t;
  const char* msg;
  bool dump;

public:
  autotimer(): msg("autotimer"), dump(false) {
    t.tic();
  }

  autotimer(const char* m, bool d = false): msg(m), dump(d) {
    t.tic();
  }

  ~autotimer() {
    t.toc(msg, dump);
  }
};
