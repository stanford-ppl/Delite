#ifndef __CPPFILESTREAM_H__
#define __CPPFILESTREAM_H__

#include <cstdarg>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <vector>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <algorithm>
#include <string>
#include <errno.h>
#include <fcntl.h>

using namespace std;

// each line of file is limited to 1M characters
#define MAX_BUFSIZE 1048576

// switch for priting debugging message
//#define DFS_DEBUG(...) fprintf(stderr, "[DEBUG-DeliteFS] "); fprintf(stderr, __VA_ARGS__)
#define DFS_DEBUG(...)

//TODO: check if need to compile with _FILE_OFFSET_BITS == 64?
class cppFileStream {
  public:
    vector<char*> files;
    vector<int> filelengths;
    long size;
    int numThreads;

    // padding to avoid false sharing between threads
    static const int pad = 32;
    FILE **allReader;
    unsigned int *allIdx;
    long *allPos;
    long *allEnd;
    char **allText;

    void findFileOffset(long start, int &fileIdx, long &offset) {
      offset = start;
      fileIdx = 0;
      while (offset >= filelengths.at(fileIdx)) {
        offset -= filelengths.at(fileIdx);
        fileIdx += 1;
      }
    }

    void openAtNewLine(int threadIdx) { 
      long pos = threadIdx * size / numThreads;
      allEnd[pad*threadIdx] = (threadIdx + 1) * size / numThreads;
      int fileIdx; long offset;
      findFileOffset(pos, fileIdx, offset);
      FILE *fp = fopen(files.at(fileIdx),"r");
      allReader[pad*threadIdx] = fp;
      if (offset != 0) {
        // jump to the offset
        if(lseek(fileno(fp), offset, SEEK_SET) == -1) {
          assert(false && "lseek call failed");
        }
        // find the next newline after offset
        // TODO: check why scala uses length-a
        char *line = allText[pad*threadIdx];
        if (fgets(line, MAX_BUFSIZE, allReader[pad*threadIdx]) == NULL) {
          assert(false && "first fgets failed");
        }
        pos += strlen(line);
      }
      allPos[pad*threadIdx] = pos;
      allIdx[pad*threadIdx] = fileIdx;
      allReader[pad*threadIdx] = fp;
    }

    long pos(int idx) { return allPos[pad*idx]; }
    long end(int idx) { return allEnd[pad*idx]; }

    string readLine(int idx) {
      char *line = allText[pad*idx];
      if (fgets(line, MAX_BUFSIZE, allReader[pad*idx]) == NULL) {
        // read the next file
        allIdx[pad*idx] += 1;
        if (allIdx[pad*idx] >= files.size()) 
          return "";
        else 
          fclose(allReader[pad*idx]);
        allReader[pad*idx] = fopen(files.at(allIdx[pad*idx]), "r");
        if (fgets(line, MAX_BUFSIZE, allReader[pad*idx]) == NULL) 
          assert(false  && "fgets failed");
      }
      allPos[pad*idx] += strlen(line);
      string str(line);
      str.erase(std::remove(str.begin(), str.end(), '\n'), str.end());
      str.erase(std::remove(str.begin(), str.end(), '\r'), str.end());
      return str;
    }

    cppFileStream(int num, ...) {
      size = 0;
      //TODO: get the number of threads at runtime instead of preprocessor pragma?
      numThreads = DELITE_CPP;
      allReader = (FILE **)malloc(numThreads * pad * sizeof(FILE *));
      allIdx = (unsigned int *)malloc(numThreads * pad * sizeof(unsigned int));
      allPos = (long *)malloc(numThreads * pad * sizeof(long));
      allEnd = (long *)malloc(numThreads * pad * sizeof(long));
      allText = (char **)malloc(numThreads * pad * sizeof(char *));
      for(int i=0; i<numThreads; i++) {
        allText[i*pad] = (char *)malloc(MAX_BUFSIZE*sizeof(char));
      }

      va_list arguments;
      DFS_DEBUG("number of paths is %d\n", num);
      va_start(arguments, num);
      for(int i=0; i<num; i++) {
        char *pathname = va_arg(arguments, char *);
        DFS_DEBUG("pathname is %s\n", pathname);

        // check if file or directory
        struct stat st;
        lstat(pathname, &st);
        if(S_ISDIR(st.st_mode)) {
          assert(false && "Directory paths are not supported yet");
        }
        else {
          // append to the list of files
          files.push_back(pathname);
          filelengths.push_back(st.st_size);
          size += st.st_size;
        }
      }
      va_end(arguments);

      DFS_DEBUG("total size of file is %ld\n", size);
    }

    void close(int idx) { 
      fclose(allReader[pad*idx]);
      free(allText[pad*idx]);
    }
};

#endif

/* 
// main function for test
int main(void) {
  cppFileStream file(1, "hello.txt");
  file.openAtNewLine(0);
  file.openAtNewLine(1);

  string s;
  while((s = file.readLine(0)).length() != 0) {
    std::cout << "string is " << s << endl;
  }
  std::cout << "hline" << endl;
  while((s = file.readLine(1)).length() != 0) {
    std::cout << "string is " << s << endl;
  }

  return 0;
}
*/
