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

// switch for printing debugging message
//#define DFS_DEBUG(...) fprintf(stderr, "[DEBUG-DeliteFS] "); fprintf(stderr, __VA_ARGS__)
#define DFS_DEBUG(...)

//TODO: check if need to compile with _FILE_OFFSET_BITS == 64?
class cppFileStream {
  private:
    vector<char*> files;
    vector<size_t> fileLengths;
    size_t size;

    FILE *reader;
    char *text;

    void findFileOffset(size_t start, size_t &fileIdx, size_t &offset) {
      offset = start;
      fileIdx = 0;
      while (offset >= fileLengths.at(fileIdx)) {
        offset -= fileLengths.at(fileIdx);
        fileIdx += 1;
      }
    }

  public:
    size_t idx;
    size_t position;

    cppFileStream* openCopyAtNewLine(size_t start) {
      cppFileStream* copy = new cppFileStream(size, &files, &fileLengths);
      copy->openAtNewLine(start);
      return copy;
    }

    void openAtNewLine(size_t start) { 
      findFileOffset(start, idx, position);
      reader = fopen(files.at(idx),"r");
      text = (char *)malloc(MAX_BUFSIZE*sizeof(char));

      if (reader == NULL) {
        printf("error reading file (%s)\n", strerror(errno));
        assert(false);
      }

      if (position != 0) {
        // jump to the offset
        if(lseek(fileno(reader), position-1, SEEK_SET) == -1) {
          assert(false && "lseek call failed");
        }
        // find the next newline after offset
        if (fgets(text, MAX_BUFSIZE, reader) == NULL) {
          assert(false && "first fgets failed");
        }
        position += strlen(text) - 1;
      }
    }

    string readLine() {
      char *line = text;
      while (fgets(line, MAX_BUFSIZE, reader) == NULL) {
        // read the next file
        idx += 1;
        if (idx >= files.size()) 
          return "";
        else 
          fclose(reader);
        reader = fopen(files.at(idx), "r");
        if (reader == NULL) {
          printf("error reading file (%s)\n", strerror(errno));
          assert(false);
        }
      }
      position += strlen(line);
      string str(line);
      str.erase(std::remove(str.begin(), str.end(), '\n'), str.end());
      str.erase(std::remove(str.begin(), str.end(), '\r'), str.end());
      return str;
    }

    cppFileStream(size_t size, vector<char*> *_files, vector<size_t> *_fileLengths) {
      size = size;
      //could share the files but then the free logic becomes confusing
      for (int i = 0; i < _files->size(); i++) {
        char *pathname = _files->at(i);
        char *p = (char *)malloc(strlen(pathname)+1);
        strcpy(p, pathname);
        files.push_back(p);
        fileLengths.push_back(_fileLengths->at(i));
      }
    }

    cppFileStream(size_t num, ...) {
      size = 0;
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
          // NOTE: explicit copy is required since the pathname (char*) given to constructor may be freed outside.
          char *p = (char *)malloc(strlen(pathname)+1);
          strcpy(p, pathname);
          files.push_back(p);
          fileLengths.push_back(st.st_size);
          size += st.st_size;
        }
      }
      va_end(arguments);

      DFS_DEBUG("total size of file is %ld\n", size);
    }

    void close() { 
      fclose(reader);
    }

    ~cppFileStream() {
      free(reader);
      free(text);
      for(vector<char*>::iterator it = files.begin(); it != files.end(); ++it) {
        free(*it);
      }
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
