#ifndef _OPENCLLIST_H_
#define _OPENCLLIST_H_

#include <stdio.h>
#include <CL/cl.h>

class OpenCLIntList {
public:
    cl_mem data;
    int length;

    // Constructors
    OpenCLIntList() {
        length = 0;
    }
    OpenCLIntList(int _length) {
        length = _length;
    }

    int dcSize() {
        return length;
    }
};

#endif
