#ifndef _INDEXVECTORIMPL_H_
#define _INDEXVECTORIMPL_H_

#include <stdio.h>
#include <CL/cl.h>

class IndexVector {
public:
    cl_mem data;
    int length;
    bool isRow;

    // Constructors
    IndexVector() {
        length = 0;
        isRow = true;
    }

    IndexVector(int _length, bool _isRow, cl_mem _data) {
        length = _length;
        isRow = _isRow;
    }

    // Accessor Functions
    /*
    int apply(int idx) {
        return data[idx];
    }

    void update(int idx, int newVal) {
        data[idx] = newVal;
    }
    */

    // DeliteCoolection
    int dcSize() {
        return length;
    }

/*
    int dcApply(int idx) {
        return data[idx];
    }

    void dcUpdate(int idx, int value) {
        data[idx] = value;
    }
*/
};

#endif
