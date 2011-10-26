#ifndef _RANGEVECTORIMPL_H_
#define _RANGEVECTORIMPL_H_

#include <stdio.h>
#include <CL/cl.h>

class RangeVector {
public:
    int start;
    int end;
    int stride;
    bool isRow;

    // Constructors
    RangeVector() {
        start = 0;
        end = 0;
        stride = 1;
        isRow = true;
    }

    RangeVector(int _start, int _end, int _stride, bool _isRow) {
        start = _start;
        end = _end;
        stride = _stride;
        isRow = _isRow;
    }

    // Accessor Functions
    /*
    int apply(int idx) {
        return start+stride*idx;
    }

    int length() {
        return (end - start + stride - 1);
	}
	*/

    // DeliteCoolection
    int dcSize() {
        return (end - start + stride - 1);
    }

/*
    int dcApply(int idx) {
        return start+stride*idx;
    }
*/
};

#endif
