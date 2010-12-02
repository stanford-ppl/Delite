#ifndef _VECTORIMPL_H_
#define _VECTORIMPL_H_

#include <stdio.h>

template <class T>
class Vector {
private:
    T *data;

public:
    int length;
    bool is_row;

    // Constructors
    Vector() {
        length = 0;
        is_row = true;
        data = NULL;
    }

    Vector(int _length, bool _isRow, T *_data) {
        length = _length;
        is_row = _isRow;
        data = _data;
    }

    // Accessor Functions
    T apply(int idx) {
        return data[idx];
    }

    void update(int idx, T newVal) {
        data[idx] = newVal;
    }
};

#endif
