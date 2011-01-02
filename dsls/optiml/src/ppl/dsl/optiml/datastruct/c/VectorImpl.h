#ifndef _VECTORIMPL_H_
#define _VECTORIMPL_H_

#include <stdio.h>

template <class T>
class Vector {
public:
    T *data;
    int length;
    bool isRow;

    // Constructors
    Vector() {
        length = 0;
        isRow = true;
        data = NULL;
    }

    Vector(int _length, bool _isRow, T *_data) {
        length = _length;
        isRow = _isRow;
        data = _data;
    }

    // Accessor Functions
    T apply(int idx) {
        return data[idx];
    }

    void update(int idx, T newVal) {
        data[idx] = newVal;
    }

    // DeliteCoolection
    int size() {
        return length;
    }

    T dcApply(int idx) {
        return data[idx];
    }

    void dcUpdate(int idx, T value) {
        data[idx] = value;
    }
    
};

#endif
