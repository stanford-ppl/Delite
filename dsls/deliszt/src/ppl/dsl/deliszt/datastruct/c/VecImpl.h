#ifndef _VECIMPL_H_
#define _VECIMPL_H_

#include <stdio.h>

template <class T>
class Vec {
public:
    T *data;
    int length;

    // Constructors
    Vec() {
        length = 0;
        data = NULL;
    }

    Vec(int _length, T *_data) {
        length = _length;
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
