#ifndef _FIELDIMPL_H_
#define _FIELDIMPL_H_

#include <stdio.h>

template <class MO, class T>
class Field {
public:
    T *data;
    int length;

    // Constructors
    Field() {
        length = 0;
        data = NULL;
    }

    Field(int _length, T *_data) {
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
    
    // Accessor Functions
    T apply(MO mo) {
        return data[mo.internalId()];
    }

    void update(MO mo, T newVal) {
        data[mo.internalId()] = newVal;
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
