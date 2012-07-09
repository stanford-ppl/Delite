#ifndef _DENSEVECTOR_H_
#define _DENSEVECTOR_H_

//#include <DeliteArray.h>
#include <stdlib.h>

template <class T>
class DenseVector {
public:
    T *data;
    int length;
    bool isRow;

    // Constructor
    //DenseVector() { }

    DenseVector(int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        data = (T *)malloc(length*sizeof(T));
    }

    DenseVector(T *_data, int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        data = (T *)_data;
    }
    // Reset the value to given value
    /*
    DenseVector(int _length, bool _isRow, T value) {
        length = _length;
        isRow = _isRow;
        data = malloc(length*sizeof(T));
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

    // unsafeSetData
    void unsafeSetData(DeliteArray<T> *da, int _length) {
        data = da->data;
        length = _length;
    }

    DeliteArray<T> getdata(void) {
      DeliteArray<T> da(length, data);
      return da;
    }

    void setdata(DeliteArray<T> da) {
      data = da.data;
    }
    */
};

#endif
