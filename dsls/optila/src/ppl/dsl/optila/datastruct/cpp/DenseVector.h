#ifndef _DENSEVECTOR_H_
#define _DENSEVECTOR_H_

#include <DeliteArray.h>
#include <stdlib.h>

template <class T>
class DenseVector {
public:
    DeliteArray<T> *da;
    T *data;
    int length;
    bool isRow;

    // Constructor
    DenseVector(int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        da = new DeliteArray<T>(length);
        data = da->data;
    }

    DenseVector(DeliteArray<T> *_da, int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        da = _da;
        data = _da->data;
    }

    DenseVector(T *_data, int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        da = new DeliteArray<T>(_data, _length);
        data = _data;
    }

    // Accessor Functions
    T apply(int idx) {
        return data[idx];
    }

    void update(int idx, T newVal) {
        data[idx] = newVal;
    }

    DeliteArray<T> *getData(void) {
      return da;
    }

    void setData(DeliteArray<T> *_da) {
      da = _da;
      data = da->data;
    }

    /*
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
