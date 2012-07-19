#ifndef _INDEXVECTOR_H_
#define _INDEXVECTOR_H_

#include <DeliteArray.h>
#include <stdlib.h>

class IndexVectorDense {
public:
    DeliteArray<int> *da;
    int *data;
    int length;
    bool isRow;

    // Constructor
    IndexVectorDense(int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        da = new DeliteArray<int>(length);
        data = da->data;
    }

    IndexVectorDense(DeliteArray<int> *_da, int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        da = _da;
        data = _da->data;
    }

    IndexVectorDense(int *_data, int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        da = new DeliteArray<int>(_data, _length);
        data = _data;
    }

    // Accessor Functions
    int apply(int idx) {
        return data[idx];
    }

    void update(int idx, int newVal) {
        data[idx] = newVal;
    }

    DeliteArray<int> *getData(void) {
      return da;
    }

    void setData(DeliteArray<int> *_da) {
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
