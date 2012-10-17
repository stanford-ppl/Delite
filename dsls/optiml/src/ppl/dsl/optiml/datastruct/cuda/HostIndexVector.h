#ifndef _HOST_INDEXVECTOR_H_
#define _HOST_INDEXVECTOR_H_

#include <HostDeliteArray.h>
#include <stdlib.h>

class HostIndexVectorDense {
public:
    HostDeliteArray<int> *da;
    int *data;
    int length;
    bool isRow;

    // Constructor
    HostIndexVectorDense(int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        da = new HostDeliteArray<int>(length);
        data = da->data;
    }

    HostIndexVectorDense(HostDeliteArray<int> *_da, int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        da = _da;
        data = _da->data;
    }

    HostIndexVectorDense(int *_data, int _length, bool _isRow) {
        length = _length;
        isRow = _isRow;
        da = new HostDeliteArray<int>(_data, _length);
        data = _data;
    }

    // Accessor Functions
    int apply(int idx) {
        return data[idx];
    }

    void update(int idx, int newVal) {
        data[idx] = newVal;
    }

    HostDeliteArray<int> *getData(void) {
      return da;
    }

    void setData(HostDeliteArray<int> *_da) {
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
