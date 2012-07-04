#ifndef _DELITEARRAY_H_
#define _DELITEARRAY_H_

template <class T>
class DeliteArray {
public:
    T *data;
    int length;

    // Constructor
    DeliteArray(int _length) {
        length = _length;
        data = (T *)malloc(length*sizeof(T));
    }

    DeliteArray(int _length, T *_data) {
        length = _length;
        data = _data;
    }

    T apply(int idx) {
        return data[idx];
    }

    void update(int idx, T value) {
        data[idx] = value;
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
