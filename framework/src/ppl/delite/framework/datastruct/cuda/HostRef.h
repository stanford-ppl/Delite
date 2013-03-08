#ifndef _HOST_DELITEREF_H_
#define _HOST_DELITEREF_H_

template <class T>
class HostRef {
public:
    T data;

    HostRef(void) {
      data = NULL;
    }

    HostRef(T _data) {
      data = _data;
    }

    T get(void) {
      return data;
    }

    void set(T newVal) {
        data = newVal;
    }
};

#endif
