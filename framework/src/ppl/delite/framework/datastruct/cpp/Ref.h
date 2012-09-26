#ifndef _DELITEREF_H_
#define _DELITEREF_H_

template <class T>
class Ref {
public:
    T data;

    Ref(T _data) {
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
