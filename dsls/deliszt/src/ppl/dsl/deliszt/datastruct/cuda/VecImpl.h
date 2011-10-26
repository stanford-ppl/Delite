#ifndef _VECIMPL_H_
#define _VECIMPL_H_

template <class T, int N>
class Vec {
public:
    T data[N];

    // Accessor Functions
    __host__ __device__ T apply(int idx) {
        return data[idx];
    }

    __host__ __device__ void update(int idx, T newVal) {
        data[idx] = newVal;
    }

    // DeliteCoolection
    __host__ __device__ int dcSize() {
        return N;
    }

    __host__ __device__ T dcApply(int idx) {
        return data[idx];
    }

    __host__ __device__ void dcUpdate(int idx, T value) {
        data[idx] = value;
    }
    
};


#endif
