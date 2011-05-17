#ifndef __VECTOR_H
#define __VECTOR_H
#ifdef USE_SIMD
#include<emmintrin.h>
#include<pmmintrin.h>
#endif

#include <stdarg.h>
#include <iostream>
#include <string>
#include <sstream>
#include <cmath>

#include "Liszt/static_assert.h"

#ifdef GPURUNTIME
#define GPU_DECLS __device__ __host__
#else
#define GPU_DECLS
#endif

#ifdef GPURUNTIME
#define INLINE_OPTIONS __device__ __host__ inline __attribute__((always_inline))
#else
#define INLINE_OPTIONS inline __attribute__((always_inline))
#endif

template<size_t N, typename scalar = double> class vec {
public:
  enum { VectorDim = N };
  typedef scalar ScalarType;

        GPU_DECLS vec();
        GPU_DECLS vec(const vec<N,scalar> & rhs);
        GPU_DECLS vec(const scalar * arr);
        GPU_DECLS vec(const scalar & arr);

        //easy initialization functions for the first 5 elements
        vec(const scalar & x, const scalar & y);
        vec(const scalar & x, const scalar & y, const scalar & z);
        vec(const scalar & x, const scalar & y, const scalar & z, const scalar & w);
        vec(const scalar & x, const scalar & y, const scalar & z, const scalar & w, const scalar & q);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15, const scalar & a16);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15, const scalar & a16, const scalar & a17);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15, const scalar & a16, const scalar & a17, const scalar & a18);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15, const scalar & a16, const scalar & a17, const scalar & a18, const scalar & a19);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15, const scalar & a16, const scalar & a17, const scalar & a18, const scalar & a19, const scalar & a20);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15, const scalar & a16, const scalar & a17, const scalar & a18, const scalar & a19, const scalar & a20, const scalar & a21);
        vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15, const scalar & a16, const scalar & a17, const scalar & a18, const scalar & a19, const scalar & a20, const scalar & a21, const scalar & a22);

        GPU_DECLS scalar & x();
        GPU_DECLS scalar & y();
        GPU_DECLS scalar & z();
        GPU_DECLS scalar & w();
        GPU_DECLS scalar & q();
        GPU_DECLS const scalar & x() const;
        GPU_DECLS const scalar & y() const;
        GPU_DECLS const scalar & z() const;
        GPU_DECLS const scalar & w() const;
        GPU_DECLS const scalar & q() const;

        GPU_DECLS scalar & operator[](size_t);
        GPU_DECLS const scalar operator[](size_t) const;
        GPU_DECLS const scalar dot(const vec<N,scalar> &) const;
        GPU_DECLS vec<N,scalar> cross(const vec<N,scalar> &) const;
        GPU_DECLS vec<N,scalar> abs() const;
        GPU_DECLS void min(const vec<N, scalar> &);
        GPU_DECLS void max(const vec<N, scalar> &);
        GPU_DECLS void normalize();
        GPU_DECLS scalar length() const;

        GPU_DECLS void operator+=(const vec<N,scalar> & rhs);
        GPU_DECLS void operator+=(const scalar * rhs);
        GPU_DECLS void operator-=(const vec<N,scalar> & rhs);
        GPU_DECLS void operator*=(const scalar & rhs);
        GPU_DECLS void operator*=(const vec<N,scalar> & rhs) ;
        GPU_DECLS void operator/=(const scalar & rhs);
        GPU_DECLS void operator/=(const vec<N,scalar> & rhs) ;
        template<typename rhs_scalar> GPU_DECLS void operator=(const vec<N,rhs_scalar> & rhs);
        GPU_DECLS vec<N,scalar> operator-() const;
        GPU_DECLS size_t dim() const { return N; }
        template<typename scalar2> GPU_DECLS
        operator vec<N,scalar2>() const;

public:
        template<size_t N2, typename scalar2> friend class vec;
        scalar v[N];
};

template<size_t N,typename scalar> GPU_DECLS vec<N,scalar> operator+(const vec<N,scalar> & lhs, const vec<N,scalar> & rhs);
template<size_t N,typename scalar> GPU_DECLS vec<N,scalar> operator-(const vec<N,scalar> & lhs, const vec<N,scalar> & rhs);

template<size_t N,typename scalar> GPU_DECLS vec<N,scalar> operator*(const vec<N,scalar> & lhs, const vec<N,scalar> & rhs);
template<size_t N,typename scalar> GPU_DECLS vec<N,scalar> operator*(const scalar & lhs, const vec<N,scalar> & rhs);
template<size_t N,typename scalar> GPU_DECLS vec<N,scalar> operator*(const vec<N,scalar> & rhs,const scalar & lhs);

template<size_t N,typename scalar> GPU_DECLS vec<N,scalar> operator/(const vec<N,scalar> & lhs, const vec<N,scalar> & rhs);
template<size_t N,typename scalar> GPU_DECLS vec<N,scalar> operator/(const scalar & lhs, const vec<N,scalar> & rhs);
template<size_t N,typename scalar> GPU_DECLS vec<N,scalar> operator/(const vec<N,scalar> & rhs,const scalar & lhs);

template<size_t N,typename scalar> std::ostream & operator<<(std::ostream& os, const vec<N,scalar> & obj);

template<size_t N,typename scalar> GPU_DECLS bool operator==(const vec<N,scalar> & lhs, const vec<N,scalar> & rhs);
template<size_t N,typename scalar> GPU_DECLS bool operator!=(const vec<N,scalar> & lhs, const vec<N,scalar> & rhs);

template<size_t N1,size_t N2,typename scalar> GPU_DECLS vec<N1 + N2,scalar> operator&(const vec<N1,scalar> & lhs, const vec<N2,scalar> & rhs);


//End decls begin definitions

//For General N,scalar:
template<size_t N, typename scalar> INLINE_OPTIONS vec<N,scalar>::vec() {}
template<size_t N, typename scalar> INLINE_OPTIONS vec<N,scalar>::vec(const vec<N,scalar> & rhs) { for(size_t i = 0; i < N; i++) v[i] = rhs.v[i]; }
template<size_t N, typename scalar> INLINE_OPTIONS vec<N,scalar>::vec(const scalar * rhs) { for(size_t i = 0; i < N; i++) v[i] = rhs[i]; }
template<size_t N, typename scalar> INLINE_OPTIONS vec<N,scalar>::vec(const scalar & rhs) { for(size_t i = 0; i < N; i++) v[i] = rhs; }

template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a, const scalar & b) {
        static_assert(N >= 2);
        v[0] = a; v[1] = b;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a, const scalar & b, const scalar & c) {
        static_assert(N >= 3);
        v[0] = a; v[1] = b; v[2] = c;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a, const scalar & b, const scalar & c, const scalar & d) {
        static_assert(N >= 4);
        v[0] = a; v[1] = b; v[2] = c; v[3] = d;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a, const scalar & b, const scalar & c, const scalar & d, const scalar & e) {
        static_assert(N >= 5);
        v[0] = a; v[1] = b; v[2] = c; v[3] = d; v[4] = e;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6) {
        static_assert(N >= 6);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7) {
        static_assert(N >= 7);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8) {
        static_assert(N >= 8);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9) {
        static_assert(N >= 9);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8; v[8] = a9;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10) {
        static_assert(N >= 10);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8; v[8] = a9; v[9] = a10;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11) {
        static_assert(N >= 11);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8; v[8] = a9; v[9] = a10; v[10] = a11;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12) {
        static_assert(N >= 12);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8; v[8] = a9; v[9] = a10; v[10] = a11; v[11] = a12;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13) {
        static_assert(N >= 13);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8; v[8] = a9; v[9] = a10; v[10] = a11; v[11] = a12; v[12] = a13;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14) {
        static_assert(N >= 14);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8; v[8] = a9; v[9] = a10; v[10] = a11; v[11] = a12; v[12] = a13; v[13] = a14;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15) {
        static_assert(N >= 15);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8; v[8] = a9; v[9] = a10; v[10] = a11; v[11] = a12; v[12] = a13; v[13] = a14; v[14] = a15;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15, const scalar & a16) {
        static_assert(N >= 16);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8; v[8] = a9; v[9] = a10; v[10] = a11; v[11] = a12; v[12] = a13; v[13] = a14; v[14] = a15; v[15] = a16;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15, const scalar & a16, const scalar & a17) {
        static_assert(N >= 17);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8; v[8] = a9; v[9] = a10; v[10] = a11; v[11] = a12; v[12] = a13; v[13] = a14; v[14] = a15; v[15] = a16; v[16] = a17;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15, const scalar & a16, const scalar & a17, const scalar & a18) {
        static_assert(N >= 18);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8; v[8] = a9; v[9] = a10; v[10] = a11; v[11] = a12; v[12] = a13; v[13] = a14; v[14] = a15; v[15] = a16; v[16] = a17; v[17] = a18;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15, const scalar & a16, const scalar & a17, const scalar & a18, const scalar & a19) {
        static_assert(N >= 19);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8; v[8] = a9; v[9] = a10; v[10] = a11; v[11] = a12; v[12] = a13; v[13] = a14; v[14] = a15; v[15] = a16; v[16] = a17; v[17] = a18; v[18] = a19;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15, const scalar & a16, const scalar & a17, const scalar & a18, const scalar & a19, const scalar & a20) {
        static_assert(N >= 20);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8; v[8] = a9; v[9] = a10; v[10] = a11; v[11] = a12; v[12] = a13; v[13] = a14; v[14] = a15; v[15] = a16; v[16] = a17; v[17] = a18; v[18] = a19; v[19] = a20;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15, const scalar & a16, const scalar & a17, const scalar & a18, const scalar & a19, const scalar & a20, const scalar & a21) {
        static_assert(N >= 21);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8; v[8] = a9; v[9] = a10; v[10] = a11; v[11] = a12; v[12] = a13; v[13] = a14; v[14] = a15; v[15] = a16; v[16] = a17; v[17] = a18; v[18] = a19; v[19] = a20; v[20] = a21;
}
template<size_t N, typename scalar> inline vec<N,scalar>::vec(const scalar & a1, const scalar & a2, const scalar & a3, const scalar & a4, const scalar & a5, const scalar & a6, const scalar & a7, const scalar & a8, const scalar & a9, const scalar & a10, const scalar & a11, const scalar & a12, const scalar & a13, const scalar & a14, const scalar & a15, const scalar & a16, const scalar & a17, const scalar & a18, const scalar & a19, const scalar & a20, const scalar & a21, const scalar & a22) {
        static_assert(N >= 22);
        v[0] = a1; v[1] = a2; v[2] = a3; v[3] = a4; v[4] = a5; v[5] = a6; v[6] = a7; v[7] = a8; v[8] = a9; v[9] = a10; v[10] = a11; v[11] = a12; v[12] = a13; v[13] = a14; v[14] = a15; v[15] = a16; v[16] = a17; v[17] = a18; v[18] = a19; v[19] = a20; v[20] = a21; v[21] = a22;
}

template<size_t N, typename scalar> INLINE_OPTIONS scalar & vec<N,scalar>::operator[](size_t idx) { return v[idx]; }
template<size_t N, typename scalar> INLINE_OPTIONS const scalar vec<N,scalar>::operator[](size_t idx) const { return v[idx]; }

template<size_t N, typename scalar> INLINE_OPTIONS scalar & vec<N,scalar>::x() { static_assert(0 < N); return v[0]; }
template<size_t N, typename scalar> INLINE_OPTIONS scalar & vec<N,scalar>::y() { static_assert(1 < N); return v[1]; }
template<size_t N, typename scalar> INLINE_OPTIONS scalar & vec<N,scalar>::z() { static_assert(2 < N); return v[2]; }
template<size_t N, typename scalar> INLINE_OPTIONS scalar & vec<N,scalar>::w() { static_assert(3 < N); return v[3]; }

template<size_t N, typename scalar> INLINE_OPTIONS const scalar & vec<N,scalar>::x() const { static_assert(0 < N); return v[0]; }
template<size_t N, typename scalar> INLINE_OPTIONS const scalar & vec<N,scalar>::y() const { static_assert(1 < N); return v[1]; }
template<size_t N, typename scalar> INLINE_OPTIONS const scalar & vec<N,scalar>::z() const { static_assert(2 < N); return v[2]; }
template<size_t N, typename scalar> INLINE_OPTIONS const scalar & vec<N,scalar>::w() const { static_assert(3 < N); return v[3]; }

template<size_t N, typename scalar> INLINE_OPTIONS const scalar vec<N,scalar>::dot(const vec<N,scalar> & rhs) const {
        static_assert(N > 0);
        scalar acc = v[0] * rhs[0];
        for(size_t i = 1; i < N; i++)
                acc += v[i] * rhs[i];
        return acc;
}

template<size_t N, typename scalar> INLINE_OPTIONS vec<N,scalar> vec<N, scalar>::abs() const {
    vec<N,scalar> ret ;
    for (size_t i = 0; i < N; i++) {
        ret[i] = std::fabs(v[i]);
    }
    return ret ;
}

template<size_t N, typename scalar> INLINE_OPTIONS void vec<N, scalar>::min(const vec<N, scalar> & rhs) {
    for (size_t i = 0; i < N; i++) {
        v[i] = std::min(v[i], rhs[i]);
    }
}

template<size_t N, typename scalar> INLINE_OPTIONS void vec<N, scalar>::max(const vec<N, scalar> & rhs) {
    for (size_t i = 0; i < N; i++) {
        v[i] = std::max(v[i], rhs[i]);
    }
}

template<size_t N, typename scalar> INLINE_OPTIONS void vec<N, scalar>::normalize() {
    scalar mag = length();
    for (size_t i = 0; i < N; i++) {
        v[i] /= mag;
    }
}

template<size_t N, typename scalar> INLINE_OPTIONS scalar vec<N, scalar>::length() const {
    scalar length = 0;
    for (size_t i = 0; i < N; i++) {
        length += v[i] * v[i];
    }
    return sqrt(length);
}

template<size_t N, typename scalar> INLINE_OPTIONS vec<N,scalar> operator*(const vec<N,scalar> & lhs, const vec<N,scalar> & rhs) { vec<N,scalar> ret; for(size_t i = 0; i < N; i++) ret[i] = lhs[i] * rhs[i]; return ret; }
template<size_t N, typename scalar> INLINE_OPTIONS vec<N,scalar> operator*(const scalar & lhs, const vec<N,scalar> & rhs) { vec<N,scalar> ret; for(size_t i = 0; i < N; i++) ret[i] = lhs * rhs[i]; return ret; }
template<size_t N, typename scalar> INLINE_OPTIONS vec<N,scalar> operator*(const vec<N,scalar> & lhs,const scalar & rhs) { vec<N,scalar> ret; for(size_t i = 0; i < N; i++) ret[i] = lhs[i] * rhs; return ret; }

template<size_t N, typename scalar> INLINE_OPTIONS vec<N,scalar> operator/(const vec<N,scalar> & lhs, const vec<N,scalar> & rhs) { vec<N,scalar> ret; for(size_t i = 0; i < N; i++) ret[i] = lhs[i] / rhs[i]; return ret; }
template<size_t N, typename scalar> INLINE_OPTIONS vec<N,scalar> operator/(const scalar & lhs, const vec<N,scalar> & rhs) { vec<N,scalar> ret; for(size_t i = 0; i < N; i++) ret[i] = lhs / rhs[i]; return ret; }
template<size_t N, typename scalar> INLINE_OPTIONS vec<N,scalar> operator/(const vec<N,scalar> & lhs,const scalar & rhs) { vec<N,scalar> ret; for(size_t i = 0; i < N; i++) ret[i] = lhs[i] / rhs; return ret; }



template<size_t N, typename scalar> INLINE_OPTIONS vec<N,scalar> operator+(const vec<N,scalar> & lhs, const vec<N,scalar> & rhs) { vec<N,scalar> ret; for(size_t i = 0; i < N; i++) ret[i] = lhs[i] + rhs[i]; return ret; }
template<size_t N, typename scalar> INLINE_OPTIONS vec<N,scalar> operator-(const vec<N,scalar> & lhs, const vec<N,scalar> & rhs) { vec<N,scalar> ret; for(size_t i = 0; i < N; i++) ret[i] = lhs[i] - rhs[i]; return ret; }

template<size_t N1,size_t N2,typename scalar> GPU_DECLS vec<N1 + N2,scalar> operator&(const vec<N1,scalar> & lhs, const vec<N2,scalar> & rhs) {
        vec<N1 + N2, scalar> ret;
        for(int i = 0; i < N1; i++)
                ret[i] = lhs[i];
        for(int i = 0; i < N2; i++)
                ret[i + N1] = rhs[i];
        return ret;
}

template<size_t N,typename scalar> INLINE_OPTIONS std::ostream & operator<<(std::ostream& os, const vec<N,scalar> & obj) {
        os << "(";
        for(size_t i = 0; i < N - 1; i++) os << obj[i] << ",";
        return os << obj[N-1] << ")";
}

template<size_t N,typename scalar> INLINE_OPTIONS bool operator==(const vec<N,scalar> & lhs, const vec<N,scalar> & rhs) {
        for(size_t i = 0; i < N; i++)
                if(lhs[i] != rhs[i])
                        return false;
        return true;
}
template<size_t N,typename scalar> INLINE_OPTIONS bool operator!=(const vec<N,scalar> & lhs, const vec<N,scalar> & rhs) {
        return !(lhs == rhs);
}
template<size_t N, typename scalar> INLINE_OPTIONS bool operator<(const vec<N,scalar> & lhs, const vec<N,scalar> & rhs) {
        size_t i = N;
        while (i != 0) {
                i--;
                if (lhs[i] < rhs[i])
                        return true;
                else if (lhs[i] > rhs[i])
                        return false;
        }
        return false;
}
template<size_t N, typename scalar> INLINE_OPTIONS void vec<N,scalar>::operator+=(const vec<N,scalar> & rhs) { for(size_t i = 0; i < N; i++) v[i] += rhs[i]; }
template<size_t N, typename scalar> INLINE_OPTIONS void vec<N,scalar>::operator+=(const scalar * rhs) { for(size_t i = 0; i < N; i++) v[i] += rhs[i]; }

template<size_t N, typename scalar> INLINE_OPTIONS void vec<N,scalar>::operator-=(const vec<N,scalar> & rhs) { for(size_t i = 0; i < N; i++) v[i] -= rhs[i]; }
template<size_t N, typename scalar> INLINE_OPTIONS void vec<N,scalar>::operator*=(const vec<N,scalar> & rhs) { for(size_t i = 0; i < N; i++) v[i] *= rhs[i]; }
template<size_t N, typename scalar> INLINE_OPTIONS void vec<N,scalar>::operator*=(const scalar & rhs) { for(size_t i = 0; i < N; i++) v[i] *= rhs; }
template<size_t N, typename scalar> INLINE_OPTIONS void vec<N,scalar>::operator/=(const vec<N,scalar> & rhs) { for(size_t i = 0; i < N; i++) v[i] /= rhs[i]; }
template<size_t N, typename scalar> INLINE_OPTIONS void vec<N,scalar>::operator/=(const scalar & rhs) { for(size_t i = 0; i < N; i++) v[i] /= rhs; }
template<size_t N, typename scalar> INLINE_OPTIONS vec<N,scalar> vec<N,scalar>::operator-() const {vec<N,scalar> ret; for(size_t i = 0; i < N; i++) ret[i] = -v[i]; return ret; }
template<size_t N, typename scalar> template<typename rhs_scalar> INLINE_OPTIONS void vec<N,scalar>::operator=(const vec<N,rhs_scalar> & rhs) {
    for(size_t i = 0; i < N; i++)
        v[i] = rhs[i];
}
template<size_t N, typename scalar>
template<typename scalar2>
vec<N,scalar>::operator vec<N,scalar2>() const {
    vec<N,scalar2> ret;
    for(size_t i = 0; i < N; i++)
        ret.v[i] = v[i];
    return ret;
}

/*
template<size_t N,typename scalar> void vec<N,scalar>::read(const json_spirit::Value & val) {
        using namespace json_spirit;
        const Array & arr = val.get_array();
        assert(arr.size() == N);
        for(size_t i = 0; i < N; i++) {
                IO::read(v[i],arr[i]);
        }
}

template<size_t N, typename scalar> json_spirit::Value vec<N,scalar>::write() const{
        using namespace json_spirit;
        Array arr(N);
        for(size_t i = 0; i < N; i++) {
                arr[i] = IO::write(v[i]);
        }
        return arr;
}*/

#ifdef USE_SIMD
template<> class vec<3,double> {
    public:
        vec();
        vec(const vec<3,double> & rhs);
        vec(const double * arr);
        vec(const double & arr);

        double x();
        double y();
        double z();

        const double operator[](size_t) const;
        const double dot(const vec<3,double> &) const;
        vec<3,double> cross(const vec<3,double> &) const;

        void operator+=(const vec<3,double> & rhs);
        void operator+=(const double * rhs);
        void operator-=(const vec<3,double> & rhs);
        void operator*=(const double & rhs);
        void operator/=(const double & rhs);

        vec<3,double> operator-();

        template<size_t i> const double get() const;
public:
        __m128d a0;
        __m128d a1;
};

//For vec<3,double> e.g. double
INLINE_OPTIONS vec<3,double> vec<3,double>::cross(const vec<3,double> & rhs) const {
        vec<3,double> ret;

        __m128d v0v1 = _mm_shuffle_pd(a0,a0,_MM_SHUFFLE2(0,1));
        __m128d r1r0 = rhs.a0;

        __m128d r0r2 = _mm_shuffle_pd(rhs.a1,rhs.a0,_MM_SHUFFLE2(0,0));
        __m128d v2v0 = _mm_shuffle_pd(a0,a1,_MM_SHUFFLE2(0,0));

        __m128d r2r1 = _mm_shuffle_pd(rhs.a0,rhs.a1,_MM_SHUFFLE2(0,1));
        __m128d v1v2 = _mm_shuffle_pd(a1,a0,_MM_SHUFFLE2(1,0));

        __m128d t1 = _mm_mul_pd(v0v1,r1r0);
        __m128d t2 = _mm_mul_pd(r0r2,v2v0);
        __m128d t3 = _mm_mul_pd(r2r1,v1v2);

#ifdef SSE2
        __m128d tshuf1 = _mm_shuffle_pd(t3, t2, _MM_SHUFFLE2(0, 0));
        __m128d tshuf2 = _mm_shuffle_pd(t3, t2, _MM_SHUFFLE2(1, 1));
        __m128d tshuf3 = _mm_shuffle_pd(t1, t1, _MM_SHUFFLE2(0, 0));
        __m128d tshuf4 = _mm_shuffle_pd(t1, t1, _MM_SHUFFLE2(1, 1));

        ret.a0 = _mm_sub_pd(tshuf1, tshuf2);
        ret.a1 = _mm_sub_pd(tshuf3, tshuf4);
#else
        ret.a0 = _mm_hsub_pd(t3,t2);
        ret.a1 = _mm_hsub_pd(t1,t1);
#endif
        return ret;
}

INLINE_OPTIONS vec<3,double>::vec() {}
INLINE_OPTIONS vec<3,double>::vec(const vec<3,double> & rhs) { a0 = rhs.a0; a1 = rhs.a1; }
INLINE_OPTIONS vec<3,double>::vec(const double * rhs) {
    a0 = _mm_set_pd(rhs[0],rhs[1]);
    a1 = _mm_set_pd(rhs[2],rhs[2]);
}
INLINE_OPTIONS vec<3,double>::vec(const double & rhs) {
    a0 = _mm_set_pd(rhs,rhs);
    a1 = _mm_set_pd(rhs,rhs);
}

template<> INLINE_OPTIONS const double vec<3,double>::get<0>() const { return __builtin_ia32_vec_ext_v2df(a0,1); }
template<> INLINE_OPTIONS const double vec<3,double>::get<1>() const { return __builtin_ia32_vec_ext_v2df(a0,0); }
template<> INLINE_OPTIONS const double vec<3,double>::get<2>() const { return __builtin_ia32_vec_ext_v2df(a1,0); }

INLINE_OPTIONS const double vec<3,double>::operator[](size_t idx) const {
    switch(idx & 3) {
        case 0:
            return get<0>();
        case 1:
            return get<1>();
        case 2:
            return get<2>();
    }
    assert(false);
}

INLINE_OPTIONS double vec<3,double>::x() { return get<0>(); }
INLINE_OPTIONS double vec<3,double>::y() { return get<1>(); }
INLINE_OPTIONS double vec<3,double>::z() { return get<2>(); }


INLINE_OPTIONS const double vec<3,double>::dot(const vec<3,double> & rhs) const {
    /*/
    const int mask =  0x31; //11111
    __m128d t = _mm_dp_pd(a0,rhs.a0,mask);
    __m128d t2 = _mm_mul_pd(a1,rhs.a1);
    __m128d t3 = _mm_add_pd(t,t2);
    return __builtin_ia32_vec_ext_v2df(t3,0); */
    __m128d t = _mm_mul_pd(a0,rhs.a0);
    __m128d t2 = _mm_mul_pd(a1,rhs.a1);
#ifdef SSE2
    t2 = _mm_add_pd(t,t2);
    t = _mm_shuffle_pd(t, t, _MM_SHUFFLE2(0, 1));
#else
    t = _mm_hadd_pd(t,t);
#endif
    t = _mm_add_pd(t,t2);
    return __builtin_ia32_vec_ext_v2df(t,0);
}

INLINE_OPTIONS vec<3,double> operator*(const double & lhs, const vec<3,double> & rhs) {
    vec<3,double> ret;
    __m128d lhsd = _mm_set_pd(lhs,lhs);
    ret.a0 = _mm_mul_pd(lhsd,rhs.a0);
    ret.a1 = _mm_mul_pd(lhsd,rhs.a1);
    return ret;
 }
INLINE_OPTIONS vec<3,double> operator*(const vec<3,double> & lhs,const double & rhs) {
    vec<3,double> ret;
    __m128d rhsd = _mm_set_pd(rhs,rhs);
    ret.a0 = _mm_mul_pd(lhs.a0,rhsd);
    ret.a1 = _mm_mul_pd(lhs.a1,rhsd);
    return ret;
 }

INLINE_OPTIONS vec<3,double> operator/(const double & lhs, const vec<3,double> & rhs) {
    vec<3,double> ret;
    __m128d lhsd = _mm_set_pd(lhs,lhs);
    ret.a0 = _mm_div_pd(lhsd,rhs.a0);
    ret.a1 = _mm_div_pd(lhsd,rhs.a1);
    return ret;
 }

INLINE_OPTIONS vec<3,double> operator/(const vec<3,double> & lhs,const double & rhs) {
    vec<3,double> ret;
    __m128d rhsd = _mm_set_pd(rhs,rhs);
    ret.a0 = _mm_div_pd(lhs.a0,rhsd);
    ret.a1 = _mm_div_pd(lhs.a1,rhsd);
    return ret;
 }
INLINE_OPTIONS vec<3,double> operator+(const vec<3,double> & lhs, const vec<3,double> & rhs) {
    vec<3,double> ret;
    ret.a0 = _mm_add_pd(lhs.a0,rhs.a0);
    ret.a1 = _mm_add_pd(lhs.a1,rhs.a1);
    return ret;
 }
INLINE_OPTIONS vec<3,double> operator-(const vec<3,double> & lhs, const vec<3,double> & rhs) {
    vec<3,double> ret;
    ret.a0 = _mm_sub_pd(lhs.a0,rhs.a0);
    ret.a1 = _mm_sub_pd(lhs.a1,rhs.a1);
    return ret;
 }
INLINE_OPTIONS std::ostream & operator<<(std::ostream& os, const vec<3,double> & m) {
    return os << "(" << m.get<0>() << "," << m.get<1>() << "," << m.get<2>() << ")";
}

INLINE_OPTIONS vec<3,double> vec<3,double>::operator-() {
    vec<3,double> ret;
    __m128d zero = _mm_setzero_pd();
    ret.a0 = _mm_sub_pd(zero,a0);
    ret.a1 = _mm_sub_pd(zero,a1);
    return ret;
}

INLINE_OPTIONS void vec<3,double>::operator+=(const vec<3,double> & rhs) {
    a0 = _mm_add_pd(a0,rhs.a0);
    a1 = _mm_add_pd(a1,rhs.a1);
}
INLINE_OPTIONS void vec<3,double>::operator+=(const double * rhs) {
    vec<3,double> rhsv(rhs);
    a0 = _mm_add_pd(a0,rhsv.a0);
    a1 = _mm_add_pd(a1,rhsv.a1);
}

INLINE_OPTIONS void vec<3,double>::operator-=(const vec<3,double> & rhs){
    a0 = _mm_sub_pd(a0,rhs.a0);
    a1 = _mm_sub_pd(a1,rhs.a1);
}
INLINE_OPTIONS void vec<3,double>::operator*=(const double & rhs) {
    __m128d rhsd = _mm_set_pd(rhs,rhs);
    a0 = _mm_mul_pd(a0,rhsd);
    a1 = _mm_mul_pd(a1,rhsd);
}
INLINE_OPTIONS void vec<3,double>::operator/=(const double & rhs) {
    __m128d rhsd = _mm_set_pd(rhs,rhs);
    a0 = _mm_div_pd(a0,rhsd);
    a1 = _mm_div_pd(a1,rhsd);
}
#else

#define CROSS(type) \
template<> INLINE_OPTIONS vec<3,type> vec<3,type>::cross(const vec<3,type> & rhs) const { \
        vec<3,type> ret; \
        ret[0] = v[1]*rhs[2] - v[2]*rhs[1]; \
        ret[1] = v[2]*rhs[0] - v[0]*rhs[2]; \
        ret[2] = v[0]*rhs[1] - v[1]*rhs[0]; \
        return ret; \
}
CROSS(double)
CROSS(float)
#undef CROSS
#endif //USE_SIMD

#ifndef GPURUNTIME
typedef vec<2,float> float2;
typedef vec<3,float> float3;
typedef vec<4,float> float4;

typedef vec<2,double> double2;
typedef vec<3,double> double3;
typedef vec<4,double> double4;
#endif

#undef GPU_DECLS
#endif //header
