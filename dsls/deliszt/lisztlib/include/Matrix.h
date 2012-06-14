/*
 * Matrix.h
 *
 *  Created on: Apr 22, 2009
 *      Author: zdevito
 */

#ifndef MATRIX_H_
#define MATRIX_H_
#include <assert.h>

#include <stdarg.h>
#include <iostream>
#include <string>
#include <sstream>
#include <cmath>
#include "Vector.h"

#ifdef GPURUNTIME
#define GPU_DECLS __device__ __host__
#define GPU_ASSERT(x)
#else
#define GPU_ASSERT(x) assert(x)
#define GPU_DECLS 
#endif

template<size_t R, size_t C, typename scalar = double>
class matrix {
public:
    GPU_DECLS matrix() {}
    GPU_DECLS matrix(const matrix<R,C,scalar> & rhs) {
        for(size_t i = 0; i < R; i++)
            for(size_t j = 0; j < C; j++)
                v[i][j] = rhs.v[i][j];
    }
    GPU_DECLS matrix(const scalar * arr) {
        for(size_t i = 0; i < R; i++)
            for(size_t j = 0; j < C; j++)
                v[i][j] = *arr++;
    }
    GPU_DECLS matrix(const scalar & arr) {
        for(size_t i = 0; i < R; i++)
            for(size_t j = 0; j < C; j++)
                v[i][j] = arr;
    }

    GPU_DECLS scalar & operator()(size_t a, size_t b) {
        GPU_ASSERT(a < R);
        GPU_ASSERT(b < C);
        return v[a][b];
    }
    GPU_DECLS const scalar & operator()(size_t a, size_t b) const {
        GPU_ASSERT(a < R);
        GPU_ASSERT(b < C);
        return v[a][b];
    }

    GPU_DECLS void operator+=(const matrix<R,C,scalar> & rhs) {
        for(size_t i = 0; i < R; i++)
            for(size_t j = 0; j < C; j++)
                v[i][j] += rhs.v[i][j];
    }

    GPU_DECLS void operator-=(const matrix<R,C,scalar> & rhs) {
        for(size_t i = 0; i < R; i++)
            for(size_t j = 0; j < C; j++)
                v[i][j] -= rhs.v[i][j];
    }

    GPU_DECLS void operator*=(const scalar & rhs) {
        for(size_t i = 0; i < R; i++)
            for(size_t j = 0; j < C; j++)
                v[i][j] *= rhs;
    }

    GPU_DECLS void operator/=(const scalar & rhs) {
        for(size_t i = 0; i < R; i++)
            for(size_t j = 0; j < C; j++)
                v[i][j] /= rhs;
    }

    GPU_DECLS void operator*=(const matrix<R,C,scalar> & rhs) {
        for(size_t i = 0; i < R; i++)
            for(size_t j = 0; j < C; j++)
                v[i][j] *= rhs.v[i][j];
    }
	

    template<typename rhs_scalar> GPU_DECLS void operator=(const matrix<R,C,rhs_scalar> & rhs) {
        for(size_t i = 0; i < R; i++)
            for(size_t j = 0; j < C; j++)
                v[i][j] = rhs.v[i][j];
    }
    
    GPU_DECLS matrix<R,C,scalar> operator-() const {
        matrix<R,C,scalar> m;
        for(size_t i = 0; i < R; i++)
            for(size_t j = 0; j < C; j++)
                m.v[i][j] = -v[i][j];
        return m;
    }

    GPU_DECLS matrix<C,R,scalar> transpose() const { 
    	matrix<C,R,scalar> m;
        for(size_t i = 0; i < R; i++)
            for(size_t j = 0; j < C; j++)
            	m.v[j][i] = v[i][j];
        return m;
    }
    

    matrix<C,R,scalar> eye() {
	matrix<C,R,scalar> m ;
	    for(size_t i = 0; i < R; i++) {
	        for(size_t j = 0; j < C; j++) {
	        	if ( j == i ) {
	        		m.v[i][j] = 1 ;
	        	} else {
	        		m.v[i][j] = 0;
	        	}   
	        } 
	    }  
	return m;        
    }


	void zeros() {
	    for(size_t i = 0; i < R; i++) {
	        for(size_t j = 0; j < C; j++) {
	        	v[i][j] = 0;  
	        } 
	    }    
	}
	GPU_DECLS size_t dim_row() const { return R; }
	GPU_DECLS size_t dim_col() const { return C; }
public:
    scalar v[R][C];
};

template<size_t R,size_t C,typename scalar>
GPU_DECLS inline matrix<R,C,scalar> operator*(const scalar & lhs, const matrix<R,C,scalar> & rhs) {
    matrix<R,C,scalar> ret;
    for(size_t i = 0; i < R; i++)
        for(size_t j = 0; j < C; j++)
          ret.v[i][j] = lhs * rhs.v[i][j];
    return ret;
}
template<size_t R,size_t C,typename scalar>
GPU_DECLS inline matrix<R,C,scalar> operator/(const scalar & lhs, const matrix<R,C,scalar> & rhs) {
    matrix<R,C,scalar> ret;
    for(size_t i = 0; i < R; i++)
        for(size_t j = 0; j < C; j++)
          ret.v[i][j] = lhs / rhs.v[i][j];
    return ret;
}

template<size_t R,size_t C,typename scalar>
GPU_DECLS inline matrix<R,C,scalar> operator*(const matrix<R,C,scalar> & rhs,const scalar & lhs) {
    matrix<R,C,scalar> ret;
    for(size_t i = 0; i < R; i++)
        for(size_t j = 0; j < C; j++)
          ret.v[i][j] = rhs.v[i][j] * lhs;
    return ret;
}


template<size_t R,size_t C,typename scalar>
GPU_DECLS inline vec<R,scalar> operator*(const matrix<R,C,scalar> & rhs,const vec<C,scalar> & lhs) {
    vec<R,scalar> ret;
    for(size_t i = 0; i < R; i++) {
        ret[i] = rhs.v[i][0] * lhs[0];
        for(size_t j = 1; j < C; j++)
            ret[i] += rhs.v[i][j] * lhs[j];
    }
    return ret;
}

template<size_t R,size_t C,typename scalar>
GPU_DECLS inline vec<C,scalar> operator*(const vec<R,scalar> & lhs,const matrix<R,C,scalar> & rhs) {
    vec<C,scalar> ret;
    for(size_t i = 0; i < C; i++) {
        ret[i] = rhs.v[0][i] * lhs[0];
        for(size_t j = 1; j < R; j++)
            ret[i] += rhs.v[j][i] * lhs[j];
    }
    return ret;
}

template<size_t A,size_t B,size_t C,typename scalar>
GPU_DECLS inline matrix<A,C,scalar> operator*(const matrix<A,B,scalar> lhs, const matrix<B,C,scalar> & rhs) {
    matrix<A,C,scalar> m;
    for(size_t i = 0; i < A; i++) {
        for(size_t j = 0; j < C; j++) {
            m.v[i][j] = 0;
            for(size_t k = 0; k < B; k++) {
                m.v[i][j] += lhs.v[i][k] * rhs.v[k][j];
            }
        }
    }
    return m;
}

template<size_t R,size_t C,typename scalar>
GPU_DECLS inline matrix<R,C,scalar> operator/(const matrix<R,C,scalar> & rhs,const scalar & lhs) {
    matrix<R,C,scalar> ret;
    for(size_t i = 0; i < R; i++)
        for(size_t j = 0; j < C; j++)
          ret.v[i][j] = rhs.v[i][j] / lhs;
    return ret;
}

template<size_t R,size_t C,typename scalar>
GPU_DECLS inline matrix<R,C,scalar> operator+(const matrix<R,C,scalar> & lhs,const matrix<R,C,scalar> & rhs) {
    matrix<R,C,scalar> ret;
    for(size_t i = 0; i < R; i++)
        for(size_t j = 0; j < C; j++)
          ret.v[i][j] = rhs.v[i][j] + lhs.v[i][j];
    return ret;
}

template<size_t R,size_t C,typename scalar>
GPU_DECLS inline matrix<R,C,scalar> operator-(const matrix<R,C,scalar> & lhs,const matrix<R,C,scalar> & rhs) {
    matrix<R,C,scalar> ret;
    for(size_t i = 0; i < R; i++)
        for(size_t j = 0; j < C; j++)
          ret.v[i][j] = lhs.v[i][j] - rhs.v[i][j];
    return ret;
}

template<size_t R,size_t C,typename scalar>
GPU_DECLS inline vec<R,scalar> diag(const matrix<R,C,scalar> & rhs) {
    GPU_ASSERT(R==C) ;
    vec<R,scalar> ret;
    for(size_t i = 0; i < R; i++) {
        ret[i] = rhs.v[i][i] ;
    }     
    return ret;
}

template<size_t N,typename scalar>
GPU_DECLS inline matrix<N, N,scalar> diag(const vec<N,scalar> & lhs) {
    matrix<N,N,scalar> ret ;
    for(size_t i = 0; i < N; i++) {
        for(size_t j = 0; j <= i; j++) {
        	if ( j == i ) {
        		ret.v[i][i] = lhs[i] ;
        	} else {
        		ret.v[i][j] = 0;
        		ret.v[j][i] = 0;
        	}   
        } 
    } 
    return ret ;    
}

template<size_t R,size_t C,typename scalar>
GPU_DECLS inline matrix<R, C,scalar> outer_prod(const vec<C,scalar> & v1, const vec<R,scalar> & v2) {
    matrix<R,C,scalar> ret ;
    for(size_t i = 0; i < R; i++) {
        for(size_t j = 0; j < C; j++) {
            ret.v[i][j] = v1[j]*v2[i] ;
        }
    }
    return ret ;
}

template<size_t R,size_t C, typename scalar> 
INLINE_OPTIONS bool operator==(const matrix<R,C,scalar> & lhs,const matrix<R,C,scalar> & rhs) {
	for(size_t i = 0; i < R; i++)
        for(size_t j = 0; j < C; j++)
			if(lhs.v[i][j] != rhs.v[i][j])
				return false;
	return true;
}

template<size_t R,size_t C, typename scalar> INLINE_OPTIONS bool operator!=(const matrix<R,C,scalar> & lhs,const matrix<R,C,scalar> & rhs) {
	return (!(lhs == rhs)) ;
}

template<size_t R,size_t C,typename scalar> INLINE_OPTIONS std::ostream & operator<<(std::ostream& os, const matrix<R,C,scalar> & obj) {
	os << "[";
	for(size_t i = 0; i < R; i++) {
            for (size_t j = 0; j < C; j++) {
                 os << obj.v[i][j] ;
                 if(j < C - 1) os << "," ;
            }
            if(i < R - 1) os << "|" ;
            else os << "]" ;  
        }
        return os ;
}

typedef matrix<3,3,double> double3x3;
typedef matrix<2,2,double> double2x2;
typedef matrix<4,4,double> double4x4;

typedef matrix<3,3,float> float3x3;
typedef matrix<2,2,float> float2x2;
typedef matrix<4,4,float> float4x4;

#undef GPU_DECLS
#undef GPU_ASSERT
#endif /* MATRIX_H_ */
