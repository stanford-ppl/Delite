
#include <stdint.h>
#include "immintrin.h"
#include "DeliteCppSIMD.h"


float lpblas_dot8(const int8_t* x, const int8_t* y, long n) {
  const float MAX = 128;
  const float DIVIDEDBY = 1.0; /// MAX / MAX;
  const int n_remainder = n % 16;
  
  float rs[4];
  
  __m128i ymm0, ymm1, ymm2;
  
  __m128 ymm3;
  
  __m128i ymm_ones_16bit = _mm_set_epi16(1,1,1,1,1,1,1,1);
  
  __m128  ymm_aggregated_sum = _mm_set_ps(0.0,0.0,0.0,0.0);

  for(int i = n_remainder; i < n; i += 16){
    ymm0 = _mm_loadu_si128((__m128i const *)&x[i]);
    ymm1 = _mm_loadu_si128((__m128i const *)&y[i]);
    
    ymm1 = _mm_sign_epi8(ymm1, ymm0);
    ymm0 = _mm_abs_epi8(ymm0);

    ymm2 = _mm_maddubs_epi16(ymm0, ymm1);
    ymm2 = _mm_madd_epi16(ymm2, ymm_ones_16bit);
    ymm3 = _mm_cvtepi32_ps(ymm2);

    ymm_aggregated_sum = _mm_add_ps(ymm_aggregated_sum, ymm3);
  }

  _mm_storeu_ps(rs, ymm_aggregated_sum);
  float toreturn = DIVIDEDBY*(rs[0]+rs[1]+rs[2]+rs[3]);
  for(int i = 0; i < n_remainder; i++){
    toreturn += (float)x[i] * (float)y[i] * DIVIDEDBY;
  }
  
  return toreturn;
}


float lpblas_dot16(const int16_t* x, const int16_t* y, long n) {
  const float MAX = 32767;
  const float DIVIDEDBY = 1.0; // / MAX / MAX;
  const int n_remainder = n % 8;
  
  float rs[4];
  
  __m128i ymm0, ymm1, ymm2;

  __m128 ymm3;

  __m128 ymm_aggregated_sum = _mm_set_ps(0.0,0.0,0.0,0.0);

  for(int i = n_remainder; i < n; i+=8){
    ymm0 = _mm_loadu_si128((__m128i const *)&x[i]);
    ymm1 = _mm_loadu_si128((__m128i const *)&y[i]);
    
    ymm2 = _mm_madd_epi16(ymm0, ymm1);

    ymm3 = _mm_cvtepi32_ps(ymm2);

    ymm_aggregated_sum = _mm_add_ps(ymm_aggregated_sum, ymm3);
  }

  _mm_storeu_ps(rs, ymm_aggregated_sum);
  float toreturn = DIVIDEDBY*(rs[0]+rs[1]+rs[2]+rs[3]);
  for(int i = 0; i < n_remainder; i++){
    toreturn += x[i] * y[i] * DIVIDEDBY;
  }
  return toreturn;
}
