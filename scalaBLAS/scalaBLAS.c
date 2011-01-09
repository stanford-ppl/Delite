#include <stdlib.h>
#include <stdio.h>

#include "scalaBLAS__.h"
#include "mkl.h"

JNIEXPORT void JNICALL Java_scalaBLAS_00024_matMult_00024mDc_00024sp
(JNIEnv *env, jobject obj, jdoubleArray mat1, jdoubleArray mat2, jdoubleArray mat3, jint mat1_r, jint mat1_c, jint mat2_c)
{
	jboolean copy;
	jdouble *mat1_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy);
	jdouble *mat2_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat2, &copy);
	jdouble *mat3_ptr = (*env)->GetPrimitiveArrayCritical(env, (jarray)mat3, &copy);
	
	cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, mat1_r, mat2_c, mat1_c, 1.0, mat1_ptr, mat1_c, mat2_ptr, mat2_c, 0.0, mat3_ptr, mat2_c);

	(*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, mat2, mat2_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, mat3, mat3_ptr, 0);
}

JNIEXPORT void JNICALL Java_scalaBLAS_00024_matMult_00024mFc_00024sp
(JNIEnv *env, jobject obj, jfloatArray mat1, jfloatArray mat2, jfloatArray mat3, jint mat1_r, jint mat1_c, jint mat2_c)
{
	jboolean copy;
	
	jfloat *mat1_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy));
	jfloat *mat2_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat2, &copy));
	jfloat *mat3_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat3, &copy));

	cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, mat1_r, mat2_c, mat1_c, 1.0, mat1_ptr, mat1_c, mat2_ptr, mat2_c, 0.0, mat3_ptr, mat2_c);

	(*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, mat2, mat2_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, mat3, mat3_ptr, 0);
}


JNIEXPORT void JNICALL Java_scalaBLAS_00024_matVMult_00024mFc_00024sp
(JNIEnv *env, jobject obj, jfloatArray mat1, jfloatArray vec2, jfloatArray vec3, jint mat_row, jint mat_col, jint vec_offset, jint vec_stride)
{
	jboolean copy;
	
	jfloat *mat1_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy));
	jfloat *vec2_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));
	jfloat *vec3_ptr = (jfloat*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec3, &copy));

	vec2_ptr += vec_offset;

	cblas_sgemv(CblasRowMajor, CblasNoTrans, mat_row, mat_col, 1.0, mat1_ptr, mat_row, vec2_ptr, vec_stride, 0.0, vec3_ptr, 1); 

	(*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec3, vec3_ptr, 0);
}

JNIEXPORT void JNICALL Java_scalaBLAS_00024_matVMult_00024mDc_00024sp
(JNIEnv *env, jobject obj, jdoubleArray mat1, jdoubleArray vec2, jdoubleArray vec3, jint mat_row, jint mat_col, jint vec_offset, jint vec_stride)
{
	jboolean copy;
	
	jdouble *mat1_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)mat1, &copy));
	jdouble *vec2_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec2, &copy));
	jdouble *vec3_ptr = (jdouble*)((*env)->GetPrimitiveArrayCritical(env, (jarray)vec3, &copy));

	vec2_ptr += vec_offset;

	cblas_dgemv(CblasRowMajor, CblasNoTrans, mat_row, mat_col, 1.0, mat1_ptr, mat_col, vec2_ptr, vec_stride, 0.0, vec3_ptr, 1); 

	(*env)->ReleasePrimitiveArrayCritical(env, mat1, mat1_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec2, vec2_ptr, 0);
	(*env)->ReleasePrimitiveArrayCritical(env, vec3, vec3_ptr, 0);
}

void pprint(jdouble* mat1, jint mat1_r, jint mat1_c) {
  int i = 0;
  int j = 0;
  for (i=0; i < mat1_r; i++){
    printf("[ ");
    for (j=0; j < mat1_c; j++){
      printf("%f\t", mat1[i*mat1_c + j]);
    }
    printf("]");
    printf("\n");
  }
  printf("\n\n");
}
