#ifndef _OPENCLBLAS_
#define _OPENCLBLAS_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <CL/cl.h>

//extern "C" {
void clblasInit(cl_context context, cl_device_id device_id);
void clblasSetQueue(cl_command_queue cmd_q);
void clblasSgemm(char transa, char transb, int m, int n, int k, float alpha, cl_mem A, int lda, cl_mem B, int ldb, float beta, cl_mem C, int ldc);
void clblasSgemv(char transa, int m, int n, float alpha, cl_mem A, int lda, cl_mem x, int incx, float beta, cl_mem y, int incy);
void clblasDgemm(char transa, char transb, int m, int n, int k, double alpha, cl_mem A, int lda, cl_mem B, int ldb, double beta, cl_mem C, int ldc);
void clblasDgemv(char transa, int m, int n, double alpha, cl_mem A, int lda, cl_mem x, int incx, double beta, cl_mem y, int incy);
//};
//TODO: For windows below should be used to generate dynamic load library
//__declspec(dllexport) void clblasInit(cl_context context, cl_device_id device_id);
//__declspec(dllexport) void clblasSetQueue(cl_command_queue cmd_q);
//__declspec(dllexport) void clblasSgemm(char transa, char transb, int m, int n, int k, float alpha, cl_mem A, int lda, cl_mem B, int ldb, float beta, cl_mem C, int ldc);
//__declspec(dllexport) void clblasSgemv(char transa, int m, int n, float alpha, cl_mem A, int lda, cl_mem x, int incx, float beta, cl_mem y, int incy);

#endif
