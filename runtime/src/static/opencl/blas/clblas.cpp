#include "clblas.h"

//#define DEBUG

/*
const char *clblasKernels =
"__kernel void clblasSgemm(__global float *in1, __global float *in2, __global float *out, int in1_numRows, int in2_numCols, int in2_numRows) {"
"  for(int i=0; i<in1_numRows; i++) {"
"    for(int j=0; j<in2_numCols; j++) {"
"      float temp = 0.0f;"
"      for(int t=0; t<in2_numRows; t++) {"
"        temp += in1[i*in2_numRows+t] * in2[t*in2_numCols+j];"
"      }"
"      out[i*in2_numCols+j] = temp;"
"    }"
"  }"
"}"
"__kernel void clblasSgemv(__global float *in1, __global float *in2, __global float *out, int in1_numRows, int in1_numCols) {"
"  int idxX = get_global_id(0);"
"  if( idxX == 0) {"
"    for(int i=0; i<in1_numRows; i++) {"
"      float temp = 0.0f;"
"      for(int j=0; j<in1_numCols; j++) {"
"        temp += in1[i*in1_numCols+j] * in2[j];"
"      }"
"      out[i] = temp;"
"    }"
"  }"
"}";
*/
const char *clblasKernels =
"__kernel void clblasSgemm(__global float *in1, __global float *in2, __global float *out, int in1_numRows, int in2_numCols, int in2_numRows) {"
"  int idxX = get_global_id(0);"
"  if( idxX < in2_numCols ) {"
"    for(int i=0; i<in1_numRows; i++) {"
"      float temp = 0.0f;"
"      for(int j=0; j<in2_numRows; j++) {"
"        temp += in1[i*in2_numRows+j] * in2[j*in2_numCols+idxX];"
"      }"
"      out[i*in2_numCols+idxX] = temp;"
"    }"
"  }"
"}"
"__kernel void clblasSgemv(__global float *in1, __global float *in2, __global float *out, int in1_numRows, int in1_numCols) {"
"  int idxX = get_global_id(0);"
"  if( idxX < in1_numRows ) {"
"    float temp = 0.0f;"
"    for(int i=0; i<in1_numCols; i++) {"
"      temp += in1[idxX*in1_numCols+i] * in2[i];"
"    }"
"    out[idxX] = temp;"
"  }"
"}"
"__kernel void clblasDgemm(__global double *in1, __global double *in2, __global double *out, int in1_numRows, int in2_numCols, int in2_numRows) {"
"  int idxX = get_global_id(0);"
"  if( idxX < in2_numCols ) {"
"    for(int i=0; i<in1_numRows; i++) {"
"      double temp = 0.0;"
"      for(int j=0; j<in2_numRows; j++) {"
"        temp += in1[i*in2_numRows+j] * in2[j*in2_numCols+idxX];"
"      }"
"      out[i*in2_numCols+idxX] = temp;"
"    }"
"  }"
"}"
"__kernel void clblasDgemv(__global double *in1, __global double *in2, __global double *out, int in1_numRows, int in1_numCols) {"
"  int idxX = get_global_id(0);"
"  if( idxX < in1_numRows ) {"
"    double temp = 0.0;"
"    for(int i=0; i<in1_numCols; i++) {"
"      temp += in1[idxX*in1_numCols+i] * in2[i];"
"    }"
"    out[idxX] = temp;"
"  }"
"}";

cl_command_queue command_queue;
cl_kernel kernel_sgemm;
cl_kernel kernel_sgemv;
cl_kernel kernel_dgemm;
cl_kernel kernel_dgemv;

void clblasInit(cl_context context, cl_device_id device_id) {
  printf("Initializing the opencl BLAS\n");
  cl_int ret = CL_SUCCESS;

  cl_program program = clCreateProgramWithSource(context, 1, (const char **)&clblasKernels, NULL, &ret);
  if(ret != CL_SUCCESS)
	  printf("ERROR during kernel compilation\n");
  
  ret = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);
  if(ret != CL_SUCCESS)
	  printf("ERROR during kernel compilation\n");

  /* Create Kernels (set the global kernel variables */
  kernel_sgemm = clCreateKernel(program, "clblasSgemm", &ret);
  kernel_sgemv = clCreateKernel(program, "clblasSgemv", &ret);
  kernel_dgemm = clCreateKernel(program, "clblasDgemm", &ret);
  kernel_dgemv = clCreateKernel(program, "clblasDgemv", &ret);
}

void clblasSetQueue(cl_command_queue cmd_q) {
  printf("Setting the command queue\n");
  command_queue = cmd_q;
}

//m : y.col
//n : x.row
//k : y.row
void clblasSgemm(char transa, char transb, int m, int n, int k, float alpha, cl_mem A, int lda, cl_mem B, int ldb, float beta, cl_mem C, int ldc) {
  
  cl_int ret;
  ret = clSetKernelArg(kernel_sgemm, 0, sizeof(cl_mem), (void *)&B);
  ret = clSetKernelArg(kernel_sgemm, 1, sizeof(cl_mem), (void *)&A);
  ret = clSetKernelArg(kernel_sgemm, 2, sizeof(cl_mem), (void *)&C);
  ret = clSetKernelArg(kernel_sgemm, 3, sizeof(int), (void *)&n);
  ret = clSetKernelArg(kernel_sgemm, 4, sizeof(int), (void *)&m);
  ret = clSetKernelArg(kernel_sgemm, 5, sizeof(int), (void *)&k);

  /*
  //Using only 1 thread 
  size_t global_item_size = 1; 
  size_t local_item_size = 1; 
  ret = clEnqueueNDRangeKernel(command_queue, kernel_sgemm, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
  */
cl_event event_sgemv;
cl_ulong start_sgemv;
cl_ulong end_sgemv;

  size_t local_item_size = 64; 
  size_t global_item_size = 64*(1+(m-1)/64); 
  ret = clEnqueueNDRangeKernel(command_queue, kernel_sgemm, 1, NULL, &global_item_size, &local_item_size, 0, NULL, &event_sgemv);
  if(ret != CL_SUCCESS)
	  printf("ERROR during kernel 2\n");
  
  clFinish(command_queue);
#ifdef DEBUG
  clWaitForEvents(1, &event_sgemv);
  clGetEventProfilingInfo(event_sgemv, CL_PROFILING_COMMAND_START, sizeof(cl_ulong), &start_sgemv, NULL);
  clGetEventProfilingInfo(event_sgemv, CL_PROFILING_COMMAND_END, sizeof(cl_ulong), &end_sgemv, NULL);
  float exeTime_sgemv = (end_sgemv - start_sgemv) * 1.0e-6f;
  printf("Elapsed Time sgemm: %f [ms]\n", exeTime_sgemv);
#endif
  clReleaseEvent(event_sgemv);
}

void clblasSgemv(char transa, int m, int n, float alpha, cl_mem A, int lda, cl_mem x, int incx, float beta, cl_mem y, int incy) {

  cl_int ret = CL_SUCCESS;
  ret = clSetKernelArg(kernel_sgemv, 0, sizeof(cl_mem), (void *)&A);
  ret = clSetKernelArg(kernel_sgemv, 1, sizeof(cl_mem), (void *)&x);
  ret = clSetKernelArg(kernel_sgemv, 2, sizeof(cl_mem), (void *)&y);
  ret = clSetKernelArg(kernel_sgemv, 3, sizeof(int), (void *)&n);
  ret = clSetKernelArg(kernel_sgemv, 4, sizeof(int), (void *)&m);
  if(ret != CL_SUCCESS)
	  printf("ERROR during kernel 1\n");


  /*
  //Using only 1 thread 
  size_t global_item_size = 1;
  size_t local_item_size = 1;
  */

  size_t local_item_size = 64;
  size_t global_item_size = 64*(1+(m-1)/64);

cl_event event_sgemv;
cl_ulong start_sgemv;
cl_ulong end_sgemv;

  ret = clEnqueueNDRangeKernel(command_queue, kernel_sgemv, 1, NULL, &global_item_size, &local_item_size, 0, NULL, &event_sgemv);
  if(ret != CL_SUCCESS)
	  printf("ERROR during kernel 2\n");

clFinish(command_queue);
#ifdef DEBUG
clWaitForEvents(1, &event_sgemv);
clGetEventProfilingInfo(event_sgemv, CL_PROFILING_COMMAND_START, sizeof(cl_ulong), &start_sgemv, NULL);
clGetEventProfilingInfo(event_sgemv, CL_PROFILING_COMMAND_END, sizeof(cl_ulong), &end_sgemv, NULL);
float exeTime_sgemv = (end_sgemv - start_sgemv) * 1.0e-6f;
printf("Elapsed Time sgemv: %f [ms]\n", exeTime_sgemv);
#endif
  clReleaseEvent(event_sgemv);
  //clFinish(command_queue);
}

//m : y.col
//n : x.row
//k : y.row
void clblasDgemm(char transa, char transb, int m, int n, int k, double alpha, cl_mem A, int lda, cl_mem B, int ldb, double beta, cl_mem C, int ldc) {
  
  cl_int ret;
  ret = clSetKernelArg(kernel_dgemm, 0, sizeof(cl_mem), (void *)&B);
  ret = clSetKernelArg(kernel_dgemm, 1, sizeof(cl_mem), (void *)&A);
  ret = clSetKernelArg(kernel_dgemm, 2, sizeof(cl_mem), (void *)&C);
  ret = clSetKernelArg(kernel_dgemm, 3, sizeof(int), (void *)&n);
  ret = clSetKernelArg(kernel_dgemm, 4, sizeof(int), (void *)&m);
  ret = clSetKernelArg(kernel_dgemm, 5, sizeof(int), (void *)&k);

  /*
  //Using only 1 thread 
  size_t global_item_size = 1; 
  size_t local_item_size = 1; 
  ret = clEnqueueNDRangeKernel(command_queue, kernel_sgemm, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
  */
cl_event event_dgemm;
cl_ulong start_dgemm;
cl_ulong end_dgemm;

  size_t local_item_size = 64; 
  size_t global_item_size = 64*(1+(m-1)/64); 
  ret = clEnqueueNDRangeKernel(command_queue, kernel_dgemm, 1, NULL, &global_item_size, &local_item_size, 0, NULL, &event_dgemm);
  if(ret != CL_SUCCESS)
	  printf("ERROR during kernel 2\n");
  
  clFinish(command_queue);
#ifdef DEBUG
  clWaitForEvents(1, &event_dgemm);
  clGetEventProfilingInfo(event_dgemm, CL_PROFILING_COMMAND_START, sizeof(cl_ulong), &start_dgemm, NULL);
  clGetEventProfilingInfo(event_dgemm, CL_PROFILING_COMMAND_END, sizeof(cl_ulong), &end_dgemm, NULL);
  float exeTime_dgemm = (end_dgemm - start_dgemm) * 1.0e-6f;
  printf("Elapsed Time dgemm: %f [ms]\n", exeTime_dgemm);
#endif
  clReleaseEvent(event_dgemm);
}

void clblasDgemv(char transa, int m, int n, double alpha, cl_mem A, int lda, cl_mem x, int incx, double beta, cl_mem y, int incy) {

  cl_int ret = CL_SUCCESS;
  ret = clSetKernelArg(kernel_dgemv, 0, sizeof(cl_mem), (void *)&A);
  ret = clSetKernelArg(kernel_dgemv, 1, sizeof(cl_mem), (void *)&x);
  ret = clSetKernelArg(kernel_dgemv, 2, sizeof(cl_mem), (void *)&y);
  ret = clSetKernelArg(kernel_dgemv, 3, sizeof(int), (void *)&n);
  ret = clSetKernelArg(kernel_dgemv, 4, sizeof(int), (void *)&m);
  if(ret != CL_SUCCESS)
	  printf("ERROR during kernel 1\n");


  /*
  //Using only 1 thread 
  size_t global_item_size = 1;
  size_t local_item_size = 1;
  */

  size_t local_item_size = 64;
  size_t global_item_size = 64*(1+(m-1)/64);

cl_event event_dgemv;
cl_ulong start_dgemv;
cl_ulong end_dgemv;

  ret = clEnqueueNDRangeKernel(command_queue, kernel_dgemv, 1, NULL, &global_item_size, &local_item_size, 0, NULL, &event_dgemv);
  if(ret != CL_SUCCESS)
	  printf("ERROR during kernel 2\n");

clFinish(command_queue);
#ifdef DEBUG
clWaitForEvents(1, &event_dgemv);
clGetEventProfilingInfo(event_dgemv, CL_PROFILING_COMMAND_START, sizeof(cl_ulong), &start_dgemv, NULL);
clGetEventProfilingInfo(event_dgemv, CL_PROFILING_COMMAND_END, sizeof(cl_ulong), &end_dgemv, NULL);
float exeTime_dgemv = (end_dgemv - start_sgemv) * 1.0e-6f;
printf("Elapsed Time dgemv: %f [ms]\n", exeTime_dgemv);
#endif
  clReleaseEvent(event_dgemv);
  //clFinish(command_queue);
}
