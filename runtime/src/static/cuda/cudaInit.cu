#include <jni.h>
#include <cuda_runtime.h>

extern "C" JNIEXPORT void JNICALL Java_ppl_delite_runtime_executor_CudaExecutionThread_initializeThread(JNIEnv* env, jobject obj, jint threadId, jint numThreads);

JNIEXPORT void JNICALL Java_ppl_delite_runtime_executor_CudaExecutionThread_initializeThread(JNIEnv* env, jobject obj, jint threadId, jint numThreads) {
  //chose device num
  if(cudaSuccess != cudaSetDevice(threadId)) {
    printf("FATAL : GPU device could not be initialized. \n");	
    exit(1);
  }

  // Using blocksynchronization slows down some kepler devices due to power management. Default is polling.
  /*
  //reset the device
  if(cudaSuccess != cudaDeviceReset()) {
    printf("FATAL : cudaDeviceReset() failed \n");	
    exit(1);
  }
  //set device options
  if(cudaSuccess != cudaSetDeviceFlags(cudaDeviceScheduleBlockingSync)) {
    printf("FATAL : GPU device has crashed (cudaSetDviceFlags). \n");	
    exit(1);
  }
  */

  //called to initialize the device (can take a while)
  if(cudaSuccess != cudaThreadSynchronize()) {
    printf("FATAL : GPU device has crashed (cudaThreadSynchronize). \n");	
    exit(1);
  }
}
