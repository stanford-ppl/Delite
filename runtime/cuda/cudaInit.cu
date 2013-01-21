#include <jni.h>
#include <cuda_runtime.h>

extern "C" JNIEXPORT void JNICALL Java_ppl_delite_runtime_executor_AccExecutionThread_initializeDevice(JNIEnv* env, jobject obj, jint deviceNum);

JNIEXPORT void JNICALL Java_ppl_delite_runtime_executor_AccExecutionThread_initializeDevice(JNIEnv* env, jobject obj, jint deviceNum) {
	//chose device num
  if(cudaSuccess != cudaSetDevice(deviceNum)) {
    printf("FATAL : GPU device could not be initialized. \n");	
    exit(1);
  }
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
  //called to initialize the device (can take a while)
  if(cudaSuccess != cudaThreadSynchronize()) {
    printf("FATAL : GPU device has crashed (cudaThreadSynchronize). \n");	
    exit(1);
  }
}
