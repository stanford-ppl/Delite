#include <jni.h>
#include <cuda_runtime.h>

extern "C" JNIEXPORT void JNICALL Java_ppl_delite_runtime_executor_GPUExecutionThread_initializeDevice(JNIEnv* env, jobject obj, jint deviceNum);

JNIEXPORT void JNICALL Java_ppl_delite_runtime_executor_GPUExecutionThread_initializeDevice(JNIEnv* env, jobject obj, jint deviceNum) {
	cudaSetDevice(deviceNum); //chose device num
	cudaSetDeviceFlags(cudaDeviceBlockingSync); //set device options
	cudaThreadSynchronize();//called to initialize the device (can take a while)
}
