#include <jni.h>

extern "C" JNIEXPORT void JNICALL Java_ppl_delite_runtime_executor_GPUExecutionThread_initializeDevice(JNIEnv* env, jobject obj, jint deviceNum);

//TODO: Check initialization errors!
//TODO: Move more stuffs to the initialization phase
JNIEXPORT void JNICALL Java_ppl_delite_runtime_executor_GPUExecutionThread_initializeDevice(JNIEnv* env, jobject obj, jint deviceNum) {

    printf("OpenCL device is being initialized....\n");

/*
    char buf[1024];
    cl_uint numPlatforms;
    cl_uint numDevices;
    clGetPlatformIDs(0, NULL, &numPlatforms);
    cl_platform_id *platformList = (cl_platform_id *)malloc(sizeof(cl_platform_id) * numPlatforms);
    cl_int ret = clGetPlatformIDs(numPlatforms, platformList, NULL);

    clGetPlatformInfo(platformList[0],CL_PLATFORM_NAME,1024,buf,NULL);
    printf("\nDelite Runtime is using OpenCL platform : %s\n\n", buf);

    cl_device_id device_id = NULL;

    //ret = clGetDeviceIDs( platform_id, CL_DEVICE_TYPE_DEFAULT, 1,
    ret = clGetDeviceIDs( platformList[0], CL_DEVICE_TYPE_GPU, 1, &device_id, &numDevices);
*/

}
