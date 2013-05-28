package ppl.delite.runtime.codegen

import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.ops.DeliteOP
import ppl.delite.runtime.graph.targets.Targets

/*
trait OpenCLExecutableGenerator extends ExecutableGenerator {

  protected def target = Targets.OpenCL

  protected def writeHeader(out: StringBuilder) {
    if (Config.profile) out.append("#define PROFILE_ENABLE\n")
    out.append("#include <CL/cl.h>\n") //OpenCL API
    out.append("#include \"DeliteOpenCL.cpp\"\n") //Delite-OpenCL interface for DSL
    out.append("#include \"" + target + "helperFuncs.cpp\"\n") //imports all dsl kernels and helper functions
    //out.append("#include \"clblas.h\"\n")
    //out.append("#include \"dsl.h\"\n")
    out.append(OpenCLCompile.headers.map(s => "#include \"" + s + "\"\n").mkString(""))
    out.append("#define MAX_SRC_SIZE 1048576\n")
  }

  protected def writeGlobals(out: StringBuilder) {
    // OpenCL device init
    //out.append("char *source_str = \"#pragma OPENCL EXTENSION cl_khr_fp64 : enable\\n __kernel void kernel_x9(__global double *a, __global double *b) {\\n int i = get_global_id(0);\\n if(i < 10)\\n a[i] = b[i] + 4.0;\\n }\";\n")
    out.append("FILE *fp;\n")
    out.append("char *source_str;\n")
    out.append("size_t source_size;\n")
    out.append("cl_uint numPlatforms;\n")
    out.append("cl_uint numDevices;\n")
    out.append("cl_platform_id *platformList;\n")
    out.append("cl_device_id device_id;\n")
    out.append("cl_context context;\n")
    out.append("cl_command_queue command_queue;\n")
    out.append("cl_program program;\n")
    out.append("cl_int ret;\n")
  }

  protected def writeGlobalsInitializer(out: StringBuilder) {
    out.append("hostInit();\n")
    // OpenCL device init
    if (System.getProperty("os.name").contains("Windows"))
      out.append("fp = fopen(\""+OpenCLCompile.binCacheHome.replace("\\","\\\\")+"clKernels.cl\",\"r\");\n")
    else
      out.append("fp = fopen(\""+OpenCLCompile.binCacheHome+"clKernels.cl\",\"r\");\n")
    out.append("if(!fp) { fprintf(stderr, \"Failed to load clKernels.cl ..\\n\"); exit(1); }\n")
    out.append("source_str = (char*)malloc(MAX_SRC_SIZE);\n")
    out.append("source_size = fread( source_str, 1, MAX_SRC_SIZE, fp);\n")
    out.append("fclose(fp);\n")
    out.append("clGetPlatformIDs(0, NULL, &numPlatforms);\n")
    out.append("platformList = (cl_platform_id*)malloc(sizeof(cl_platform_id) * numPlatforms);\n")
    out.append("ret = clGetPlatformIDs(numPlatforms, platformList, NULL);\n")
    out.append("char buf[1024];\n")
    out.append("clGetPlatformInfo(platformList[0],CL_PLATFORM_NAME,1024,buf,NULL);\n")
    //out.append("clGetPlatformInfo(platformList[1],CL_PLATFORM_NAME,1024,buf,NULL);\n")
    out.append("printf(\"Delite Runtime is using OpenCL platform : %s\\n\", buf);\n")
    out.append("ret = clGetDeviceIDs( platformList[0], CL_DEVICE_TYPE_GPU, 1, &device_id, &numDevices);\n")
    //out.append("ret = clGetDeviceIDs( platformList[1], CL_DEVICE_TYPE_GPU, 1, &device_id, &numDevices);\n")
    out.append("context = clCreateContext( NULL, 1, &device_id, NULL, NULL, &ret);\n")
    out.append("command_queue = clCreateCommandQueue(context, device_id, CL_QUEUE_PROFILING_ENABLE, &ret);\n")
    out.append("program = clCreateProgramWithSource(context, 1, (const char **)&source_str, (const size_t *)&source_size, &ret);\n")
    out.append("ret = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);\n")
    out.append("if(ret != CL_SUCCESS) printf(\"ERROR during kernel compillation\\n\");\n")
    out.append("size_t logsize;\n")
    out.append("clGetProgramBuildInfo(program,device_id,CL_PROGRAM_BUILD_LOG,0,NULL,&logsize);\n")
    out.append("char *log = (char *)malloc(sizeof(char)*(logsize+1));\n")
    out.append("clGetProgramBuildInfo(program,device_id,CL_PROGRAM_BUILD_LOG,logsize,log,NULL);\n")
    out.append("printf(\"The CL compile log: %s\\n\", log);\n")
    /*
   out.append("char **ptx; *ptx = (char*)malloc(1048576*8*128);\n")
   out.append("clGetProgramInfo(program,CL_PROGRAM_BINARIES,1,ptx,NULL);\n")
   out.append("printf(\"HIHI%sHIHI\",*ptx);\n")
    */

    //Do dummy operation
    out.append("int dummy_size = 4;\n")
    out.append("float *dummy = (float *)malloc(dummy_size);\n")
    out.append("cl_mem dummy_mem_obj = clCreateBuffer(context, CL_MEM_READ_ONLY, 4, NULL, &ret);\n")
    out.append("ret = clEnqueueWriteBuffer(command_queue, dummy_mem_obj, CL_TRUE, 0, 4, dummy, 0, NULL, NULL);\n")
    out.append("clFinish(command_queue);\n")

    //Do dummy kernel launch
    out.append("size_t dummy_global_item_size = 4;\n")
    out.append("size_t dummy_local_item_size = 1;\n")
    out.append("cl_kernel dummy_kernel = clCreateKernel(program, \"dummy_kernel\", &ret);\n")
    out.append("ret = clSetKernelArg(dummy_kernel, 0, sizeof(cl_mem), (void *)&dummy_mem_obj);\n")
    out.append("ret = clSetKernelArg(dummy_kernel, 1, sizeof(int), (void *)&dummy_size);\n")
    out.append("ret = clEnqueueNDRangeKernel(command_queue, dummy_kernel, 1, NULL, &dummy_global_item_size, &dummy_local_item_size, 0, NULL, NULL);\n")
    out.append("clFinish(command_queue);\n")

    // BLAS init
    //out.append("clblasInit(context,device_id);\n")
    //out.append("clblasSetQueue(command_queue);\n")
  }

  override protected def writeLibraryCall(op: DeliteOP) {
    super.writeLibraryCall(op)
    out.append("clFinish(command_queue);\n")
  }

}
*/
