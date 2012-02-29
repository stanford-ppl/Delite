package ppl.delite.runtime.codegen

import java.util.ArrayDeque
import ppl.delite.runtime.graph.ops._
import collection.mutable.ArrayBuffer
import java.io.File
import sun.net.www.content.image.x_xbitmap
import ppl.delite.runtime.graph.targets.{OPData, Targets}
import ppl.delite.runtime._

trait OpenCLGPUExecutableGenerator extends GPUExecutableGenerator {

  val target = Targets.OpenCL

  var argIdx : Int = 0

  protected def emitCppHeader = {
    val out = new StringBuilder

    //the header
    writeHeader(out)

    //write globals
    writeGlobals(out)

    //the event function
    //writeEventFunction(out)
    //writeHostEventFunction(out)

    out.toString
  }

  protected def emitCppBody(schedule: ArrayDeque[DeliteOP], location: Int, syncList: ArrayBuffer[DeliteOP]): String = {
    val out = new StringBuilder //the output string
    implicit val aliases = new AliasTable[(DeliteOP,String)]

    //the JNI method
    writeFunctionHeader(location, out)

    //initialize
    writeGlobalsInitializer(out)
    val locations = Range.inclusive(0,location).toSet
    writeJNIInitializer(locations, out)

    //execute
    addKernelCalls(schedule, location, new ArrayBuffer[(DeliteOP,String)], new ArrayBuffer[DeliteOP], syncList, out)

    writeJNIFinalizer(locations, out)
    out.append('}')
    out.append('\n')

    out.toString
  }

  protected def writeHeader(out: StringBuilder) {

    if (Config.profile) out.append("#define PROFILE_ENABLE\n")
    out.append("#include <stdio.h>\n") // stdio
    out.append("#include <stdlib.h>\n") // stdlib
    out.append("#include <jni.h>\n") //jni
    out.append("#include <CL/cl.h>\n") //OpenCL API
    out.append("#include \"DeliteOpenCL.cpp\"\n") //Delite-OpenCL interface for DSL
    out.append("#include \"helperFuncs.h\"\n") //imports all dsl kernels and helper functions
    out.append("#include \"clblas.h\"\n")
    out.append("#include \"dsl.h\"\n")
    out.append("#define MAX_SRC_SIZE 1048576\n")
  }

  protected def writeFunctionHeader(location: Int, out: StringBuilder) {
    val function = "JNIEXPORT void JNICALL Java_Executable" + location + "_00024_hostGPU(JNIEnv* jnienv, jobject object)"
    out.append("extern \"C\" ") //necessary because of JNI
    out.append(function)
    out.append(";\n")
    out.append(function)
    out.append(" {\n")
  }

  protected def writeGlobals(out: StringBuilder) {
    out.append("JNIEnv* env;\n")
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
    out.append("env = jnienv;\n")
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
    out.append("clblasInit(context,device_id);\n")
    out.append("clblasSetQueue(command_queue);\n")
  }


  //TODO: have multiple command queues for separate data transfers
  protected def addKernelCalls(schedule: ArrayDeque[DeliteOP], location: Int, available: ArrayBuffer[(DeliteOP,String)], awaited: ArrayBuffer[DeliteOP], syncList: ArrayBuffer[DeliteOP], out: StringBuilder)(implicit aliases:AliasTable[(DeliteOP,String)]) {
    //available: list of ops with data currently on gpu, have a "g" symbol
    //awaited: list of ops synchronized with but data only resides on cpu, have a "c" symbol
    val iter = schedule.iterator
    while (iter.hasNext) {
      val op = iter.next
      //add to available & awaited lists
      available += Pair(op,op.id)
      awaited += op

      if (op.isInstanceOf[OP_Nested]) makeNestedFunction(op, location)

      //get all dependencies
      for (dep <- op.getDependencies) { //foreach dependency
        if(!awaited.contains(dep)) { //this dependency does not yet exist for this resource
          awaited += dep
          for (sym <- dep.getOutputs) //TODO: should get and set outputs individually; SMP needs to adopt this strategy as well
            writeGetter(dep, sym, location, out) //get to synchronize
        }
      }
      //get kernel inputs (dependencies that could require a memory transfer)
      var addInputCopy = false
      for ((input, sym) <- op.getInputs) { //foreach input
        val inData = op.getGPUMetadata(target).inputs.getOrElse((input, sym), null)
        if(!available.contains(input,sym)) { //this input does not yet exist on the device
          //add to available list
          available += Pair(input,sym)
          //write a copy function for objects
          if (inData != null) { //only perform a copy for object types
            addInputCopy = true
            writeInputCopy(input, sym, inData.func, inData.resultType, out)
          }
          else if (isPrimitiveType(input.outputType(sym)))
            writeInputCast(input, sym, out) //if primitive type, simply cast to transform from "c" type into "g" type
          else {
            //TODO: What is this case??
            assert(op.isInstanceOf[OP_Nested]) //object without copy must be for a nested function call
            available -= Pair(input,sym) //this input doesn't actually reside on GPU
          }
        }
        else if (needsUpdate(op, input, sym)) { //input exists on device but data is old
          //write a new copy function (input must be an object)
          addInputCopy = true
          writeInputCopy(input, sym, inData.func, inData.resultType, out)
        }
      }
      /*
      if (addInputCopy) { //if a h2d data transfer occurred
        //sync kernel launch with completion of last input copy
        out.append("addEvent(h2dStream, kernelStream);\n")
      }
      */

      //write the temporary allocations
      writeTempAllocs(op, out)
      //write the output allocation
      writeOutputAllocs(op, out)
      //write the call
      if (op.isInstanceOf[OP_Nested])
        writeFunctionCall(op, out)
      else if (op.isInstanceOf[OP_External]) {
        OpenCLCompile.externList.append(op.id)
        writeLibraryCall(op, out)
      }
      else
        writeKernelCall(op, out)

      //write the setter
      var addSetter = false
      for (cons <- op.getConsumers) {
        if (cons.scheduledResource != location) addSetter = true
      }
      if (addSetter) {
        syncList += op //add op to list that needs sync generation
        /*
        //sync output copy with kernel completion
        out.append("addEvent(kernelStream, d2hStream);\n")
        */
        //write a setter
        //TODO: Is this the best time to copy back?
        writeSetters(op, location, out)
      }
      writeDataFrees(op, out, available)
    }
  }

  //TODO: should track if data has already been updated - implement current version for each resource - useful for gpu/cluster
  //TODO: updates don't work across loop iterations (write seen as happening after rather than before)
  protected def needsUpdate(op: DeliteOP, input: DeliteOP, sym: String): Boolean = {
    for (dep <- op.getDependencies) {
      if (dep.getMutableInputs.contains(input, sym) && dep.scheduledResource != op.scheduledResource) {
        return true
      }
    }
    false
  }

  protected def writeOutputAllocs(op: DeliteOP, out: StringBuilder) {
    if (op.isInstanceOf[OP_Executable]) {
      for ((data,name) <- op.getGPUMetadata(target).outputs) {
        out.append(op.outputType(Targets.OpenCL,name))
        out.append("* ")
        out.append(getSymGPU(name))
        out.append(" = ")
        out.append(data.func)
        out.append('(')
        writeInputList(op, data, out)
        out.append(");\n")
        writeMemoryAdd(name, out)
      }
    }

  }

  protected def writeTempAllocs(op: DeliteOP, out: StringBuilder) {
    for ((temp,name) <- op.getGPUMetadata(target).temps) {
      out.append(temp.resultType)
      out.append("* ")
      out.append(getSymGPU(name))
      out.append(" = ")
      out.append(temp.func)
      out.append('(')
      var first = true
      for ((in,sym) <- temp.inputs) {
        if (!first) out.append(',')
        first = false
        out.append(getSymGPU(sym))
      }
      out.append(");\n")
      writeMemoryAdd(name, out)
    }
  }

  protected def writeMemoryAdd(sym: String, out: StringBuilder) {
    out.append("clMemoryMap->insert(pair<void*,list<cl_mem>*>(")
    out.append(getSymGPU(sym))
    out.append(",lastAlloc));\n")
    out.append("lastAlloc = new list<cl_mem>();\n")
  }

  protected def writeInputList(op: DeliteOP, data: OPData, out: StringBuilder) {
    var first = true
    for ((in,sym) <- data.inputs) {
      if (!first) out.append(',')
      first = false
      out.append(getSymGPU(sym))
    }
  }

  protected def writeKernelCall(op: DeliteOP, out: StringBuilder) {
    if (op.task == null) return //dummy op

    // Create the OpenCL kernel
    //TODO: Move this to init phase
    out.append("cl_kernel " + op.task + " = clCreateKernel(program, \"" + op.task + "\", &ret);\n")

    // Set the arguments of the kernel
    argIdx = 0
    writeOutputs(op, out)
    writeInputs(op, out)
    writeTemps(op,out)

    // Set the kernel local dimensions
    out.append("size_t local_item_size_%s = ".format(op.task))
    out.append(op.getGPUMetadata(target).blockSizeX.func)
    out.append('(')
    writeInputList(op,op.getGPUMetadata(target).blockSizeX,out)
    out.append(')')
    out.append(";\n")

    // Set the kernel global dimensions
    out.append("size_t global_item_size_%s = ".format(op.task))
    out.append(op.getGPUMetadata(target).dimSizeX.func)
    out.append('(')
    writeInputList(op,op.getGPUMetadata(target).dimSizeX,out)
    out.append(')')
    out.append(" * local_item_size_%s;\n".format(op.task))


    // Launch the kernel
    out.append("cl_event event_"+op.task+";\n")
    out.append("cl_ulong start_%s;\n".format(op.task))
    out.append("cl_ulong end_%s;\n".format(op.task))
    out.append("ret = clEnqueueNDRangeKernel(command_queue, " + op.task + ", 1, NULL, &global_item_size_%s, &local_item_size_%s, 0, NULL, &%s);\n".format(op.task,op.task,"event_"+op.task))

    // Flush the command queue
    // TODO: The reason to flush here is not to use events to synchronize kernels to decide when to free the allocations.
    // TODO: By using multiple command queue and events for synchronization, we can remove this flush.
    // TODO: Doing that also enables overlapping the computation with communication.
    out.append("clFinish(command_queue);\n")
    if (Config.profile) {
      out.append("clWaitForEvents(1, &event_%s);\n".format(op.task))
      out.append("clGetEventProfilingInfo(event_%s, CL_PROFILING_COMMAND_START, sizeof(cl_ulong), &start_%s, NULL);\n".format(op.task,op.task))
      out.append("clGetEventProfilingInfo(event_%s, CL_PROFILING_COMMAND_END, sizeof(cl_ulong), &end_%s, NULL);\n".format(op.task,op.task))
      out.append("float exeTime_%s = (end_%s - start_%s) * 1.0e-6f;\n".format(op.task,op.task,op.task))
      out.append("printf(\"Elapsed Time "+op.task+": %f [ms]\\n\", exeTime_"+op.task+");\n")
    }
    out.append("clReleaseEvent(event_"+op.task+");\n")
    out.append("clReleaseKernel("+op.task+");\n")
  }

  protected def writeLibraryCall(op: DeliteOP, out: StringBuilder) {
    if (op.task == null) return //dummy op
    out.append(op.task)
    out.append('(')
    assert(op.getOutputs.size == 1) //TODO: what does libCall support?
    for (name <- op.getOutputs) {
      if (op.outputType(name) != "Unit") {
        out.append('*')
        out.append(getSymGPU(name)) //first kernel input is OP output
        out.append(',')
      }
    }
    writeLibCallInputs(op, out) //then all op inputs
    //out.append(",kernelStream")
    out.append(");\n")
    out.append("clFinish(command_queue);\n")
    //assert(false)
  }

  protected def writeFunctionCall(op: DeliteOP, out: StringBuilder) {
    if (op.outputType != "Unit") {
      out.append(op.outputType(Targets.OpenCL))
      out.append(' ')
      out.append(getSymGPU(op.getOutputs.head))
      out.append(" = ")
    }
    out.append(op.task)
    out.append('(')
    var first = true
    for ((input,sym) <- op.getInputs) {
      if (op.getGPUMetadata(target).inputs.contains(input,sym) || isPrimitiveType(input.outputType(sym))) {
        if (!first) out.append(',')
        first = false
        out.append(getSymGPU(sym))
        if ((op.getMutableInputs. contains (input,sym)) && (input.getConsumers.filter(_.scheduledResource!=input.scheduledResource).nonEmpty)) {
          out.append(',')
          out.append(getSymCPU(sym))
        }
      }

    }
    out.append(");\n")
    //assert(false)
  }

  protected def writeGetter(op: DeliteOP, sym: String, location: Int, out: StringBuilder) {
    //get data from CPU
    if (op.outputType(sym) != "Unit") { //skip the variable declaration if return type is "Unit"
      out.append(getJNIType(op.outputType(sym)))
      out.append(' ')
      out.append(getSymCPU(sym))
      out.append(" = ")
    }
    out.append("env->CallStatic")
    out.append(getJNIFuncType(op.outputType(sym)))
    out.append("Method(cls")
    out.append(op.scheduledResource)
    out.append(",env->GetStaticMethodID(cls")
    out.append(op.scheduledResource)
    out.append(",\"get")
    out.append(location)
    out.append('_')
    out.append(getScalaSym(op, sym))
    out.append("\",\"()")
    out.append(getJNIOutputType(op.outputType(Targets.Scala,sym)))
    out.append("\"));\n")
  }

  protected def writeInputCopy(op: DeliteOP, sym: String, function: String, opType: String, out: StringBuilder) {
    //copy data from CPU to GPU
    out.append(opType)
    out.append("* ")
    out.append(getSymGPU(sym))
    out.append(" = ")
    out.append(function)
    out.append('(')
    out.append("env,") //JNI environment pointer
    out.append(getSymCPU(sym)) //jobject
    out.append(");\n")
    writeMemoryAdd(sym, out)
  }

  protected def writeInputCast(op: DeliteOP, sym: String, out: StringBuilder) {
    if (op.outputType(sym) != "Unit") {
      out.append(getCPrimitiveType(op.outputType(sym))) //C primitive
      out.append(' ')
      out.append(getSymGPU(sym))
      out.append(" = ")
      out.append('(') //cast
      out.append(getCPrimitiveType(op.outputType(sym))) //C primitive
      out.append(')')
      out.append(getSymCPU(sym)) //J primitive
      out.append(';')
      out.append('\n')
    }
  }

  protected def writeOutputs(op: DeliteOP, out: StringBuilder) {
    //if (op.outputType == "Unit") return

    for ((data,name) <- op.getGPUMetadata(target).outputs) {
      if (data.objFields.size != 0) {
        for (f <- data.objFields) {
          out.append("ret = clSetKernelArg(" + op.task + ", " + argIdx + ", sizeof(" + f._2 + "), (void*)&(" + getSymGPU(name) + "->" + f._1 + "));\n")
          argIdx += 1
        }
      }
      else {
        out.append("ret = clSetKernelArg(" + op.task + ", " + argIdx + ", sizeof(" + op.outputType(Targets.OpenCL) + "), (void *)&" + getSymGPU(name) + ");\n")
        argIdx += 1
      }
    }
  }

  protected def writeInputs(op: DeliteOP, out: StringBuilder) {

    for ((input,name) <- op.getInputs) {
      val opData = op.getGPUMetadata(target).inputs.getOrElse((input,name),null)
      if (opData != null) {
        assert(opData.objFields.size != 0)
        for (f <- opData.objFields) {
          out.append("ret = clSetKernelArg(" + op.task + ", " + argIdx + ", sizeof(" + f._2 + "), (void*)&(" + getSymGPU(name) + "->" + f._1 + "));\n")
          argIdx += 1
        }
      }
      else {
        out.append("ret = clSetKernelArg(" + op.task + ", " + argIdx + ", sizeof(" + getCPrimitiveType(input.outputType) + "), (void *)&" + getSymGPU(name) + ");\n")
        argIdx += 1
      }
    }
  }

  //TODO: handle temps as output
  protected def writeTemps(op: DeliteOP, out: StringBuilder) {
    var idx = 0
    for ((temp, name) <- op.getGPUMetadata(target).temps) {
      out.append("ret = clSetKernelArg(" + op.task + ", " + argIdx + ", sizeof(" + op.getGPUMetadata(target).temps(idx)._1.resultType + "), (void *)&" + getSymGPU(name) + ");\n")
      idx += 1
      argIdx += 1
    }
  }

    protected def writeTeddmps(op: DeliteOP, out: StringBuilder) {
    for ((temp, name) <- op.getGPUMetadata(target).temps) {
      out.append(',')
      out.append('*')
      out.append(getSymGPU(name))
    }
  }

  protected def writeLibCallInputs(op: DeliteOP, out: StringBuilder) {
    var first = true
    for ((input,name) <- op.getInputs) {
      if (!first) out.append(',')
      first = false
      if (!isPrimitiveType(input.outputType(name))) out.append('*')
      out.append(getSymGPU(name))
    }
  }


  protected def writeSetters(op: DeliteOP, location: Int, out: StringBuilder) {
    for ((in,name) <- op.getGPUMetadata(target).inputs.keys) {
      if (op.getMutableInputs.contains(in,name)) {
        //copy any mutated inputs from GPU to CPU
        val inData = op.getGPUMetadata(target).inputs(in,name)
        out.append(inData.funcReturn)
        out.append("(env,") //JNI environment pointer
        out.append(getSymCPU(name)) //jobject
        out.append(',')
        out.append(getSymGPU(name)) //C++ object
        out.append(");\n")
      }
    }

    for (name <- op.getOutputs) {
      var deleteLocalRef = false
      op.getGPUMetadata(target).outputs.find(_._2 == name) match {
        case Some((data, n)) => {
          //copy output from GPU to CPU
          out.append(getJNIType(op.outputType(name))) //jobject
          out.append(' ')
          out.append(getSymCPU(name))
          out.append(" = ")
          out.append(data.funcReturn)
          out.append('(')
          out.append("env,") //JNI environment pointer
          out.append(getSymGPU(name)) //C++ object
          out.append(");\n")
          deleteLocalRef = true
        }
        case None => //do nothing
      }

      //set data as available to CPU
      out.append("env->CallStaticVoidMethod(cls")
      out.append(location)
      out.append(",env->GetStaticMethodID(cls")
      out.append(location)
      out.append(",\"set")
      out.append(getScalaSym(op, name))
      out.append("\",\"(")
      out.append(getJNIArgType(op.outputType(name)))
      out.append(")V\"),")
      if (op.outputType(name) == "Unit") out.append("boxedUnit") else out.append(getSymCPU(name))
      out.append(");\n")

      //delete LocalRef
      if (deleteLocalRef) {
        out.append("env->DeleteLocalRef(")
        out.append(getSymCPU(name))
        out.append(");\n")
      }
    }
  }

  protected def writeDataFrees(op: DeliteOP, out: StringBuilder, available: ArrayBuffer[(DeliteOP,String)]) {
    var count = 0
    val freeItem = "freeItem_" + op.id

    def writeFreeInit() {
      out.append("FreeItem ")
      out.append(freeItem)
      out.append(";\n")
      out.append(freeItem)
      out.append(".keys = new list<void*>();\n")
    }

    def writeFree(sym: String) {
      if (count == 0) writeFreeInit()
      out.append(freeItem)
      out.append(".keys->push_back(")
      out.append(getSymGPU(sym))
      out.append(");\n")
    }

    def opFreeable(op: DeliteOP, sym: String) = {
      op.isInstanceOf[OP_Executable] && available.contains(op,sym)
    }

    def outputFreeable(op: DeliteOP, sym: String) = {
      !isPrimitiveType(op.outputType(sym))
    }

    //free temps
    for ((temp, name) <- op.getGPUMetadata(target).temps) {
      writeFree(name)
      count += 1
    }

    //free outputs (?)
    if (opFreeable(op,op.id)) {
      for (name <- op.getOutputs if outputFreeable(op, name)) {
        if (op.getConsumers.filter(c => c.getInputs.contains((op,name)) && c.scheduledResource == op.scheduledResource).size == 0) {
          writeFree(name)
          count += 1
        }
      }
    }

    //free inputs (?)
    for ((in,name) <- op.getInputs if(opFreeable(in,name) && outputFreeable(in, name))) {
      val possible = in.getConsumers.filter(c => c.getInputs.contains((in,name)) && c.scheduledResource == op.scheduledResource)
      var free = true
      for (p <- possible) {
        if (!available.contains(p,p.id)) free = false
      }
      if (free) {
        writeFree(name)
        count += 1
      }
    }

    if (count > 0) {
      //sync on kernel stream (if copied back guaranteed to have completed, so don't need sync on d2h stream)
      //out.append(freeItem)
      //out.append(".event = addHostEvent(kernelStream);\n")
      //TODO: Need to flush & finish command queue?
      out.append("freeList->push(")
      out.append(freeItem)
      out.append(");\n")
    }
  }

  protected def makeNestedFunction(op: DeliteOP, location: Int) {
    op match {
      case c: OP_Condition => new OpenCLGPUConditionGenerator(c, location).makeExecutable()
      case w: OP_While => new OpenCLGPUWhileGenerator(w, location).makeExecutable()
      case v: OP_Variant => new OpenCLGPUVariantGenerator(v, location).makeExecutable()
      case err => error("Unrecognized nested OP type: " + err.getClass.getSimpleName)
    }
  }

  protected def writeEventFunction(out: StringBuilder) {
    /*
    out.append("void addEvent(cudaStream_t fromStream, cudaStream_t toStream) {\n")
    out.append("cudaEvent_t event;\n")
    out.append("cudaEventCreateWithFlags(&event, cudaEventDisableTiming);\n")
    out.append("cudaEventRecord(event, fromStream);\n");

    out.append("cudaStreamWaitEvent(toStream, event, 0);\n")

    out.append("cudaEventDestroy(event);\n")
    out.append('}')
    out.append('\n')
    */
  }

  protected def writeHostEventFunction(out: StringBuilder) {
    /*
    out.append("cudaEvent_t addHostEvent(cudaStream_t stream) {\n")
    out.append("cudaEvent_t event;\n")
    out.append("cudaEventCreateWithFlags(&event, cudaEventDisableTiming | cudaEventBlockingSync);\n")
    out.append("cudaEventRecord(event, stream);\n");
    out.append("return event;\n")
    out.append('}')
    out.append('\n')
    */
  }

}

/*
abstract class OpenCLGPUScalaExecutableGenerator extends ExecutableGenerator {

  def emitScala(location: Int, syncList: ArrayBuffer[DeliteOP], kernelPath: String): String = {
    val out = new StringBuilder

    //the header
    writeHeader(out, location, "")

    //the run method
    out.append("def run() {\n")
    out.append("hostGPU\n")
    out.append('}')
    out.append('\n')

    //the native method
    out.append("@native def hostGPU : Unit\n")

    //link the native code upon object creation
    out.append("System.load(\"\"\"")
    out.append(OpenCLCompile.binCacheHome)
    out.append("openclHost.dll\"\"\")\n")

    //the sync methods/objects
    addSync(syncList, out)
    writeOuterSet(syncList, out) //helper set methods for JNI calls to access

    //an accessor method for the object
    addAccessor(out)

    //the footer
    out.append('}')
    out.append('\n')

    out.toString
  }

  protected def writeOuterSet(list: ArrayBuffer[DeliteOP], out: StringBuilder) {
    for (op <- list) {
      for (sym <- op.getOutputs) {
        out.append("def set")
        out.append(getSym(op, sym))
        out.append("(result : ")
        out.append(op.outputType(sym))
        out.append(") = ")
        out.append(getSync(op, sym))
        out.append(".set(result)\n")
      }
    }
  }
}
*/
