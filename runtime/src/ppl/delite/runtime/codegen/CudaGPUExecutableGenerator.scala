package ppl.delite.runtime.codegen

import java.util.ArrayDeque
import kernels.cuda.MultiLoop_GPU_Array_Generator
import ppl.delite.runtime.graph.ops._
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.targets.{OPData, Targets}

trait CudaGPUExecutableGenerator extends GPUExecutableGenerator {

  val target = Targets.Cuda

  protected def emitCppHeader = {
    val out = new StringBuilder

    //the header
    writeHeader(out)

    //write globals
    writeGlobals(out)

    //the event function
    writeEventFunction(out)
    writeHostEventFunction(out)

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
    out.append("#include <jni.h>\n") //jni
    out.append("#include <cuda_runtime.h>\n") //Cuda runtime api
    out.append("#include \"cublas.h\"\n") //cublas library
    out.append("#include \"DeliteCuda.cu\"\n") //Delite-Cuda interface for DSL
    out.append("#include \"helperFuncs.cu\"\n")
    out.append("#include \"library.h\"\n")
    out.append("#include \"dsl.h\"\n") //imports all dsl kernels
    //TODO: Not enabling C target would not generate dsl.hpp    
    out.append("#include \"dsl.hpp\"\n") //imports all dsl kernels and helper functions
    out.append(CudaCompile.headers.map(s => "#include \"" + s + "\"\n").mkString(""))
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
    out.append("cudaStream_t kernelStream;\n")
    out.append("cudaStream_t h2dStream;\n")
    out.append("cudaStream_t d2hStream;\n")
  }

  protected def writeGlobalsInitializer(out: StringBuilder) {
    out.append("env = jnienv;\n")
    out.append("cudaStreamCreate(&kernelStream);\n")
    out.append("cudaStreamCreate(&h2dStream);\n")
    out.append("cudaStreamCreate(&d2hStream);\n")
    out.append("cublasSetKernelStream(kernelStream);\n")  // set cublas to use the kernel stream
  }



  protected def addKernelCalls(schedule: ArrayDeque[DeliteOP], location: Int, available: ArrayBuffer[(DeliteOP,String)], awaited: ArrayBuffer[DeliteOP], syncList: ArrayBuffer[DeliteOP], out: StringBuilder)(implicit aliases:AliasTable[(DeliteOP,String)]) {
    //available: list of ops with data currently on gpu, have a "g" symbol
    //awaited: list of ops synchronized with but data only resides on cpu, have a "c" symbol
    val getterList = new ArrayBuffer[String]
    val iter = schedule.iterator
    while (iter.hasNext) {
      val op = iter.next
      //add to available & awaited lists
      if (op.isInstanceOf[OP_Nested])
        available += Pair(op,op.id.replaceAll("_"+op.scheduledResource,""))
      else
        available ++= op.getGPUMetadata(target).outputs.map(o=>Pair(op,o._2))

      awaited += op

      if (op.isInstanceOf[OP_Nested]) makeNestedFunction(op, location)

      //get all dependencies
      for (dep <- op.getDependencies) { //foreach dependency
        if(!awaited.contains(dep)) { //this dependency does not yet exist for this resource
          awaited += dep
          for (sym <- dep.getOutputs) //TODO: should get and set outputs individually; SMP needs to adopt this strategy as well
            writeGetter(dep, sym, location, getterList, out) //get to synchronize
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
            writeInputCopy(input, sym, inData.func, inData.resultType, out, true)
          }
          else if (isPrimitiveType(input.outputType(sym)))
            writeInputCast(input, sym, out) //if primitive type, simply cast to transform from "c" type into "g" type
          else {
            assert(op.isInstanceOf[OP_Nested],op.id+":cuda metadata for output copy not specified") //object without copy must be for a nested function call
            available -= Pair(input,sym) //this input doesn't actually reside on GPU
          }
        }
        else if (needsUpdate(op, input, sym)) { //input exists on device but data is old
          //write a new copy function (input must be an object)
          addInputCopy = true
          writeInputCopy(input, sym, inData.func, inData.resultType, out, false)
        }
      }
      if (addInputCopy) { //if a h2d data transfer occurred
        //sync kernel launch with completion of last input copy
        out.append("addEvent(h2dStream, kernelStream);\n")
      }

      //write the temporary allocations
      writeTempAllocs(op, out)
      //write the output allocation
      writeOutputAllocs(op, out)
      //write the call
      op match {
        case _ if op.task != null && !op.supportsTarget(target) => writeHostKernelCall(op, out)
        case n: OP_Nested => writeFunctionCall(op, out)
        case e: OP_External => writeLibraryCall(op, out)
        case m: OP_MultiLoop => writeMultiKernelCall(op, out)
        case _ => writeKernelCall(op, out)
      }

      //write the setter
      val addWriteBack = needsWriteBack(op)       
      if (addWriteBack) {
        out.append("addEvent(kernelStream, d2hStream);\n")
        writeCopyBack(op, location, out)
      }
      val addSetter = op.getConsumers exists { _.scheduledResource != location }
      if (addSetter) {
        syncList += op //add op to list that needs sync generation
        //sync output copy with kernel completion
        //out.append("addEvent(kernelStream, d2hStream);\n")
        //write a setter
        writeSetters(op, location, out)
      }
      writeDataFrees(op, out, available)
    }
    writeGetterFrees(getterList, out)
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

  protected def needsWriteBack(op: DeliteOP): Boolean = {
    // try to coalesce writes
    for (cons <- op.getConsumers) {
      if (cons.scheduledResource != op.scheduledResource) {
        val deps = op.getMutableInputs intersect cons.getInputs.toSet
        for (d <- deps) {
          if (!(op.getConsumers exists { c => c.getMutableInputs.contains(d) &&
                                              c.getConsumers.contains(cons) &&
                                              c.scheduledResource == op.scheduledResource }))
            return true
        }
      }
    }
    false
  }

  protected def writeOutputAllocs(op: DeliteOP, out: StringBuilder) {
    if (op.isInstanceOf[OP_Executable] && op.supportsTarget(target)) {
      for ((data,name) <- op.getGPUMetadata(target).outputs if data.resultType!="void") {
        out.append(op.outputType(Targets.Cuda,name))
        out.append("* ")
        out.append(getSymGPU(name))
        if (op.isInstanceOf[OP_MultiLoop])
          out.append(";\n")
        else {
          out.append(" = ")
          out.append(data.func)
          out.append('(')
          writeInputList(op, data, out)
          out.append(");\n")
          writeMemoryAdd(name, out)
        }
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
    out.append("cudaMemoryMap->insert(pair<void*,list<void*>*>(")
    out.append(getSymGPU(sym))
    out.append(",lastAlloc));\n")
    out.append("lastAlloc = new list<void*>();\n")
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
    //out.append("cudaDeviceSynchronize();\n")
    //out.append("mytic();\n")
    out.append(op.task) //kernel name
    val dims = op.getGPUMetadata(target)
    out.append("<<<") //kernel dimensions
    //grid dimensions
    out.append("dim3")
    out.append('(')
    out.append(dims.dimSizeX.func)
    out.append('(')
    writeInputList(op, dims.dimSizeX, out)
    out.append(')')
    out.append(',')
    out.append(dims.dimSizeY.func)
    out.append('(')
    writeInputList(op, dims.dimSizeY, out)
    out.append(')')
    out.append(',')
    out.append('1')
    out.append(')')
    out.append(',')
    //block dimensions
    out.append("dim3")
    out.append('(')
    out.append(dims.blockSizeX.func)
    out.append('(')
    writeInputList(op, dims.blockSizeX, out)
    out.append(')')
    out.append(',')
    out.append(dims.blockSizeY.func)
    out.append('(')
    writeInputList(op, dims.blockSizeY, out)
    out.append(')')
    out.append(',')
    out.append(dims.blockSizeZ.func)
    out.append('(')
    writeInputList(op, dims.blockSizeZ, out)
    out.append(')')
    out.append(')')
    out.append(',')
    //dynamic shared memory (unused)
    out.append('0')
    out.append(',')
    //stream
    out.append("kernelStream")
    out.append(">>>")

    out.append('(')
    var first = true
    for ((data,name) <- (op.getGPUMetadata(target).outputs) if data.resultType!="void") {
      if(!first) out.append(',')
      out.append('*')
      out.append(getSymGPU(name)) //first kernel inputs are OP outputs
      first = false
    }
    if (!first && (op.getInputs.length > 0 || op.getGPUMetadata(target).temps.length > 0)) out.append(",")
    writeInputs(op, out) //then all op inputs
    writeTemps(op, out) //then all op temporaries
    out.append(");\n")
    //out.append("cudaDeviceSynchronize();\n")
    //out.append("mytoc();\n")
    //out.append("printf(\"%s\\n\",cudaGetErrorString(cudaGetLastError()));")
  }

  protected def writeLibraryCall(op: DeliteOP, out: StringBuilder) {
    if (op.task == null) return //dummy op
    out.append(op.task) //kernel name
    out.append('(')
    assert(op.getOutputs.size == 1) //TODO: what does libCall support?
    for (name <- op.getOutputs) {
      if (op.outputType(name) != "Unit") {
        out.append('*')
        out.append(getSymGPU(name)) //first kernel input is OP output
        out.append(',')
      }
    }
    writeInputs(op, out) //then all op inputs
    //out.append(",kernelStream") // aks TODO: how did this work for library calls?
    out.append(");\n")
  }

  protected def writeMultiKernelCall(op: DeliteOP, out: StringBuilder) {
    if (op.task == null) return //dummy op
    out.append(op.task) //kernel name
    out.append('(')
    var first = true
    for ((data,name) <- (op.getGPUMetadata(target).outputs) if data.resultType!="void") {
      if(!first) out.append(',')
      out.append('&')
      out.append(getSymGPU(name)) //first kernel inputs are OP outputs
      first = false
    }
    if (!first && (op.getInputs.length > 0 || op.getGPUMetadata(target).temps.length > 0)) out.append(",")
    writeInputs(op, out, false) //then all op inputs
    writeTemps(op, out, false) //then all op temporaries
    out.append(");\n")
  }
  
  protected def writeHostKernelCall(op: DeliteOP, out: StringBuilder) {
    if (op.outputType != "Unit") {
      out.append(op.outputType(Targets.C))
      out.append(' ')
      assert(op.getOutputs.size == 1, "C Host kernel does not support multiple outputs")
      out.append(getSymGPU(op.getOutputs.head))
      out.append(" = ")
    }
    out.append(op.task)
    out.append(op.getInputs.map(in => getSymGPU(in._2)).mkString("(",", ",");\n"))
  }

  protected def writeFunctionCall(op: DeliteOP, out: StringBuilder) {
    if (op.outputType != "Unit") {
      out.append(op.outputType(Targets.Cuda))
      out.append(' ')
      // GPU nested block can only return when both condition branches are returned by GPU,
      // meaning that the return object will be a pointer type
      if(op.outputType != "Unit") out.append('*')
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
        if ((op.getMutableInputs contains (input,sym)) && (input.scheduledResource != op.scheduledResource)) {
          out.append(',')
          out.append(getSymCPU(sym))
        }
      }
    }
    out.append(");\n")
  }

  protected def writeGetter(op: DeliteOP, sym: String, location: Int, getterList: ArrayBuffer[String], out: StringBuilder) {
    //get data from CPU
    if (op.outputType(sym) != "Unit") { //skip the variable declaration if return type is "Unit"
      if (!isPrimitiveType(op.outputType(sym))) getterList += getSymCPU(sym)
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

  protected def writeGetterFrees(getterList: ArrayBuffer[String], out: StringBuilder) {
    for (g <- getterList) {
      out.append("env->DeleteLocalRef(")
      out.append(g)
      out.append(");\n")
    }
  }

  protected def writeInputCopy(op: DeliteOP, sym: String, function: String, opType: String, out: StringBuilder, firstAlloc: Boolean) {
    //copy data from CPU to GPU
    if(firstAlloc) {
      out.append(opType)
      out.append("* ")
    }
    out.append(getSymGPU(sym))
    out.append(" = ")
    out.append(function)
    out.append('(')
    out.append("env,") //JNI environment pointer
    out.append(getSymCPU(sym)) //jobject
    out.append(");\n")
    if(!isPrimitiveType(op.outputType(sym))) writeMemoryAdd(sym, out)
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

  protected def writeInputs(op: DeliteOP, out: StringBuilder, dereference: Boolean = true) {
    var first = true
    for ((input,name) <- op.getInputs) {
      if (!first) out.append(',')
      first = false
      if (!isPrimitiveType(input.outputType(name)) && dereference) out.append('*')
      out.append(getSymGPU(name))
    }
  }

  protected def writeTemps(op: DeliteOP, out: StringBuilder, dereference: Boolean = true) {
    for ((temp, name) <- op.getGPUMetadata(target).temps) {
      out.append(',')
      if (dereference) out.append('*')
      out.append(getSymGPU(name))
    }
  }

  protected def ref(op: DeliteOP, sym: String) = {
    if(!isPrimitiveType(op.outputType(sym))) "*"
    else ""
    //if (op.supportsTarget(target) && !isPrimitiveType(op.outputType(target, sym))) "*" //Cuda objects allocated on heap
    //else "" //primitive types and C types allocated on stack
  }

  protected def writeCopyBack(op: DeliteOP, location: Int, out: StringBuilder) {
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
  }

  protected def writeSetters(op: DeliteOP, location: Int, out: StringBuilder) {    
    for (name <- op.getOutputs) {
      var deleteLocalRef = false
      op.getGPUMetadata(target).outputs.find(o => (o._2==name)&&(o._1.resultType!="void")) match {
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
          if(!isPrimitiveType(op.outputType(name))) deleteLocalRef = true
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

      if (deleteLocalRef) {
        out.append("env->DeleteLocalRef(")
        out.append(getSymCPU(name))
        out.append(");\n")
      }
    }
  }

  protected def writeDataFrees(op: DeliteOP, out: StringBuilder, available: ArrayBuffer[(DeliteOP,String)])(implicit aliases: AliasTable[(DeliteOP,String)]) {
    var count = 0
    val freeItem = "freeItem_" + op.id

    def writeFreeInit() {
      out.append("FreeItem ")
      out.append(freeItem)
      out.append(";\n")
      out.append(freeItem)
      out.append(".keys = new list< pair<void*,bool> >();\n")
    }

    def writeFree(sym: String, isPrim: Boolean) {
      if (count == 0) writeFreeInit()
      out.append("pair<void*,bool> ")
      out.append(getSymGPU(sym))
      out.append("_pair(")
      out.append(getSymGPU(sym))
      out.append(",")
      out.append(isPrim) //Do not free this ptr using free() : primitive type pointers points to device memory
      out.append(");\n")
      out.append(freeItem)
      out.append(".keys->push_back(")
      out.append(getSymGPU(sym))
      out.append("_pair);\n")
    }

    def opFreeable(op: DeliteOP, sym: String) = {
      //TODO: Better to make OP_Condition extending OP_Executable?
      (op.isInstanceOf[OP_Executable] || op.isInstanceOf[OP_Condition]) && available.contains(op,sym)
    }

    def outputFreeable(op: DeliteOP, sym: String) = {
      if(op.outputType(sym) == "Unit") false
      else true
    }

    //free temps
    for ((temp, name) <- op.getGPUMetadata(target).temps) {
      writeFree(name,false)
      count += 1
    }

    //free outputs
    for (name <- op.getOutputs if(opFreeable(op,name) && outputFreeable(op,name))) {
      if (op.getConsumers.filter(c => c.getInputs.contains((op,name)) && c.scheduledResource == op.scheduledResource).size == 0) {
        writeFree(name,isPrimitiveType(op.outputType(name)))
        count += 1
      }
    }

    //free inputs
    for ((in,name) <- op.getInputs if(opFreeable(in,name) && outputFreeable(in,name))) {
      var free = true
      if (isPrimitiveType(in.outputType(name)) && (in.scheduledResource!=op.scheduledResource)) free = false
      for ((i,n) <- aliases.get(in,name); c <- i.getConsumers.filter(c => c.getInputs.contains(i,n) && c.scheduledResource == op.scheduledResource)) {
        if (!available.map(_._1).contains(c)) free = false
      }
      if (free) {
        writeFree(name,isPrimitiveType(in.outputType(name)))
        count += 1
      }
    }

    if (count > 0) {
      //sync on kernel stream (if copied back guaranteed to have completed, so don't need sync on d2h stream)
      out.append(freeItem)
      out.append(".event = addHostEvent(kernelStream);\n")
      out.append("freeList->push(")
      out.append(freeItem)
      out.append(");\n")
    }
  }

  protected def makeNestedFunction(op: DeliteOP, location: Int) {
    op match {
      case c: OP_Condition => new CudaGPUConditionGenerator(c, location).makeExecutable()
      case w: OP_While => new CudaGPUWhileGenerator(w, location).makeExecutable()
      case v: OP_Variant => new CudaGPUVariantGenerator(v, location).makeExecutable()
      case err => error("Unrecognized nested OP type: " + err.getClass.getSimpleName)
    }
  }

  protected def writeEventFunction(out: StringBuilder) {
    out.append("void addEvent(cudaStream_t fromStream, cudaStream_t toStream) {\n")
    out.append("cudaEvent_t event;\n")
    out.append("cudaEventCreateWithFlags(&event, cudaEventDisableTiming);\n")
    out.append("cudaEventRecord(event, fromStream);\n");

    out.append("cudaStreamWaitEvent(toStream, event, 0);\n")

    out.append("cudaEventDestroy(event);\n")
    out.append('}')
    out.append('\n')
  }

  protected def writeHostEventFunction(out: StringBuilder) {
    out.append("cudaEvent_t addHostEvent(cudaStream_t stream) {\n")
    out.append("cudaEvent_t event;\n")
    out.append("cudaEventCreateWithFlags(&event, cudaEventDisableTiming | cudaEventBlockingSync);\n")
    out.append("cudaEventRecord(event, stream);\n");
    out.append("return event;\n")
    out.append('}')
    out.append('\n')
  }


}
