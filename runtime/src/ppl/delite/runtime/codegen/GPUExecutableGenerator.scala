package ppl.delite.runtime.codegen

import java.util.ArrayDeque
import ppl.delite.runtime.graph.ops._
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.targets.{OPData, Targets}

/**
 * Author: Kevin J. Brown
 * Date: Dec 1, 2010
 * Time: 8:18:07 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * Generates optimized DeliteExecutable for a CUDA host thread for a given schedule
 * This generator creates a single executable function for the GPU host
 * The generated code is C++ (and a JNI call) in order to work with CUDA efficiently
 * WARNING: The implementation used here is not efficient for hosting multiple CUDA kernel streams simultaneously
 *
 * This generator makes the following synchronization optimizations:
 * 1) It utilizes 3 CUDA streams (1 for kernels, 1 for h2d transfers, and 1 for d2h transfers), for the maximum possible communication/computation overlap
 * 2) It generates a synchronized getter (h2d transfers) for dependencies from other resources for the first use only
 *    transferred data remains in the GPU device memory for local reuse
 * 3) It generates synchronized result publications (d2h transfers) only for outputs that other resources will need to consume
 *    outputs that the scheduler has restricted to this GPU resource exist only in device memory
 * 4) All kernel launches and device memory transfers are asynchronous with ordering maintained through CUDA events
 *    This allows the host thread to run-ahead as much as possible (keep multiple streams occupied)
 *    The host thread only blocks when data must be transferred to/from other CPU threads
 */

abstract class GPUExecutableGenerator {

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

    //the JNI method
    writeFunctionHeader(location, out)

    //initialize
    writeGlobalsInitializer(out)
    writeJNIInitializer(location, out)

    //execute
    addKernelCalls(schedule, location, new ArrayBuffer[DeliteOP], new ArrayBuffer[DeliteOP], syncList, out)
    out.append('}')
    out.append('\n')

    out.toString
  }

  protected def writeHeader(out: StringBuilder) {
    out.append("#include <jni.h>\n") //jni
    out.append("#include <cuda_runtime.h>\n") //Cuda runtime api
    out.append("#include \"DeliteCuda.cu\"\n") //Delite-Cuda interface for DSL
    out.append("#include \"dsl.h\"\n") //imports all dsl kernels and helper functions
    out.append("#include \"library.h\"\n")
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
  }

  protected def writeJNIInitializer(location: Int, out: StringBuilder) {
    //TODO: this loop should not assume its location is the last
    for (i <- 0 to location) {
      out.append("jclass cls")
      out.append(i)
      out.append(" = env->FindClass(\"")
      out.append(executableName)
      out.append(i)
      out.append("\");\n")
    }
    //add a reference to the singleton of scala.runtime.BoxedUnit for use everywhere required
    out.append("jclass clsBU = env->FindClass(\"scala/runtime/BoxedUnit\");\n")
    out.append("jobject boxedUnit = env->GetStaticObjectField(clsBU, env->GetStaticFieldID(clsBU, \"UNIT\", \"Lscala/runtime/BoxedUnit;\"));\n")
  }

  protected def executableName: String

  protected def addKernelCalls(schedule: ArrayDeque[DeliteOP], location: Int, available: ArrayBuffer[DeliteOP], awaited: ArrayBuffer[DeliteOP], syncList: ArrayBuffer[DeliteOP], out: StringBuilder) {
    //available: list of ops with data currently on gpu, have a "g" symbol
    //awaited: list of ops synchronized with but data only resides on cpu, have a "c" symbol
    val iter = schedule.iterator
    while (iter.hasNext) {
      val op = iter.next
      //add to available & awaited lists
      available += op
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
        val inData = op.cudaMetadata.inputs.getOrElse((input, sym), null)
        if(!available.contains(input)) { //this input does not yet exist on the device
          //add to available list
          available += input
          //write a copy function for objects
          if (inData != null) { //only perform a copy for object types
            addInputCopy = true
            writeInputCopy(input, sym, inData.func, inData.resultType, out)
          }
          else if (isPrimitiveType(input.outputType(sym)))
            writeInputCast(input, sym, out) //if primitive type, simply cast to transform from "c" type into "g" type
          else {
            assert(op.isInstanceOf[OP_Nested],op.id+":cuda metadata for output copy not specified") //object without copy must be for a nested function call
            available -= input //this input doesn't actually reside on GPU
          }
        }
        else if (needsUpdate(op, input, sym)) { //input exists on device but data is old
          //write a new copy function (input must be an object)
          addInputCopy = true
          writeInputCopy(input, sym, inData.func, inData.resultType, out)
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
      if (op.isInstanceOf[OP_Nested])
        writeFunctionCall(op, out)
      else if (op.isInstanceOf[OP_External])
        writeLibraryCall(op, out)
      else
        writeKernelCall(op, out)

      //write the setter
      var addSetter = false
      for (cons <- op.getConsumers) {
        if (cons.scheduledResource != location) addSetter = true
      }
      if (addSetter) {
        syncList += op //add op to list that needs sync generation
        //sync output copy with kernel completion
        out.append("addEvent(kernelStream, d2hStream);\n")
        //write a setter
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
      for ((data,name) <- op.cudaMetadata.outputs) {
        out.append(op.outputType(Targets.Cuda,name))
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
    for ((temp,name) <- op.cudaMetadata.temps) {
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
    out.append(op.task) //kernel name
    val dims = op.cudaMetadata
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
    for ((data,name) <- op.cudaMetadata.outputs) {
      out.append('*')
      out.append(getSymGPU(name)) //first kernel inputs are OP outputs
      out.append(',')
    }
    writeInputs(op, out) //then all op inputs
    writeTemps(op, out) //then all op temporaries
    out.append(");\n")
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

  protected def writeFunctionCall(op: DeliteOP, out: StringBuilder) {
    if (op.outputType != "Unit") {
      out.append(op.outputType(Targets.Cuda))
      out.append(' ')
      out.append(getSymGPU(op.getOutputs.head))
      out.append(" = ")
    }
    out.append(op.task)
    out.append('(')
    var first = true
    for ((input,sym) <- op.getInputs) {
      if (op.cudaMetadata.inputs.contains(input,sym) || isPrimitiveType(input.outputType(sym))) {
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

  protected def writeInputs(op: DeliteOP, out: StringBuilder) {
    var first = true
    for ((input,name) <- op.getInputs) {
      if (!first) out.append(',')
      first = false
      if (!isPrimitiveType(input.outputType(name))) out.append('*')
      out.append(getSymGPU(name))
    }
  }

  protected def writeTemps(op: DeliteOP, out: StringBuilder) {
    for ((temp, name) <- op.cudaMetadata.temps) {
      out.append(',')
      out.append('*')
      out.append(getSymGPU(name))
    }
  }

  protected def writeSetters(op: DeliteOP, location: Int, out: StringBuilder) {
    for ((in,name) <- op.cudaMetadata.inputs.keys) {
      if (op.getMutableInputs.contains(in,name)) {
        //copy any mutated inputs from GPU to CPU
        val inData = op.cudaMetadata.inputs(in,name)
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
      op.cudaMetadata.outputs.find(_._2 == name) match {
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

      if (deleteLocalRef) {
        out.append("env->DeleteLocalRef(")
        out.append(getSymCPU(name))
        out.append(");\n")
      }
    }
  }

  protected def writeDataFrees(op: DeliteOP, out: StringBuilder, available: ArrayBuffer[DeliteOP]) {
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

    def opFreeable(op: DeliteOP) = {
      op.isInstanceOf[OP_Executable] && available.contains(op)
    }

    def outputFreeable(op: DeliteOP, sym: String) = {
      !isPrimitiveType(op.outputType(sym))
    }

    //free temps
    for ((temp, name) <- op.cudaMetadata.temps) {
      writeFree(name)
      count += 1
    }

    //free outputs (?)
    if (opFreeable(op)) {
      for (name <- op.getOutputs if outputFreeable(op, name)) {
        if (op.getConsumers.filter(c => c.getInputs.contains((op,name)) && c.scheduledResource == op.scheduledResource).size == 0) {
          writeFree(name)
          count += 1
        }
      }
    }

    //free inputs (?)
    for ((in,name) <- op.getInputs if(opFreeable(in) && outputFreeable(in, name))) {
      val possible = in.getConsumers.filter(c => c.getInputs.contains((in,name)) && c.scheduledResource == op.scheduledResource)
      var free = true
      for (p <- possible) {
        if (!available.contains(p)) free = false
      }
      if (free) {
        writeFree(name)
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
      case c: OP_Condition => new GPUConditionGenerator(c, location).makeExecutable()
      case w: OP_While => new GPUWhileGenerator(w, location).makeExecutable()
      case v: OP_Variant => new GPUVariantGenerator(v, location).makeExecutable()
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

  protected def getSymCPU(name: String): String = {
    "xC"+name
  }

  protected def getSymGPU(name: String): String = {
    "xG"+name
  }

  protected def getScalaSym(op: DeliteOP, name: String): String = {
    "x"+name
  }

  protected def getJNIType(scalaType: String): String = {
    scalaType match {
      case "Unit" => "void"
      case "Int" => "jint"
      case "Long" => "jlong"
      case "Float" => "jfloat"
      case "Double" => "jdouble"
      case "Boolean" => "jboolean"
      case "Short" => "jshort"
      case "Char" => "jchar"
      case "Byte" => "jbyte"
      case _ => "jobject"//all other types are objects
    }
  }

  protected def getJNIArgType(scalaType: String): String = {
    scalaType match {
      case "Unit" => "Lscala/runtime/BoxedUnit;"
      case "Int" => "I"
      case "Long" => "J"
      case "Float" => "F"
      case "Double" => "D"
      case "Boolean" => "Z"
      case "Short" => "S"
      case "Char" => "C"
      case "Byte" => "B"
      case array if array.startsWith("Array[") => "[" + getJNIArgType(array.slice(6,array.length-1))
      case _ => { //all other types are objects
        var objectType = scalaType.replace('.','/')
        if (objectType.indexOf('[') != -1) objectType = objectType.substring(0, objectType.indexOf('[')) //erasure
        "L"+objectType+";" //'L' + fully qualified type + ';'
      }
    }
  }

  protected def getJNIOutputType(scalaType: String): String = {
    scalaType match {
      case "Unit" => "V"
      case "Int" => "I"
      case "Long" => "J"
      case "Float" => "F"
      case "Double" => "D"
      case "Boolean" => "Z"
      case "Short" => "S"
      case "Char" => "C"
      case "Byte" => "B"
      case array if array.startsWith("Array[") => "[" + getJNIOutputType(array.slice(6,array.length-1))
      case _ => { //all other types are objects
        var objectType = scalaType.replace('.','/')
        if (objectType.indexOf('[') != -1) objectType = objectType.substring(0, objectType.indexOf('[')) //erasure
        "L"+objectType+";" //'L' + fully qualified type + ';'
      }
    }
  }

  protected def getJNIFuncType(scalaType: String): String = scalaType match {
    case "Unit" => "Void"
    case "Int" => "Int"
    case "Long" => "Long"
    case "Float" => "Float"
    case "Double" => "Double"
    case "Boolean" => "Boolean"
    case "Short" => "Short"
    case "Char" => "Char"
    case "Byte" => "Byte"
    case _ => "Object"//all other types are objects
  }

  protected def getCPrimitiveType(scalaType: String): String = scalaType match {
    case "Unit" => "void"
    case "Int" => "int"
    case "Long" => "long"
    case "Float" => "float"
    case "Double" => "double"
    case "Boolean" => "bool"
    case "Short" => "short"
    case "Char" => "char"
    case "Byte" => "char"
    case other => error(other + " is not a primitive type")
  }

  protected def isPrimitiveType(scalaType: String): Boolean = scalaType match {
    case "Unit" => true
    case "Int" => true
    case "Long" => true
    case "Float" => true
    case "Double" => true
    case "Boolean" => true
    case "Short" => true
    case "Char" => true
    case "Byte" => true
    case _ => false
  }

}

abstract class GPUScalaExecutableGenerator extends ExecutableGenerator {

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
    out.append("System.load(\"")
    out.append(CudaCompile.binCacheHome)
    out.append("cudaHost.so\")\n")

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
