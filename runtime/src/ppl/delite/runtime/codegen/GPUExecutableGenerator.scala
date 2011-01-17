package ppl.delite.runtime.codegen

import ppl.delite.runtime.scheduler.PartialSchedule
import java.util.{ArrayDeque, ArrayList}
import java.io.File
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.graph.ops._

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

object GPUExecutableGenerator {

  def makeExecutable(schedule: PartialSchedule, kernelPath: String) {
    assert(schedule.resources.length == 1) //this implementation does not attempt to create an Executable than can efficiently handle hosting multiple kernel streams

    val location = schedule.resources(0).peek.scheduledResource //look up location id for this GPU device resource
    val syncList = new ArrayList[DeliteOP] //list of ops needing sync added

    val cppSource = emitCpp(schedule.resources(0), location, syncList)
    CudaCompile.addSource(cppSource)

    val scalaSource = emitScala(location, syncList, kernelPath)
    ScalaCompile.addSource(scalaSource)
  }

  private def emitCpp(schedule: ArrayDeque[DeliteOP], location: Int, syncList: ArrayList[DeliteOP]): String = {
    val out = new StringBuilder //the output string

    //the header
    writeHeader(out)

    //write stream globals (used for linking)
    writeGlobalStreams(out)

    //the event function
    writeEventFunction(out)

    //the JNI method
    writeFunctionHeader(location, out)

    //initialize
    writeJNIInitializer(location, out)
    writeStreamInitializer(out)

    //execute
    addKernelCalls(schedule, location, syncList, out)
    out.append('}')
    out.append('\n')

    out.toString
  }

  private def writeHeader(out: StringBuilder) {
    out.append("#include <jni.h>\n") //jni
    out.append("#include <cuda_runtime.h>\n") //Cuda runtime api
    out.append("#include \"DeliteCuda.cu\"\n") //Delite-Cuda interface for DSL
    out.append("#include \"dsl.h\"\n") //imports all dsl kernels and helper functions
  }

  private def writeFunctionHeader(location: Int, out: StringBuilder) {
    val function = "JNIEXPORT void JNICALL Java_Executable" + location + "_00024_hostGPU(JNIEnv* env, jobject object)"
    out.append("extern \"C\" ") //necessary because of JNI
    out.append(function)
    out.append(";\n")
    out.append(function)
    out.append(" {\n")
  }

  private def writeGlobalStreams(out: StringBuilder) {
    out.append("cudaStream_t kernelStream;\n")
    out.append("cudaStream_t h2dStream;\n")
    out.append("cudaStream_t d2hStream;\n")
  }

  private def writeStreamInitializer(out: StringBuilder) {
    out.append("cudaStreamCreate(&kernelStream);\n")
    out.append("cudaStreamCreate(&h2dStream);\n")
    out.append("cudaStreamCreate(&d2hStream);\n")
  }

  private def writeJNIInitializer(location: Int, out: StringBuilder) {
    //TODO: this loop should not assume its location is the last
    for (i <- 0 to location) {
      out.append("jclass cls")
      out.append(i)
      out.append(" = env->FindClass(\"Executable")
      out.append(i)
      out.append("\");\n")
    }
    //add a reference to the singleton of scala.runtime.BoxedUnit for use everywhere required
    out.append("jclass clsBU = env->FindClass(\"scala/runtime/BoxedUnit\");\n")
    out.append("jobject boxedUnit = env->GetStaticObjectField(clsBU, env->GetStaticFieldID(clsBU, \"UNIT\", \"Lscala/runtime/BoxedUnit;\"));\n")
  }

  private def addKernelCalls(schedule: ArrayDeque[DeliteOP], location: Int, syncList: ArrayList[DeliteOP], out: StringBuilder) {
    val available = new ArrayList[DeliteOP] //ops with local data (have a "g" symbol)
    val awaited = new ArrayList[DeliteOP] //ops that have been synchronized with (have a "c" symbol)
    val iter = schedule.iterator
    while (iter.hasNext) {
      val op = iter.next
      //add to available & awaited lists
      available.add(op)
      awaited.add(op)
      //get all dependencies
      for (dep <- op.getDependencies) { //foreach dependency
        if(!awaited.contains(dep)) { //this dependency does not yet exist for this resource
          awaited.add(dep)
          writeGetter(dep, out) //get to synchronize
        }
      }
      //get kernel inputs (dependencies that could require a memory transfer)
      var addInputCopy = false
      val mutatedInputs = op.getDependencies.flatMap(_.getMutableInputs).intersect(op.getInputs)
      val inputCopies = op.cudaMetadata.inputs.iterator //list of inputs that have a copy function
      for (input <- op.getInputs) { //foreach input
        val inData = if (getJNIType(input.outputType) == "jobject") inputCopies.next else null
        if(!available.contains(input)) { //this input does not yet exist on the device
          //add to available list
          available.add(input)
          //write a copy function for objects
          if (getJNIType(input.outputType) == "jobject") { //only perform a copy for object types
            addInputCopy = true
            writeInputCopy(input, inData.func, inData.resultType, out)
          }
          else writeInputCast(input, out) //if primitive type, simply cast to transform from "c" type into "g" type
        }
        else if (mutatedInputs.contains(input)) { //input exists on device but data is old
          //write a new copy function (input must be an object)
          addInputCopy = true
          writeInputCopy(input, inData.func, inData.resultType, out)
        }
      }
      if (addInputCopy) { //if a h2d data transfer occurred
        //sync kernel launch with completion of last input copy
        out.append("addEvent(h2dStream, kernelStream);\n")
      }

      if (op.isInstanceOf[OP_Control])
        writeControl(op, out)
      else {
        //write the temporary allocations
        writeTempAllocs(op, out)
        //write the output allocation
        writeOutputAlloc(op, out)
        //write the function call or library call
        if (op.cudaMetadata.libCall == null) writeFunctionCall(op, out) else writeLibraryCall(op, out)
      }

      //write the setter
      var addSetter = false
      for (cons <- op.getConsumers) {
        if (cons.scheduledResource != location) addSetter = true
      }
      if (addSetter) {
        syncList.add(op) //add op to list that needs sync generation
        //sync output copy with kernel completion
        if (!op.isInstanceOf[OP_Control]) out.append("addEvent(kernelStream, d2hStream);\n")
        //write a setter
        writeSetter(op, location, out)
      }
      //TODO: should free device memory when possible
    }
  }

  private def writeOutputAlloc(op: DeliteOP, out: StringBuilder) {
    if (op.outputType != "Unit") {
      out.append(op.outputType(Targets.Cuda))
      out.append(' ')
      out.append(getSymGPU(op))
      out.append(" = ")
      out.append(op.cudaMetadata.output.func)
      out.append('(')
      writeInputList(op, "output", out)
      out.append(");\n")
    }
  }

  private def writeTempAllocs(op: DeliteOP, out: StringBuilder) {
    val iter = op.cudaMetadata.tempOps.iterator
    for (temp <- op.cudaMetadata.temps) {
      val tempOp = iter.next
      out.append(temp.resultType)
      out.append(' ')
      out.append(getSymGPU(tempOp))
      out.append(" = ")
      out.append(temp.func)
      out.append('(')
      var first = true
      for (in <- temp.inputs) {
        if (!first) out.append(',')
        first = false
        out.append(getSymGPU(in))
      }
      out.append(");\n")
    }
  }

  private def writeInputList(op: DeliteOP, field: String, out: StringBuilder) {
    val data = op.cudaMetadata(field)
    var first = true
    for (in <- data.inputs) {
      if (!first) out.append(',')
      first = false
      out.append(getSymGPU(in))
    }
  }

  private def writeFunctionCall(op: DeliteOP, out: StringBuilder) {
    out.append(op.task) //kernel name
    val dims = op.cudaMetadata
    out.append("<<<") //kernel dimensions
    //grid dimensions
    out.append("dim3")
    out.append('(')
    out.append(dims.dimSizeX.func)
    out.append('(')
    writeInputList(op, "gpuDimSizeX", out)
    out.append(')')
    out.append(',')
    out.append(dims.dimSizeY.func)
    out.append('(')
    writeInputList(op, "gpuDimSizeY", out)
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
    writeInputList(op, "gpuBlockSizeX", out)
    out.append(')')
    out.append(',')
    out.append(dims.blockSizeY.func)
    out.append('(')
    writeInputList(op, "gpuBlockSizeY", out)
    out.append(')')
    out.append(',')
    out.append(dims.blockSizeZ.func)
    out.append('(')
    writeInputList(op, "gpuBlockSizeZ", out)
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
    if (op.outputType != "Unit") {
      out.append(getSymGPU(op)) //first kernel input is OP output
      out.append(',')
    }
    writeInputs(op, out) //then all op inputs
    writeTemps(op, out) //then all op temporaries
    out.append(");\n")
  }

  private def writeLibraryCall(op: DeliteOP, out: StringBuilder) {
    out.append(op.cudaMetadata.libCall)
    out.append('(')
    if (op.outputType != "Unit") {
      out.append(getSymGPU(op)) //first kernel input is OP output
      out.append(',')
    }
    writeInputs(op, out) //then all op inputs
    out.append(",kernelStream")
    out.append(");\n")
  }

  private def writeGetter(op: DeliteOP, out: StringBuilder) {
    //get data from CPU
    if (op.outputType != "Unit") { //skip the variable declaration if return type is "Unit"
      out.append(getJNIType(op.outputType))
      out.append(' ')
      out.append(getSymCPU(op))
      out.append(" = ")
    }
    out.append("env->CallStatic")
    out.append(getJNIFuncType(op.outputType))
    out.append("Method(cls")
    out.append(op.scheduledResource)
    out.append(",env->GetStaticMethodID(cls")
    out.append(op.scheduledResource)
    out.append(",\"get")
    out.append(op.id) //scala get method
    out.append("\",\"()")
    out.append(getJNIOutputType(op.outputType))
    out.append("\"));\n")
  }

  private def writeInputCopy(op: DeliteOP, function: String, opType: String, out: StringBuilder) {
    //copy data from CPU to GPU
    out.append(opType)
    out.append(' ')
    out.append(getSymGPU(op))
    out.append(" = ")
    out.append(function)
    out.append('(')
    out.append("env,") //JNI environment pointer
    out.append(getSymCPU(op)) //jobject
    out.append(");\n")
  }

  private def writeInputCast(op: DeliteOP, out: StringBuilder) {
    out.append(getCPrimitiveType(op.outputType)) //C primitive
    out.append(' ')
    out.append(getSymGPU(op))
    out.append(" = ")
    out.append('(') //cast
    out.append(getCPrimitiveType(op.outputType)) //C primitive
    out.append(')')
    out.append(getSymCPU(op)) //J primitive
    out.append(';')
    out.append('\n')
  }

  private def writeInputs(op: DeliteOP, out: StringBuilder) {
    var first = true
    for (input <- op.getInputs) {
      if (!first) out.append(',')
      first = false
      out.append(getSymGPU(input))
    }
  }

  private def writeTemps(op: DeliteOP, out: StringBuilder) {
    for (temp <- op.cudaMetadata.tempOps) {
      out.append(',')
      out.append(getSymGPU(temp))
    }
  }

  private def writeSetter(op: DeliteOP, location: Int, out: StringBuilder) {
    val iter = op.cudaMetadata.inputs.iterator
    for (in <- op.getInputs; if (getJNIType(in.outputType) == "jobject")) {
      val inData = iter.next
      if (op.getMutableInputs.contains(in)) {
        //copy any mutated inputs from GPU to CPU
        out.append(inData.funcReturn)
        out.append("(env,") //JNI environment pointer
        out.append(getSymCPU(in)) //jobject
        out.append(getSymGPU(in)) //C++ object
        out.append(");\n")
      }
    }

    if (op.outputType != "Unit") {
      //copy output from GPU to CPU
      out.append(getJNIType(op.outputType)) //jobject
      out.append(' ')
      out.append(getSymCPU(op))
      out.append(" = ")
      out.append(op.cudaMetadata.output.funcReturn)
      out.append('(')
      out.append("env,") //JNI environment pointer
      out.append(getSymGPU(op)) //C++ object
      out.append(");\n")
    }

    //set data as available to CPU
    out.append("env->CallStaticVoidMethod(cls")
    out.append(location)
    out.append(",env->GetStaticMethodID(cls")
    out.append(location)
    out.append(",\"set")
    out.append(op.id)
    out.append("\",\"(")        
    out.append(getJNIArgType(op.outputType))
    out.append(")V\"),")
    if (op.outputType == "Unit") out.append("boxedUnit") else out.append(getSymCPU(op))
    out.append(");\n")
  }

  private def writeControl(op: DeliteOP, out: StringBuilder) {
    op match {
      case beginCond: OP_BeginCondition => {
        out.append("if (")
        out.append(getSymCPU(beginCond.predicate))
        out.append(") {\n")
      }
      case beginElse: OP_BeginElse => {
        out.append("} else {\n")
      }
      case endCond: OP_EndCondition => {
        out.append('}')
        out.append('\n')
      }
      case beginWhile: OP_BeginWhile => {
        val sym = getSymCPU(op).dropRight(1) //the base sym for this while construct
        out.append("bool ")
        out.append(sym)
        out.append("p = ")
        out.append(getSymCPU(beginWhile.predicate))
        out.append(";\n")

        out.append("while (")
        out.append(sym)
        out.append("p) {\n")
      }
      case endWhile: OP_EndWhile => {
        out.append(getSymCPU(op))
        out.append("p = ")
        out.append(getSymCPU(endWhile.predicate))
        out.append(";\n}\n")
      }
    }
  }

  private def writeEventFunction(out: StringBuilder) {
    out.append("void addEvent(cudaStream_t fromStream, cudaStream_t toStream) {\n")
    out.append("cudaEvent_t event;\n")
    out.append("cudaEventCreateWithFlags(&event, cudaEventDisableTiming);\n")
    out.append("cudaEventRecord(event, fromStream);\n");

    out.append("cudaStreamWaitEvent(toStream, event, 0);\n")

    out.append("cudaEventDestroy(event);\n")
    out.append('}')
    out.append('\n')
  }

  private def emitScala(location: Int, syncList: ArrayList[DeliteOP], kernelPath: String): String = {
    val out = new StringBuilder

    //the header
    ExecutableGenerator.writeHeader(out, location, "")

    //the run method
    out.append("def run() {\n")
    out.append("hostGPU\n")
    out.append('}')
    out.append('\n')

    //the native method
    out.append("@native def hostGPU : Unit\n")

    //link the native code upon object creation
    val sep = java.io.File.separator
    out.append("System.load(\"")
    out.append(kernelPath)
    out.append(sep)
    out.append("cuda")
    out.append(sep)
    out.append("cudaHost.so\")\n")

    //the sync methods/objects
    ExecutableGenerator.addSync(syncList, out)
    writeOuterSet(syncList, out) //helper set methods for JNI calls to access

    //an accessor method for the object
    ExecutableGenerator.addAccessor(out)

    //the footer
    out.append('}')
    out.append('\n')

    out.toString
  }

  private def writeOuterSet(list: ArrayList[DeliteOP], out: StringBuilder) {
    val iter = list.iterator
    while (iter.hasNext) {
      val op = iter.next
      out.append("def set")
      out.append(op.id)
      out.append("(result : ")
      out.append(op.outputType)
      out.append(") = ")
      out.append(ExecutableGenerator.getSync(op))
      out.append(".set(result)\n")
    }
  }

  private def getSymCPU(op: DeliteOP): String = {
    "xC"+op.id
  }

  private def getSymGPU(op: DeliteOP): String = {
    "xG"+op.id
  }

  private def getJNIType(scalaType: String): String = {
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

  private def getJNIArgType(scalaType: String): String = {
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
      case _ => { //all other types are objects
        var objectType = scalaType.replace('.','/')
        if (objectType.indexOf('[') != -1) objectType = objectType.substring(0, objectType.indexOf('[')) //erasure
        "L"+objectType+";" //'L' + fully qualified type + ';'
      }
    } //TODO: this does not handle array types properly
  }

  private def getJNIOutputType(scalaType: String): String = {
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
      case _ => { //all other types are objects
        var objectType = scalaType.replace('.','/')
        if (objectType.indexOf('[') != -1) objectType = objectType.substring(0, objectType.indexOf('[')) //erasure
        "L"+objectType+";" //'L' + fully qualified type + ';'
      }
    } //TODO: this does not handle array types properly
  }

  private def getJNIFuncType(scalaType: String): String = scalaType match {
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

  private def getCPrimitiveType(scalaType: String): String = scalaType match {
    case "Unit" => "void"
    case "Int" => "int"
    case "Long" => "long"
    case "Float" => "float"
    case "Double" => "double"
    case "Boolean" => "bool"
    case "Short" => "short"
    case "Char" => "char"
    case "Byte" => "byte"
    case other => error(other + " is not a primitive type")
  }

}
