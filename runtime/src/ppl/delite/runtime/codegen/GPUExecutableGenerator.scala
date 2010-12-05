package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops.DeliteOP
import ppl.delite.runtime.scheduler.PartialSchedule
import java.util.{ArrayDeque, ArrayList}

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

  def makeExecutable(schedule: PartialSchedule) {
    assert(schedule.resources.length == 1) //this implementation does not attempt to create an Executable than can efficiently handle hosting multiple kernel streams

    val location = schedule.resources(0).peek.scheduledResource //look up location id for this GPU device resource
    val syncList = new ArrayList[DeliteOP] //list of ops needing sync added

    val cppSource = emitCpp(schedule.resources(0), location, syncList)
    CudaCompile.addSource(cppSource)

    val scalaSource = emitScala(location, syncList)
    ScalaCompile.addSource(scalaSource)
  }

  private def emitCpp(schedule: ArrayDeque[DeliteOP], location: Int, syncList: ArrayList[DeliteOP]): String = {
    val out = new StringBuilder //the output string

    //the header
    writeHeader(out)

    //the JNI method
    out.append("JNIEXPORT void JNICALL Java_hostGPU(JNIEnv* env, jobject object) {\n")

    //initialize
    writeInitializer(out)

    //execute
    addKernelCalls(schedule, location, syncList, out)
    out.append('}')
    out.append('\n')

    //the event function
    writeEventFunction(out)

    out.toString
  }

  private def writeHeader(out: StringBuilder) {
    out.append("include <jni.h>\n") //jni
    out.append("include <cuda_runtime.h>\n") //cuda runtime api
    out.append("include <iostream>\n") //io
  }

  private def writeInitializer(out: StringBuilder) {
    out.append("cudaStream_t kernelStream;\n")
    out.append("cudaStreamCreate(&kernelStream);\n")
    out.append("cudaStream_t h2dStream;\n")
    out.append("cudaStreamCreate(&h2dStream);\n")
    out.append("cudaStream_t d2hStream;\n")
    out.append("cudaStreamCreate(&d2hStream);\n")
  }

  private def addKernelCalls(schedule: ArrayDeque[DeliteOP], location: Int, syncList: ArrayList[DeliteOP], out: StringBuilder) {
    val available = new ArrayList[DeliteOP] //ops that have existing local symbols
    val iter = schedule.iterator
    while (iter.hasNext) {
      val op = iter.next
      //add to available list
      available.add(op)
      //get dependencies
      var addGetter = false
      for (dep <- op.getDependencies) { //foreach dependency
        if(!available.contains(dep)) {//this dependency does not yet exist on this resource
          addGetter = true
          //add to available list
          available.add(dep)
          //write a getter
          writeGetter(dep, out)
        }
      }
      if (addGetter) { //if a h2d data transfer occurred
        //sync kernel launch with completion of last input copy
        out.append("addEvent(h2dStream, kernelStream);\n")
      }
      //write the output allocation
      writeOutputAlloc(op, out)
      //write the function call
      writeFunctionCall(op, out)
      //write the setter
      var addSetter = false
      for (cons <- op.getConsumers) {
        if (cons.scheduledResource != location) addSetter = true
      }
      if (addSetter) {
        syncList.add(op) //add op to list that needs sync generation
        //sync output copy with kernel completion
        out.append("addEvent(kernelStream, d2hStream);\n")
        //write a setter
        writeSetter(op, out)
      }
      //TODO: should free device memory when possible
    }
  }

  private def writeOutputAlloc(op: DeliteOP, out: StringBuilder) {
    out.append(op.outputType)
    out.append(' ')
    out.append(getSymGPU(op))
    out.append(" = ")
    out.append("outputAlloc();\n") //TODO: need to obtain this from OP
  }

  private def writeFunctionCall(op: DeliteOP, out: StringBuilder) {
    out.append(op.task) //kernel name

    out.append("<<<") //kernel dimensions
    out.append('1') //grid dimensions //TODO: need to obtain this from OP
    out.append(',')
    out.append('1') //block dimensions //TODO: need to obtain this from OP
    out.append(',')
    out.append('0') //dynamically allocated shared memory, not used
    out.append(',')
    out.append("kernelStream") //stream
    out.append(">>>")

    out.append('(')
    out.append(getSymGPU(op)) //first kernel intput is OP output
    for (input <- op.getInputs) { //then all op inputs
      out.append(',')
      out.append(getSymGPU(input))
    }
    out.append(");\n")
  }

  private def writeGetter(op: DeliteOP, out: StringBuilder) {
    //get data from CPU
    out.append(op.outputType)
    out.append(' ')
    out.append(getSymCPU(op))
    out.append(" = get")
    out.append(op.id)
    out.append(';')
    out.append('\n')

    //copy data from CPU to GPU
    out.append(op.outputType)
    out.append(' ')
    out.append(getSymGPU(op))
    out.append(" = ")
    out.append("inputAlloc(")
    out.append(getSymCPU(op))
    out.append(");\n")
  }

  private def writeSetter(op: DeliteOP, out: StringBuilder) {
    //copy data from GPU to CPU
    out.append(op.outputType)
    out.append(' ')
    out.append(getSymCPU(op))
    out.append(" = ")
    out.append("outputSet(")
    out.append(getSymGPU(op))
    out.append(");\n")

    //set data as available to CPU
    out.append("set")
    out.append(op.id)
    out.append('(')
    out.append(getSymCPU(op))
    out.append(");\n")
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

  private def emitScala(location: Int, syncList: ArrayList[DeliteOP]): String = {
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
    out.append("System.loadLibrary(\"cudaHost.so\")\n")

    //the sync methods/objects
    ExecutableGenerator.addSync(syncList, out)

    //an accessor method for the object
    ExecutableGenerator.addAccessor(out)

    //the footer
    out.append('}')
    out.append('\n')

    out.toString
  }

  private def getSymCPU(op: DeliteOP): String = {
    "xC"+op.id
  }

  private def getSymGPU(op: DeliteOP): String = {
    "xG"+op.id
  }

}
