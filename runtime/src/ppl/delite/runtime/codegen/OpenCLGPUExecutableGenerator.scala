package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.ops._
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.targets.{OPData, Targets}
import ppl.delite.runtime._
import scheduler.OpList

/* trait OpenCLGPUExecutableGenerator extends GPUExecutableGenerator {

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

  protected def emitCppBody(schedule: OpList, location: Int, syncList: ArrayBuffer[DeliteOP]): String = {
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

  //TODO: almost identical
  protected def writeMemoryAdd(sym: String, out: StringBuilder) {
    out.append("clMemoryMap->insert(pair<void*,list<cl_mem>*>(")
    out.append(getSymGPU(sym))
    out.append(",lastAlloc));\n")
    out.append("lastAlloc = new list<cl_mem>();\n")
  }



} */
