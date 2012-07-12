package ppl.delite.runtime.codegen.kernels.cpp

import ppl.delite.runtime.graph.ops.OP_MultiLoop
import ppl.delite.runtime.codegen.kernels.{MultiLoop_SMP_Array_Header_Generator, MultiLoop_SMP_Array_Generator}
import ppl.delite.runtime.codegen.CppCompile
import ppl.delite.runtime.graph.targets.Targets
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.DeliteTaskGraph

object CppMultiLoopGenerator {
  def makeChunks(op: OP_MultiLoop, numChunks: Int, kernelPath: String) = {
    for (idx <- 0 until numChunks) yield {
      val chunk = if (idx == 0) op else op.chunk(idx)
      (new CppMultiLoopGenerator(chunk, op, idx, numChunks, kernelPath)).makeChunk()
      chunk
    }
  }
}

class CppMultiLoopGenerator(val op: OP_MultiLoop, val master: OP_MultiLoop, val chunkIdx: Int, val numChunks: Int, val kernelPath: String) extends MultiLoop_SMP_Array_Generator {

  protected val headerObject = "head"
  protected val closure = "head.closure"

  protected def addSource(source: String, name: String) = CppCompile.addSource(source, name)

  protected def writeHeader() {
    out.append("class ")
    out.append(kernelName)
    out.append(" {\n")
  }

  protected def writeFooter(){
    out.append("}\n")
  }

  protected def writeKernelHeader() {
    out.append("static ")
    out.append(op.outputType(Targets.Cpp))
    out.append(" apply(")
    out.append(op.getInputs.head._1.outputType(Targets.Cpp))
    out.append(' ')
    out.append(headerObject)
    out.append(") {\n")
  }

  protected def writeKernelFooter() {
    out.append("}\n")
  }

  protected def returnResult(result: String) {
    out.append("return "+result+";\n")
  }

  protected def calculateRange(): (String,String) = {
    out.append("int size = "+closure+".size;\n")
    out.append("int start = size*"+chunkIdx+"/"+numChunks+";\n")
    out.append("int end = size*"+(chunkIdx+1)+"/"+numChunks+";\n")
    ("start","end")
  }

  protected def allocateOutput(): String = {
    out.append(master.outputType(Targets.Cpp)+" out = "+headerObject+".out;\n")
    "out"
  }

  protected def processRange(outputSym: String, start: String, end: String) = {
    out.append(master.outputType(Targets.Cpp)+" acc = "+closure+".processRange("+outputSym+","+start+","+end+");\n")
    "acc"
  }

  protected def combine(acc: String, neighbor: String) {
    out.append(closure+".combine("+acc+", "+neighbor+");\n")
  }

  protected def postProcess(acc: String) {
    out.append(closure+".postProcess("+acc+");\n")
  }

  protected def postProcInit(acc: String) {
    out.append(closure+".postProcInit("+acc+");\n")
  }

  protected def postCombine(acc: String, neighbor: String) {
    out.append(closure+".postCombine("+acc+", "+neighbor+");\n")
  }

  protected def finalize(acc: String) {
    out.append(closure+".finalize("+acc+");\n")
  }

  protected def set(syncObject: String, idx: Int, value: String) {
    out.append(headerObject+".set"+syncObject+idx+"("+value+");\n")
  }

  protected def get(syncObject: String, idx: Int) = {
    out.append(master.outputType(Targets.Cpp)+" neighbor"+syncObject+idx+" = "+headerObject+".get"+syncObject+idx+"();\n")
    "neighbor"+syncObject+idx
  }

  //TODO: add profiling for c++ kernels
  protected def beginProfile() { }

  protected def endProfile() {  }

  protected def kernelName = {
    "MultiLoop_SMP_Array_" + master.id + "_Chunk_" + chunkIdx
  }

}


class CppMultiLoopHeaderGenerator(val op: OP_MultiLoop, val numChunks: Int, val graph: DeliteTaskGraph) extends MultiLoop_SMP_Array_Header_Generator {

  protected def addSource(source: String, name: String) = CppCompile.addSource(source, name)

  protected def writeHeader() {
    writeClassHeader()
    writeStatic()
    writeInstance()
  }

  protected def writeFooter() {
    initSync()
    out.append("}\n")
  }

  protected def writeClassHeader() {
    out.append("class ")
    out.append(kernelName)
    out.append(" {\n")
  }

  protected def writeStatic() {
    out.append("public: \n")
    out.append("static " + kernelName + " apply(")
    var inIdx = 0
    var first = true
    for ((input, name) <- op.getInputs) {
      if (!first) out.append(", ")
      first = false
      out.append(input.outputType(Targets.Cpp, name))
      out.append(" in")
      out.append(inIdx)
      inIdx += 1
    }
    out.append(") = new ")
    out.append(kernelName)
    out.append("(")
    for (i <- 0 until inIdx) {
      if (i > 0) out.append(", ")
      out.append("in")
      out.append(i)
    }
    out.append(");\n")
  }

  protected def writeInstance() {
    out.append(op.function + " closure;\n")
    out.append(op.outputType(Targets.Cpp) + " out;\n")

    out.append(kernelName)
    out.append("(")
    var inIdx = 0
    var first = true
    for ((input, name) <- op.getInputs) {
      if (!first) out.append(", ")
      first = false
      out.append(input.outputType(Targets.Cpp, name))
      out.append(" in")
      out.append(inIdx)
      inIdx += 1
    }
    out.append(") {\n")

    out.append("closure = ")
    out.append(op.function)
    out.append("(")
    for (i <- 0 until inIdx) {
      if (i > 0) out.append(", ")
      out.append("in")
      out.append(i)
    }
    out.append(")\n")

    out.append("out = closure.alloc();\n")
    out.append("initSync()")
    out.append("}\n")
  }

  protected val syncList = new ArrayBuffer[String]

  protected def writeSync(key: String) {
    syncList += key //need a way to initialize these fields in C++
    val outputType = op.outputType(Targets.Cpp)

    out.append("pthread_mutex_t lock"+key+";\n")
    out.append("pthread_cond_t cond"+key+";\n")

    out.append("bool notReady;\n")
    out.append(outputType+ " _result"+key+";\n")

    out.append(outputType+" get"+key+"(){\n")
    out.append("while(notReady"+key+") {\n")
    out.append("pthread_mutex_lock(lock"+key+"); pthread_cond_wait(cond"+key+", lock"+key+");\n")
    out.append("}\n")
    out.append("return _result"+key+";\n")
    out.append("}\n")

    out.append("void set"+key+"("+outputType+" result) {\n")
    out.append("pthread_mutex_lock(lock"+key+");\n")
    out.append("_result"+key + " = result;\n")
    out.append("notReady"+key+" = false;\n")
    out.append("pthread_cond_broadcast(cond"+key+");\n")
    out.append("pthread_mutex_unlock(lock"+key+");\n")
    out.append("}\n")
  }

  protected def initSync() {
    out.append("void initSync() {\n")
    for (key <- syncList) {
      out.append("lock"+key+" = PTHREAD_MUTEX_INITIALIZER;\n")
      out.append("cond"+key+" = PTHREAD_COND_INITIALIZER;\n")
      out.append("notReady"+key+ " = true;\n")
    }
    out.append("}\n")
  }

  protected def kernelName() = {
    "MultiLoop_SMP_Array_Header_" + op.id
  }

}
