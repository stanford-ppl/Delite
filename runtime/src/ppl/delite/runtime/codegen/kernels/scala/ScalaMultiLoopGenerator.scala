package ppl.delite.runtime.codegen.kernels.scala

import ppl.delite.runtime.codegen.{ScalaExecutableGenerator, ScalaCompile}
import ppl.delite.runtime.graph.ops.OP_MultiLoop
import ppl.delite.runtime.codegen.kernels.{MultiLoop_SMP_Array_Header_Generator, MultiLoop_SMP_Array_Generator}
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.Config

object ScalaMultiLoopGenerator {
  def makeChunks(op: OP_MultiLoop, numChunks: Int, kernelPath: String) = {
    for (idx <- 0 until numChunks) yield {
      val chunk = if (idx == 0) op else op.chunk(idx)
      (new ScalaMultiLoopGenerator(chunk, op, idx, numChunks, kernelPath)).makeChunk()
      chunk
    }
  }
}

class ScalaMultiLoopGenerator(val op: OP_MultiLoop, val master: OP_MultiLoop, val chunkIdx: Int, val numChunks: Int, val kernelPath: String) extends MultiLoop_SMP_Array_Generator {

  protected val headerObject = "head"
  protected val closure = "head.closure"

  protected def addSource(source: String, name: String) = ScalaCompile.addSource(source, name)

  protected def writeHeader() {
    out.append("import ppl.delite.runtime.profiler.PerformanceTimer\n")
    ScalaExecutableGenerator.writePath(kernelPath, out)
    out.append("object ")
    out.append(kernelName)
    out.append(" {\n")
  }

  protected def writeFooter(){
    out.append("}\n")
  }

  protected def writeKernelHeader() {
    out.append("def apply("+headerObject+": ")
    out.append(op.getInputs.head._1.outputType)
    out.append("): ")
    out.append(op.outputType)
    out.append(" = {\n")
  }

  protected def writeKernelFooter() {
    out.append("}\n")
  }

  protected def returnResult(result: String) {
    out.append(result+"\n")
  }

  protected def dynamicScheduler(outputSym: String): String = {
    out.append("println(\"dynamic Schduler\")\n")
    out.append("var start = "+headerObject+".getOffset()\n")
    out.append("val dChunkSize = "+headerObject + ".dynamicChunkSize\n")
    out.append("val numDynamicChunks = "+headerObject + ".numDynamicChunks\n")

    out.append("while(start < "+closure+".loopSize){\n")
    out.append("println(\"dynamic loop1\")\n")

    //Maybe you can make this faster?  Can you go past loop Size?  What happens? ASK KEVIN
    out.append("val end = if((start+dChunkSize) < "+closure+".loopSize) (start+dChunkSize) else "+closure+".loopSize\n")
    out.append("val accDynamic = "+closure+".processRange("+outputSym+",start,end)\n")
    out.append("val dChunkIdx = start/dChunkSize\n")
    out.append(headerObject+".dynamicSet(dChunkIdx,accDynamic)\n")

    out.append("start = "+headerObject+".getOffset()\n")
    out.append("}\n")
    out.append("val dynamicChunksPerThread = (numDynamicChunks+"+numChunks+"-1)/"+numChunks+"\n")
    out.append("val dIndex = ("+chunkIdx+" * dynamicChunksPerThread)\n")
    out.append("val acc = ")
    out.append(headerObject+".dynamicGet(dIndex)\n")
    "acc"
  }
  protected def dynamicCombine(acc: String) = {
    out.append("var i = 1+dIndex\n")
    out.append("while((i < (dynamicChunksPerThread+dIndex)) && (i < "+closure+".loopSize)){\n")
    out.append("println(\"dynamic loop2\")\n")
    out.append(closure+".combine("+acc+","+headerObject+".dynamicGet(i))\n")

    out.append("i += 1\n")
    out.append("}\n")
    out.append("println(\"exiting\")\n")
  }

  //TODO: is the division logic really target dependent?
  protected def calculateRange(): (String,String) = {
    out.append("val startOffset = "+closure+".loopStart\n")
    out.append("val size: Long = "+closure+".loopSize\n")
    out.append("val start: Int = (startOffset + size*"+chunkIdx+"/"+numChunks+").asInstanceOf[Int]\n")
    out.append("val end: Int = (startOffset + size*"+(chunkIdx+1)+"/"+numChunks+").asInstanceOf[Int]\n")
    ("start","end")
  }

  protected def allocateOutput(): String = {
    out.append("val out = "+headerObject+".out\n")
    "out"
  }

  protected def processRange(outputSym: String, start: String, end: String) = {
    out.append("val acc = "+closure+".processRange("+outputSym+","+start+","+end+")\n")
    "acc"
  }

  protected def combine(acc: String, neighbor: String) {
    out.append(closure+".combine("+acc+", "+neighbor+")\n")
  }

  protected def postProcess(acc: String) {
    out.append(closure+".postProcess("+acc+")\n")
  }

  protected def postProcInit(acc: String) {
    out.append(closure+".postProcInit("+acc+")\n")
  }

  protected def postCombine(acc: String, neighbor: String) {
    out.append(closure+".postCombine("+acc+", "+neighbor+")\n")
  }

  protected def finalize(acc: String) {
    out.append(closure+".finalize("+acc+")\n")
  }

  protected def set(syncObject: String, idx: Int, value: String) {
    out.append(headerObject+".set"+syncObject+idx+"("+value+")\n")
  }

  protected def get(syncObject: String, idx: Int) = {
    out.append("val neighbor"+syncObject+idx+" = "+headerObject+".get"+syncObject+idx+"\n")
    "neighbor"+syncObject+idx
  }

  protected def beginProfile() {
    out.append("PerformanceTimer.startChunked(\""+master.id+"\", Thread.currentThread.getName(), "+numChunks+", "+chunkIdx+")\n")
  }

  protected def endProfile() {
    out.append("PerformanceTimer.stopChunked(\""+master.id+"\", "+chunkIdx+")\n")
  }

  protected def kernelName = "MultiLoop_" + master.id + "_Chunk_" + chunkIdx

}


class ScalaMultiLoopHeaderGenerator(val op: OP_MultiLoop, val numChunks: Int, val graph: DeliteTaskGraph) extends MultiLoop_SMP_Array_Header_Generator {

  protected def addSource(source: String, name: String) = ScalaCompile.addSource(source, name)

  protected def writeHeader() {
    out.append("import java.util.concurrent.atomic._\n")
    writeObject()
    writeClass()
  }

  protected def writeFooter() {
    out.append("}\n")
  }

  protected def writeObject() {
    ScalaExecutableGenerator.writePath(graph.kernelPath, out)
    out.append("object ")
    out.append(kernelName)
    out.append(" {\n")
    writeObjectApply()
    out.append("}\n")
  }

  protected def writeObjectApply() {
    out.append("def apply(")
    var inIdx = 0
    var first = true
    for ((input, name) <- op.getInputs) {
      if (!first) out.append(", ")
      first = false
      out.append("in")
      out.append(inIdx)
      inIdx += 1
      out.append(": ")
      out.append(input.outputType(name))
    }
    out.append(") = new ")
    out.append(className)
    out.append("(")
    for (i <- 0 until inIdx) {
      if (i > 0) out.append(", ")
      out.append("in")
      out.append(i)
    }
    out.append(")\n")
  }

  protected def writeClass() {
    out.append("final class ")
    out.append(className)
    out.append("(")
    var inIdx = 0
    var first = true
    for ((input, name) <- op.getInputs) {
      if (!first) out.append(", ")
      first = false
      out.append("in")
      out.append(inIdx)
      inIdx += 1
      out.append(": ")
      out.append(input.outputType(name))
    }
    out.append(") {\n")

    out.append("val closure = ")
    out.append(op.function)
    out.append("(")
    for (i <- 0 until inIdx) {
      if (i > 0) out.append(", ")
      out.append("in")
      out.append(i)
    }
    out.append(")\n")

    if (Config.clusterMode == 2) {
      out.append("closure.loopStart = ppl.delite.runtime.DeliteMesosExecutor.loopStart\n")
      out.append("closure.loopSize = if (ppl.delite.runtime.DeliteMesosExecutor.loopSize != -1) ppl.delite.runtime.DeliteMesosExecutor.loopSize else closure.size - closure.loopStart\n")
    }
    else {
      out.append("closure.loopStart = 0\n")
      out.append("closure.loopSize = closure.size\n")
    }

    out.append("val out: ")
    out.append(op.outputType)
    out.append(" = closure.alloc\n")
  }

  //add code to code generate an atomic integer method
  //you can pull and set from this 
  protected def writeSynchronizedOffset(numDynamicChunks: String){
    out.append("private val offset = new AtomicInteger(0)\n")
    out.append("val numDynamicChunks = " + numDynamicChunks+"\n")
    out.append("val dynamicChunkSize = ((closure.loopSize+" + numDynamicChunks + "-1)/" + numDynamicChunks + ")\n")
    out.append("def getOffset() : Int = { offset.getAndAdd(dynamicChunkSize) }\n")
  }
  protected def writeSync(key: String) {
    val outputType = op.outputType

    out.append("@volatile private var notReady")
    out.append(key)
    out.append(": Boolean = true\n")

    out.append("private var _result")
    out.append(key)
    out.append(" : ")
    out.append(outputType)
    out.append(" = _\n")

    out.append("def get")
    out.append(key)
    out.append(": ")
    out.append(outputType)
    out.append(" = { while (notReady")
    out.append(key)
    out.append(") { }; _result")
    out.append(key)
    out.append(" }\n")

    out.append("def set")
    out.append(key)
    out.append("(result: ")
    out.append(outputType)
    out.append(") { _result")
    out.append(key)
    out.append(" = result; notReady")
    out.append(key)
    out.append(" = false }\n")
  }

  protected def dynamicWriteSync(len: String) {
    val outputType = op.outputType

    out.append("private val dynamicNotReady = new AtomicIntegerArray("+len+")\n")
    out.append("private val _dynamicResult")
    out.append(" : Array[")
    out.append(outputType)
    out.append("] = new Array["+outputType+"]("+len+")\n")

    out.append("def dynamicGet(i: Int) :")
    out.append(outputType)
    out.append(" = { while (dynamicNotReady.get(i)==0) { }; _dynamicResult(i) }\n")

    out.append("def dynamicSet")
    out.append("(i: Int,dynamicResult: ")
    out.append(outputType)
    out.append(") { _dynamicResult(i)")
    out.append(" = dynamicResult; dynamicNotReady.set(i,1);}\n")
  }

  protected def className = "MultiLoopHeader_" + op.id

  protected def kernelName = className

}
