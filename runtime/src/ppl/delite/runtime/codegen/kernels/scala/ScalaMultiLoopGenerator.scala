package ppl.delite.runtime.codegen.kernels.scala

import ppl.delite.runtime.codegen.{ScalaExecutableGenerator, ScalaCompile}
import ppl.delite.runtime.graph.ops.OP_MultiLoop
import ppl.delite.runtime.codegen.kernels.{MultiLoop_SMP_Array_Header_Generator, MultiLoop_SMP_Array_Generator}
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.Config

object ScalaMultiLoopGenerator {
  def makeChunks(op: OP_MultiLoop, numChunks: Int, graph: DeliteTaskGraph) = {
    for (idx <- 0 until numChunks) yield {
      val chunk = if (idx == 0) op else op.chunk(idx)
      (new ScalaMultiLoopGenerator(chunk, op, idx, numChunks, graph)).makeChunk()
      chunk
    }
  }
}

class ScalaMultiLoopGenerator(val op: OP_MultiLoop, val master: OP_MultiLoop, val chunkIdx: Int, val numChunks: Int, val graph: DeliteTaskGraph) extends MultiLoop_SMP_Array_Generator {

  protected val headerObject = "head"
  protected val closure = "head.closure"

  protected def addSource(source: String, name: String) = ScalaCompile.addSource(source, name)

  protected def writeHeader() {
    ScalaExecutableGenerator.writePackage(graph, out)
    out.append("import ppl.delite.runtime.profiler.PerformanceTimer\n")
    out.append("import ppl.delite.runtime.profiler.MemoryProfiler\n")
    ScalaExecutableGenerator.writePath(graph, out)
    out.append("object ")
    out.append(kernelName)
    out.append(" {\n")
    if (Config.profile) out.append("val threadName = Thread.currentThread.getName()\n")
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
    out.append("var dIdx = "+chunkIdx+"\n")
    out.append("val numDynamicChunks = "+headerObject + ".numDynamicChunks\n")
    out.append("val startOffset = "+closure+".loopStart\n")
    out.append("val size: Long = "+closure+".loopSize\n")
    out.append("while(dIdx < numDynamicChunks){\n")
    out.append("val start: Int = (startOffset + size*dIdx/numDynamicChunks).asInstanceOf[Int]\n")
    out.append("val end: Int = (startOffset + size*(dIdx+1)/numDynamicChunks).asInstanceOf[Int]\n")  
    //out.append("println(\"start: \" + start + \" end: \" +end + \" loopSize: \" + "+closure+".loopSize)\n")
    out.append("val accDynamic = "+closure+".processRange("+outputSym+",start,end)\n")
    out.append(headerObject+".dynamicSet(dIdx,accDynamic)\n")
    out.append("dIdx = "+headerObject+".getDynamicChunkIndex()\n")
    out.append("}\n")

    out.append("val myThreadDynamicIndexStart = ("+chunkIdx+"*numDynamicChunks)/"+numChunks+"\n")
    out.append("val myThreadDynamicIndexEnd = (("+chunkIdx+"+1)*numDynamicChunks)/"+numChunks+"\n")    
    out.append("val acc = "+headerObject+".dynamicGet(myThreadDynamicIndexStart)\n")
    "acc"
  }
  protected def dynamicCombine(acc: String) = {
    out.append("var i = 1+myThreadDynamicIndexStart\n")
    out.append("while(i < myThreadDynamicIndexEnd){\n")
    out.append(closure+".combine("+acc+","+headerObject+".dynamicGet(i))\n")
    out.append("i += 1\n")
    out.append("}\n")
  }
  protected def dynamicPostCombine(acc: String) = {
    if (chunkIdx != 0) {
      postCombine(acc, get("B", chunkIdx-1)) //linear chain combine
    }
    out.append("var j = 1+myThreadDynamicIndexStart\n")
    out.append("var old = "+acc+"\n")
    out.append("while(j < myThreadDynamicIndexEnd){\n")
    out.append(closure+".postCombine("+headerObject+".dynamicGet(j),old)\n")
    out.append("old = "+headerObject+".dynamicGet(j)\n")
    out.append("j += 1\n")
    out.append("}\n")
    if (chunkIdx == numChunks-1) {
      postProcInit("old") 
    }
    if(numChunks > 1) set("B", chunkIdx, "old")
    if (chunkIdx != numChunks-1) get("B", numChunks-1) // wait for last one

    out.append("j = myThreadDynamicIndexStart\n")
    out.append("while(j < myThreadDynamicIndexEnd){\n")
    out.append(closure+".postProcess("+headerObject+".dynamicGet(j))\n")
    out.append("j += 1\n")
    out.append("}\n")
  }
  protected def release(name: String, cond: Option[String] = None) {
    // Nothing to do (JVM GC)
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
    //out.append("PerformanceTimer.startChunked(\""+master.id+"\", Thread.currentThread.getName(), "+numChunks+", "+chunkIdx+")\n")
    val chunkName = master.id + "_" + chunkIdx
    out.append("PerformanceTimer.start(\""+chunkName+"\", Thread.currentThread.getName(), false)\n")
  }

  protected def endProfile() {
    val chunkName = master.id + "_" + chunkIdx
    //out.append("PerformanceTimer.stopChunked(\""+master.id+"\", "+chunkIdx+")\n")
    out.append("PerformanceTimer.stop(\""+chunkName+"\", Thread.currentThread.getName(), false)\n")
  }

  protected def kernelName = "MultiLoop_" + master.id + "_Chunk_" + chunkIdx

}


class ScalaMultiLoopHeaderGenerator(val op: OP_MultiLoop, val numChunks: Int, val graph: DeliteTaskGraph) extends MultiLoop_SMP_Array_Header_Generator {

  protected def addSource(source: String, name: String) = ScalaCompile.addSource(source, name)

  protected def writeHeader() {
    writeObject()
    writeClass()
  }

  protected def writeFooter() {
    out.append("}\n")
  }

  protected def writeObject() {
    ScalaExecutableGenerator.writePackage(graph, out)
    ScalaExecutableGenerator.writePath(graph, out)
    out.append("import java.util.concurrent.atomic._\n")
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
  protected def writeSynchronizedOffset(){
    if(op.numDynamicChunks == "-1"){
      //do formula
      out.append("private val proposedNumberOfDynamicChunks = (closure.loopSize/(Math.log10(closure.loopSize.toDouble)*(500.0/"+numChunks+") )).toInt\n")
    }
    else {
      out.append("private val proposedNumberOfDynamicChunks = "+op.numDynamicChunks+ "\n")
    }
    //For Debug
    //out.append("println(\"numDynamicChunks: \" + numDynamicChunks)\n")
    out.append("val numDynamicChunks = if(proposedNumberOfDynamicChunks <= "+numChunks+" || "+numChunks+" == 1 || closure.loopSize < proposedNumberOfDynamicChunks) "+numChunks+" else proposedNumberOfDynamicChunks\n")
    out.append("private val offset = new AtomicInteger("+numChunks+")\n")
    out.append("def getDynamicChunkIndex() : Int = { offset.getAndAdd(1) }\n")
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

  protected def dynamicWriteSync() {
    val outputType = op.outputType

    out.append("private val dynamicNotReady = new AtomicIntegerArray(numDynamicChunks)\n")
    out.append("private val _dynamicResult")
    out.append(" : Array[")
    out.append(outputType)
    out.append("] = new Array["+outputType+"](numDynamicChunks)\n")

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
