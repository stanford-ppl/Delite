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

  protected def processLocal(outputSym: String): String = {
    out.append("var dIdx = "+chunkIdx+"\n")
    out.append("val numDynamicChunks = "+headerObject + ".numDynamicChunks\n")
    out.append("val startOffset = "+closure+".loopStart\n")
    out.append("val size: Long = "+closure+".loopSize\n")
    out.append("while(dIdx < numDynamicChunks){\n")
    out.append("val start: Long = (startOffset + size*dIdx/numDynamicChunks)\n")
    out.append("val end: Long = (startOffset + size*(dIdx+1)/numDynamicChunks)\n")  
    //out.append("println(\"start: \" + start + \" end: \" +end + \" loopSize: \" + "+closure+".loopSize)\n")
    out.append("val accDynamic = "+closure+".processRange("+outputSym+",start,end,"+chunkIdx+")\n")
    out.append(headerObject+".dynamicSet(dIdx,accDynamic)\n")
    out.append("dIdx = "+headerObject+".getDynamicChunkIndex()\n")
    out.append("}\n")

    out.append("val myThreadDynamicIndexStart = ("+chunkIdx+"*numDynamicChunks)/"+numChunks+"\n")
    out.append("val myThreadDynamicIndexEnd = (("+chunkIdx+"+1)*numDynamicChunks)/"+numChunks+"\n")    
    out.append("val acc = "+headerObject+".dynamicGet(myThreadDynamicIndexStart)\n")
    "acc"
  }

  protected def combineLocal(acc: String) = {
    out.append("var i = 1+myThreadDynamicIndexStart\n")
    out.append("while(i < myThreadDynamicIndexEnd){\n")
    combine(acc, headerObject+".dynamicGet(i)")
    out.append("i += 1\n")
    out.append("}\n")
  }

  protected def postCombine(acc: String) = {
    if (chunkIdx != 0) {
      postCombine(acc, get("B", chunkIdx-1)) //linear chain combine
    }
    out.append("var j = 1+myThreadDynamicIndexStart\n")
    out.append("var old = "+acc+"\n")
    out.append("while(j < myThreadDynamicIndexEnd){\n")
    postCombine(headerObject+".dynamicGet(j)", "old")
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
    postProcess(headerObject+".dynamicGet(j)")
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

  protected def combine(acc: String, neighbor: String) {
    out.append(closure+".combine("+acc+", "+neighbor+", "+chunkIdx+")\n")
  }

  protected def postProcess(acc: String) {
    out.append(closure+".postProcess("+acc+","+chunkIdx+")\n")
  }

  protected def postProcInit(acc: String) {
    out.append(closure+".postProcInit("+acc+","+chunkIdx+")\n")
  }

  protected def postCombine(acc: String, neighbor: String) {
    out.append(closure+".postCombine("+acc+", "+neighbor+", "+chunkIdx+")\n")
  }

  protected def finalize(acc: String) {
    out.append(closure+".finalize("+acc+","+chunkIdx+")\n")
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
      val ins = op.getInputs.filter(i => i._1.outputType(i._2).contains("Stream"))
      if (ins.length > 0) {
        val stream = "in" + op.getInputs.indexOf(ins(0))
        out.append("closure.loopStart = 0\n")
        out.append("closure.loopSize = "+stream+".size\n")
      } else {
        val range = scala.io.Source.fromFile("/tmp/range.txt").getLines.map(_.toInt).toSeq
        out.append("closure.loopStart = "+range(0)+"\n")
        out.append("closure.loopSize = "+(range(1)-range(0))+"\n")
      //out.append("closure.loopStart = ppl.delite.runtime.DeliteMesosExecutor.loopStart\n")
      //out.append("closure.loopSize = if (ppl.delite.runtime.DeliteMesosExecutor.loopSize != -1) ppl.delite.runtime.DeliteMesosExecutor.loopSize else closure.size - closure.loopStart\n")
      //out.append("val slaveIdx = ppl.delite.runtime.DeliteMesosExecutor.slaveIdx\n")
      //out.append("val numSlaves = ppl.delite.runtime.DeliteMesosExecutor.numSlaves\n")
      //out.append("closure.loopStart = closure.size*slaveIdx/numSlaves\n")
      //out.append("closure.loopSize = closure.size*(slaveIdx+1)/numSlaves - closure.loopStart\n")
      out.append("ppl.delite.runtime.DeliteMesosExecutor.sendDebugMessage(\"loop: from \" + closure.loopStart + \" to \" + (closure.loopStart+closure.loopSize))\n")
      }
    }
    else {
      out.append("closure.loopStart = 0\n")
      out.append("closure.loopSize = closure.size\n")
    }

    out.append("val out: ")
    out.append(op.outputType)
    out.append(" = closure.alloc\n")
  }

  protected def writeSynchronizedOffset(){
    out.append("private val proposedNumberOfDynamicChunks = ")
    if(op.numDynamicChunks == "-1")
      out.append("(closure.loopSize/(Math.log10(closure.loopSize.toDouble)*(500.0/"+numChunks+"))).toInt\n")
    else
      out.append(op.numDynamicChunks+"\n")
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

    out.append("private val dynamicResult = new AtomicReferenceArray[")
    out.append(outputType)
    out.append("](numDynamicChunks)\n")

    out.append("def dynamicGet(i: Int) :")
    out.append(outputType)
    out.append(" = { while (dynamicResult.get(i) eq null) { }; dynamicResult.get(i) }\n")

    out.append("def dynamicSet")
    out.append("(i: Int, res: ")
    out.append(outputType)
    out.append(") { dynamicResult.set(i,res) }\n")
  }

  protected def className = "MultiLoopHeader_" + op.id

  protected def kernelName = className

}
