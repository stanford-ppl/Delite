package ppl.delite.runtime.codegen.kernels.scala

import ppl.delite.runtime.codegen.{ScalaExecutableGenerator, ScalaCompile}
import ppl.delite.runtime.codegen.ScalaResourceInfo._
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
  protected val closure = "closure"
  protected val allocSym = "head.out"

  protected def addSource(source: String, name: String) = ScalaCompile.addSource(source, name)

  protected def writeHeader() {
    ScalaExecutableGenerator.writePackage(graph, out)
    out.append("import ppl.delite.runtime.profiler.PerformanceTimer\n")
    out.append("import ppl.delite.runtime.profiler.MemoryProfiler\n")
    ScalaExecutableGenerator.writePath(graph, out)
    out.append("object " + kernelName + " {\n")
    if (Config.profile) out.append("val threadName = Thread.currentThread.getName()\n")
  }

  protected def writeFooter(){
    out.append("}\n")
  }

  protected def writeKernelHeader() {
    out.append(s"def apply($resourceInfoSym: $resourceInfoType, $headerObject: ${op.getInputs.head._1.outputType}): ${op.outputType} = {\n")
    out.append(s"val tid = $resourceInfoSym.threadId\n")
    out.append(s"val numThreads = $resourceInfoSym.numThreads\n")
    out.append(s"val closure = $headerObject.closure\n")
  }

  protected def writeKernelFooter() {
    out.append("}\n")
  }

  protected def processLocal() = {
    out.append(s"""//process local
  var dIdx = tid
  val numChunks = $headerObject.numChunks
  val startOffset = $closure.loopStart
  val size: Long = $closure.loopSize
  while(dIdx < numChunks) {
    val start: Long = startOffset + size*dIdx/numChunks
    val end: Long = startOffset + size*(dIdx+1)/numChunks
    val act = $closure.processRange($resourceInfoSym, $allocSym, start, end)
    $headerObject.set(dIdx,act)
    dIdx = $headerObject.getNextChunkIdx()
  }
  val localStart = tid*numChunks/numThreads
  val localEnd = (tid+1)*numChunks/numThreads
  val act = $headerObject.get(localStart)
  \n""")
  }

  protected def combineLocal() = {
    out.append(s"""//combine local
  var i = localStart+1
  while (i < localEnd) {
    $closure.combine($resourceInfoSym, act, $headerObject.get(i))
    i += 1
  }
  \n""")
  }

  protected def combineRemote() = {
    out.append(s"""//combine remote
  var half = tid
  var step = 1
  while ((half % 2 == 0) && (tid + step < numThreads)) {
    val neighborIdx = tid + step
    $closure.combine($resourceInfoSym, act, $headerObject.getC(neighborIdx))
    half /= 2
    step *= 2
  }
  $headerObject.setC(tid, act)
  \n""")
  }

  protected def postCombine() {
    out.append(s"""//post combine
  if (tid != 0) $closure.postCombine($resourceInfoSym, act, $headerObject.getP(tid-1))
  var j = localStart + 1
  var currentAct = act
  while (j < localEnd) {
    val rhsAct = $headerObject.get(j)
    $closure.postCombine($resourceInfoSym, rhsAct, currentAct)
    currentAct = rhsAct
    j += 1
  }
  if (tid == numThreads-1) $closure.postProcInit($resourceInfoSym, currentAct)
  $headerObject.setP(tid, currentAct)
  \n""")
  }

  protected def postProcess() {
    out.append(s"""//post process
  var k = localStart
  while (k < localEnd) {
    $closure.postProcess($resourceInfoSym, $headerObject.get(k))
    k += 1
  }
  \n""")
  }

  protected def finalizer() {
    out.append(s"if (tid == 0) $closure.finalize($resourceInfoSym, act)\n")
  }

  protected def returnResult() {
    out.append("act\n")
  }

  protected def barrier() {
    out.append(s"$headerObject.barrier.await()\n")
  }

  protected def beginProfile() {
<<<<<<< HEAD
    //out.append("PerformanceTimer.startChunked(\""+master.id+"\", Thread.currentThread.getName(), "+numChunks+", "+chunkIdx+")\n")
    val chunkName = master.id + "_" + chunkIdx
    out.append("PerformanceTimer.start(\""+chunkName+"\", threadName, false)\n")
  }

  protected def endProfile() {
    //out.append("PerformanceTimer.stopChunked(\""+master.id+"\", "+chunkIdx+")\n")
    val chunkName = master.id + "_" + chunkIdx
    out.append("PerformanceTimer.stop(\""+chunkName+"\", threadName, false)\n")
=======
    out.append("PerformanceTimer.start(\""+master.id+"_"+"\"+tid, threadName, false)\n")
  }

  protected def endProfile(isMaster: Boolean) {
    val timeStr = "PerformanceTimer.stop(\""+master.id+"_"+"\"+tid, threadName, false)\n"
    if (isMaster) out.append("if (tid == 0) "+timeStr) else out.append("if (tid != 0) "+timeStr)
>>>>>>> develop
  }

  protected def kernelName = "MultiLoop_" + master.id

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
    out.append("import ppl.delite.runtime.Delite\n")
    out.append("import ppl.delite.runtime.executor.DeliteExecutable\n")
    out.append("object " + kernelName + " {\n")
    writeObjectApply()
    out.append("}\n")
  }

  protected def writeObjectApply() {
    val typedArgs = ((resourceInfoSym+":"+resourceInfoType)+:op.getInputs.map(in => in._2 + ": " + in._1.outputType(in._2))).mkString(", ")
    val unTypedArgs = (resourceInfoSym+:op.getInputs.map(_._2)).mkString(", ")
    if (Config.scheduler == "dynamic") {
      out.append(s"def apply($typedArgs) = launchThreads($resourceInfoSym, new $className($unTypedArgs))\n")
      writeThreadLaunch()
    }
    else out.append(s"def apply($typedArgs) = new $className($unTypedArgs)\n")
  }

  protected def writeClass() {
    val typedArgs = ((resourceInfoSym+":"+resourceInfoType)+:op.getInputs.map(in => in._2 + ": " + in._1.outputType(in._2))).mkString(", ")
    val unTypedArgs = (resourceInfoSym+:op.getInputs.map(_._2)).mkString(", ")
    out.append(s"final class $className($typedArgs) {\n")
    out.append(s"val closure = ${op.function}($unTypedArgs)\n")

    if (Config.clusterMode == 2) {
      out.append("closure.loopStart = ppl.delite.runtime.DeliteMesosExecutor.loopStart\n")
      out.append("closure.loopSize = if (ppl.delite.runtime.DeliteMesosExecutor.loopSize != -1) ppl.delite.runtime.DeliteMesosExecutor.loopSize else closure.size("+resourceInfoSym+") - closure.loopStart\n")
    }
    else {
      out.append("closure.loopStart = 0\n")
      out.append("closure.loopSize = closure.size("+resourceInfoSym+")\n")
    }

    out.append("val numThreads = "+resourceInfoSym+".numThreads\n")
    out.append("val out: "+op.outputType+" = closure.alloc("+resourceInfoSym+")\n")
  }

  protected def defaultChunks() = {
    if(op.numDynamicChunks == "-1")
      "(closure.loopSize/(Math.log10(closure.loopSize.toDouble)*(500.0/numThreads))).toInt"
    else
      op.numDynamicChunks
  }

  protected def writeScheduler() {
    out.append(s"""//scheduler
  private[this] val defaultChunks = ${defaultChunks}
  val numChunks = if (numThreads == 1 || $defaultChunks <= numThreads || closure.loopSize < $defaultChunks) numThreads else $defaultChunks
  private[this] val offset = new java.util.concurrent.atomic.AtomicInteger(numThreads)
  def getNextChunkIdx(): Int = offset.getAndAdd(1)
  val barrier = new java.util.concurrent.CyclicBarrier(numThreads)
  \n""")
  }

  protected def writeActSync(key: String) {
    val size = if (key == "") "numChunks" else "numThreads"
    val outputType = op.outputType
    out.append(s"""//$key sync
  private[this] val results$key = new java.util.concurrent.atomic.AtomicReferenceArray[$outputType]($size)
  def get$key(i: Int): $outputType = { while (results$key.get(i) eq null) { }; results$key.get(i) }
  def set$key(i: Int, res: $outputType): Unit = { results$key.set(i,res) }
  \n""")
  }

  protected def writeThreadLaunch() {
    out.append(s"""//thread launch
  def launchThreads($resourceInfoSym: $resourceInfoType, head: $className) = {
    var i = 1
    while (i < $resourceInfoSym.numThreads) {
      val r = new $resourceInfoType(i, $resourceInfoSym.numThreads)
      val executable = new DeliteExecutable {
        def run() = MultiLoop_${op.id}(r, head)
      }
      Delite.executor.runOne(i, executable)
      i += 1
    }
    head
  }\n""")
  }

  protected def className = "MultiLoopHeader_" + op.id

  protected def kernelName = className

}
