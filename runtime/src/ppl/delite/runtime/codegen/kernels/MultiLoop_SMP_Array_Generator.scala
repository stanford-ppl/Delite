package ppl.delite.runtime.codegen.kernels

import ppl.delite.runtime.graph.ops.OP_MultiLoop
import ppl.delite.runtime.codegen.{ScalaExecutableGenerator, ScalaCompile}
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.Config

/**
 * Author: Kevin J. Brown
 * Date: Nov 17, 2010
 * Time: 9:00:34 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * Creates a chunk for OP_MultiLoop and generates an executable kernel for that chunk
 * The generated kernels are designed to run in parallel on multiple threads in an SMP system
 * This implementation of MultiLoop is optimized for a DSL collection that is backed by an Array
 */

trait MultiLoop_SMP_Array_Generator {

  val op: OP_MultiLoop
  val master: OP_MultiLoop
  val chunkIdx: Int
  val numChunks: Int
  val graph: DeliteTaskGraph

  def makeChunk() {
    val src = makeKernel()
    addSource(src, kernelName)
  }

  protected val out = new StringBuilder

  protected def makeKernel() = {
    //update the op with this kernel
    updateOP()

    //the header
    writeHeader()

    //the kernel
    writeKernel(op)

    //the footer
    writeFooter()

    out.toString
  }

  protected def updateOP() {
    op.setKernelName(kernelName)
  }

  protected def writeHeader()
  protected def writeFooter()
  protected def writeKernelHeader()
  protected def writeKernelFooter()
  protected def kernelName: String
  protected def addSource(source: String, name: String)

  protected def processLocal()
  protected def combineLocal()
  protected def combineRemote()
  protected def postCombine()
  protected def postProcess()
  protected def finalizer()
  protected def returnResult()
  protected def barrier()
  protected def beginProfile()
  protected def endProfile(isMaster: Boolean)

  protected def writeKernel(op: OP_MultiLoop) {
    writeKernelHeader()
    
    if (Config.profile) beginProfile()

    processLocal()
    if (Config.profile && !op.needsCombine && !op.needsPostProcess) endProfile(false)

    if (op.needsCombine) {
      combineLocal()
      combineRemote()
      if (Config.profile && !op.needsPostProcess) endProfile(false)
      barrier()
    }

    if (op.needsPostProcess) {
      postCombine()
      barrier()
      postProcess()
      if (Config.profile) endProfile(false)
      barrier()
    }

    if (!op.needsPostProcess && !op.needsCombine) barrier()

    finalizer()
    if (Config.profile) endProfile(true)
    returnResult()
    writeKernelFooter()
  }

}

trait MultiLoop_SMP_Array_Header_Generator {

  val op: OP_MultiLoop
  val numChunks: Int
  val graph: DeliteTaskGraph

  protected val out = new StringBuilder

  def makeHeader() = {

    writeHeader()
    writeScheduler()
    writeActSync("")
    writeActSync("C")
    writeFooter()

    //add header for compilation
    val src = out.toString
    addSource(src, className)

    //return header OP
    op.header(kernelName, className, graph)
  }

  protected def writeHeader()
  protected def writeFooter()
  protected def writeScheduler()
  protected def writeActSync(key: String)
  protected def kernelName: String
  protected def className: String
  protected def addSource(source: String, name: String)

}
