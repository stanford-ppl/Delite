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
  val kernelPath: String

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
  protected def returnResult(result: String)
  protected def kernelName: String
  protected def addSource(source: String, name: String)

  //runtime services
  protected def calculateRange(): (String, String)
  protected def get(syncObject: String, idx: Int): String
  protected def set(syncObject: String, idx: Int, value: String)

  //the compiler multiLoop api
  protected def allocateOutput(): String
  protected def processRange(output: String, start: String, end: String): String
  protected def combine(acc: String, neighbor: String)
  protected def postProcess(acc: String)
  protected def postProcInit(acc: String)
  protected def postCombine(acc: String, neighbor: String)
  protected def finalize(acc: String)

  protected def beginProfile()
  protected def endProfile()

  protected def writeKernel(op: OP_MultiLoop) {
    writeKernelHeader()

    // profiling
    if (Config.profile)
      beginProfile()

    //determine range of chunk
    val (start,end) = calculateRange()
    val outSym = allocateOutput()

    val acc = processRange(outSym,start,end)

    // profiling
    if (Config.profile)
      endProfile()

    //tree reduction
    if (op.needsCombine) {
      var half = chunkIdx
      var step = 1
      while ((half % 2 == 0) && (chunkIdx + step < numChunks)) { //half the chunks quit each iteration
        half = half / 2
        val neighbor = chunkIdx + step //the index of the chunk to reduce with
        step *= 2

        combine(acc, get("A", neighbor))
      }
      if (chunkIdx != 0) { //other chunks store result
        set("A", chunkIdx, acc)
      }
    }

    if (op.needsPostProcess) {
      if (chunkIdx != 0) {
        val neighbor = chunkIdx - 1
        postCombine(acc, get("B", neighbor))
      }
      if (chunkIdx == numChunks-1) {
        postProcInit(acc)
      }

      if (numChunks > 1) set("B", chunkIdx, acc) // kick off others
      if (chunkIdx != numChunks-1) get("B", numChunks-1) // wait for last one
      postProcess(acc)
    }

    if (chunkIdx == 0) finalize(acc)
    if (chunkIdx == 0) returnResult(acc)

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

    if (op.needsCombine) {
      //the sync state
      for (i <- 1 until numChunks) //sync for all chunks except 0
        writeSync("A"+i)
    }
    if (op.needsPostProcess && numChunks > 1) { //all chunks need to sync
      for (i <- 0 until numChunks)
        writeSync("B"+i)
    }

    writeFooter()

    //add header for compilation
    val src = out.toString
    addSource(src, className)

    //return header OP
    op.header(kernelName, className, graph)
  }

  protected def writeHeader()
  protected def writeFooter()
  protected def writeSync(key: String)
  protected def kernelName: String
  protected def className: String
  protected def addSource(source: String, name: String)

}
