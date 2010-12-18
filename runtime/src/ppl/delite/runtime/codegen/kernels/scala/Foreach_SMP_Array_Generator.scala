package ppl.delite.runtime.codegen.kernels.scala

import ppl.delite.runtime.graph.ops.OP_Foreach
import ppl.delite.runtime.codegen.{ExecutableGenerator, ScalaCompile}

/**
 * Author: Kevin J. Brown
 * Date: Nov 15, 2010
 * Time: 4:40:17 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * Creates a chunk for OP_Foreach and generates an executable kernel for that chunk
 * The generated kernels are designed to run in parallel on multiple threads in an SMP system
 * This implementation of Foreach is optimized for a DSL collection that is backed by an Array
 */

object Foreach_SMP_Array_Generator {

  def makeChunk(op: OP_Foreach, chunkIdx: Int, numChunks: Int, kernelPath: String): OP_Foreach = {
    val chunk = if (chunkIdx == 0) op else op.chunk(chunkIdx)
    ScalaCompile.addSource(makeKernel(chunk, op, chunkIdx, numChunks, kernelPath))
    chunk
  }

  private def makeKernel(op: OP_Foreach, master: OP_Foreach, chunkIdx: Int, numChunks: Int, kernelPath: String) = {
    val out = new StringBuilder

    //update the op with this kernel
    updateOP(op, master, chunkIdx)

    //the header
    writeHeader(out, master, chunkIdx, kernelPath)

    //the lock state
    if (chunkIdx == 0) { //chunk 0 holds the global state
      writeLockState(out)
    }

    //the kernel
    writeKernel(out, op, master, chunkIdx, numChunks)

    //the footer
    out.append('}')
    out.append('\n')

    out.toString
  }

  private def updateOP(op: OP_Foreach, master: OP_Foreach, idx: Int) {
    op.setKernelName(kernelName(master, idx))
  }

  private def writeHeader(out: StringBuilder, master: OP_Foreach, idx: Int, kernelPath: String) {
    ExecutableGenerator.writePath(kernelPath, out)
    out.append("import java.util.concurrent.ConcurrentHashMap\n")
    out.append("import java.util.concurrent.locks.ReentrantLock\n")
    out.append("object ")
    out.append(kernelName(master, idx))
    out.append(" {\n")
  }

  private def writeLockState(out: StringBuilder) {
    out.append("val lockMap = new ConcurrentHashMap[Any,ReentrantLock]\n")
  }

  private def writeKernel(out: StringBuilder, op: OP_Foreach, master: OP_Foreach, chunkIdx: Int, numChunks: Int) {
    out.append("def apply(")
    val inputs = op.getInputs.iterator
    var inIdx = 0
    var first = true
    while (inputs.hasNext) {
      if (!first) out.append(", ")
      first = false
      out.append("in")
      out.append(inIdx)
      inIdx += 1
      out.append(": ")
      out.append(inputs.next.outputType)
    }
    out.append(") {\n")

    //call the kernel to get the functions
    out.append("val foreach = ")
    out.append(op.function)
    out.append("(in0")
    for (i <- 1 until inIdx) {
      out.append(", in")
      out.append(i)
    }
    out.append(')')
    out.append('\n')

    out.append("val in = foreach.in\n")
    out.append("val size = in.size\n")
    out.append("var idx = size*") //var idx = size*chunkIdx/numChunks
    out.append(chunkIdx)
    out.append('/')
    out.append(numChunks)
    out.append('\n')
    out.append("val end = size*") //val end = size*(chunkIdx+1)/numChunks
    out.append(chunkIdx+1)
    out.append('/')
    out.append(numChunks)
    out.append('\n')
    out.append("while (idx < end) {\n")

    out.append("val sync = foreach.sync(idx).sortBy(System.identityHashCode(_))\n")
    out.append("for (e <- sync) {\n")
    out.append(kernelName(master, 0))
    out.append(".lockMap.putIfAbsent(e, new ReentrantLock)\n")
    out.append(kernelName(master, 0))
    out.append(".lockMap.get(e).lock\n")
    out.append("}\n")

    out.append("foreach.foreach(in.dcApply(idx))\n")

    out.append("for (e <- sync.reverse) ")
    out.append(kernelName(master, 0))
    out.append(".lockMap.get(e).unlock\n")

    out.append("idx += 1\n")
    out.append("}\n}\n")
  }

  private def kernelName(master: OP_Foreach, idx: Int) = {
    "Foreach_SMP_Array_" + master.id + "_Chunk_" + idx
  }

}
