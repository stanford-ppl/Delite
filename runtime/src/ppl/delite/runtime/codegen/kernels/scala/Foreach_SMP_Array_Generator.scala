package ppl.delite.runtime.codegen.kernels.scala

import ppl.delite.runtime.graph.ops.OP_Foreach
import ppl.delite.runtime.codegen.{ScalaExecutableGenerator, ScalaCompile}
import ppl.delite.runtime.graph.DeliteTaskGraph

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

  def makeChunk(op: OP_Foreach, chunkIdx: Int, numChunks: Int, graph: DeliteTaskGraph): OP_Foreach = {
    val chunk = if (chunkIdx == 0) op else op.chunk(chunkIdx)
    ScalaCompile.addSource(makeKernel(chunk, op, chunkIdx, numChunks, graph), kernelName(op, chunkIdx))
    chunk
  }

  private def makeKernel(op: OP_Foreach, master: OP_Foreach, chunkIdx: Int, numChunks: Int, graph: DeliteTaskGraph) = {
    val out = new StringBuilder

    //update the op with this kernel
    updateOP(op, master, chunkIdx)

    //the header
    writeHeader(out, master, chunkIdx, graph)

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

  private def writeHeader(out: StringBuilder, master: OP_Foreach, idx: Int, graph: DeliteTaskGraph) {
    ScalaExecutableGenerator.writePackage(graph, out)
    ScalaExecutableGenerator.writePath(graph, out)
    out.append("import java.util.concurrent.locks.ReentrantLock\n")
    out.append("object ")
    out.append(kernelName(master, idx))
    out.append(" {\n")
  }

  private def writeKernel(out: StringBuilder, op: OP_Foreach, master: OP_Foreach, chunkIdx: Int, numChunks: Int) {
    out.append("def apply(foreach: ")
    out.append(op.getInputs.head._1.outputType)
    out.append(") {\n")

    out.append("val in = foreach.closure.in\n")
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

    out.append("val sync = foreach.closure.sync(idx)\n")//_.sortBy(System.identityHashCode(_))\n")
    out.append("for (e <- sync) {\n")
    out.append("foreach.lockMap.putIfAbsent(e, new ReentrantLock)\n")
    out.append("foreach.lockMap.get(e).lock\n")
    out.append("}\n")

    out.append("foreach.closure.foreach(in.dcApply(idx))\n")

    out.append("for (e <- sync.reverse) ")
    out.append("foreach.lockMap.get(e).unlock\n")

    out.append("idx += 1\n")
    out.append("}\n}\n")
  }

  private def kernelName(master: OP_Foreach, idx: Int) = {
    "Foreach_SMP_Array_" + master.id + "_Chunk_" + idx
  }

}

object Foreach_SMP_Array_Header_Generator {

  def makeHeader(op: OP_Foreach, graph: DeliteTaskGraph) = {
    val out = new StringBuilder

    //the header
    writeObject(out, op, graph)

    //the kernel
    writeClass(out, op)

    //the footer
    out.append('}')
    out.append('\n')

    //add header for compilation
    ScalaCompile.addSource(out.toString, kernelName(op))

    //return header OP
    op.header(kernelName(op), graph)
  }

  private def writeObject(out: StringBuilder, op: OP_Foreach, graph: DeliteTaskGraph) {
    ScalaExecutableGenerator.writePackage(graph, out)
    ScalaExecutableGenerator.writePath(graph, out)
    out.append("import java.util.concurrent.ConcurrentHashMap\n")
    out.append("import java.util.concurrent.locks.ReentrantLock\n")
    out.append("object ")
    out.append(kernelName(op))
    out.append(" {\n")
    writeObjectApply(out, op)
    out.append("}\n")
  }

  private def writeObjectApply(out: StringBuilder, op: OP_Foreach) {
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
      val (dep,name) = inputs.next
      out.append(dep.outputType(name))
    }
    out.append(") = new ")
    out.append(kernelName(op))
    out.append("(in0")
    for (i <- 1 until inIdx) {
      out.append(", in" + i)
    }
    out.append(")\n")
  }

  private def writeClass(out: StringBuilder, op: OP_Foreach) {
    out.append("final class ")
    out.append(kernelName(op))
    out.append('(')
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
      val (dep,name) = inputs.next
      out.append(dep.outputType(name))
    }
    out.append(") {\n")

    out.append("val closure = ")
    out.append(op.function)
    out.append("(in0")
    for (i <- 1 until inIdx) {
      out.append(", in" + i)
    }
    out.append(")\n")

    out.append("val lockMap = new ConcurrentHashMap[Any, ReentrantLock]\n")
  }

  private def kernelName(op: OP_Foreach) = {
    "Foreach_SMP_Array_Header_" + op.id
  }
}
