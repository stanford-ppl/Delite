package ppl.delite.runtime.codegen.kernels.scala

import ppl.delite.runtime.graph.ops.OP_Reduce
import ppl.delite.runtime.codegen.{ExecutableGenerator, ScalaCompile}
import ppl.delite.runtime.graph.DeliteTaskGraph

/**
 * Author: Kevin J. Brown
 * Date: Nov 15, 2010
 * Time: 10:07:52 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * Creates a chunk for OP_Reduce and generates an executable kernel for that chunk
 * The generated kernels are designed to run in parallel on multiple threads in an SMP system
 * This implementation of Reduce is optimized for a DSL collection that is backed by an Array
 * WARNING: this implementation of Reduce assumes that the number of chunks <= the size of the collection:
 *  creating more chunks than there are elements in the collection being reduced will produce an INCORRECT result
 */

object Reduce_SMP_Array_Generator {

  def makeChunk(op: OP_Reduce, chunkIdx: Int, numChunks: Int, kernelPath: String): OP_Reduce = {
    val chunk = if (chunkIdx == 0) op else op.chunk(chunkIdx)
    ScalaCompile.addSource(makeKernel(chunk, op, chunkIdx, numChunks, kernelPath))
    chunk
  }

  private def makeKernel(op: OP_Reduce, master: OP_Reduce, chunkIdx: Int, numChunks: Int, kernelPath: String) = {
    val out = new StringBuilder

    //update the op with this kernel
    updateOP(op, master, chunkIdx)

    //the header
    writeHeader(out, master, chunkIdx, kernelPath)

    //the kernel
    writeKernel(out, op, master, chunkIdx, numChunks)

    //the first (primary) reduction
    //writeReduce(out, op, master.outputType, chunkIdx, numChunks)

    //the footer
    out.append('}')
    out.append('\n')

    out.toString
  }

  private def updateOP(op: OP_Reduce, master: OP_Reduce, idx: Int) {
    op.setKernelName(kernelName(master, idx))
  }

  private def writeHeader(out: StringBuilder, master: OP_Reduce, idx: Int, kernelPath: String) {
    ExecutableGenerator.writePath(kernelPath, out)
    out.append("object ")
    out.append(kernelName(master, idx))
    out.append(" {\n")
  }

  private def writeKernel(out: StringBuilder, op: OP_Reduce, master: OP_Reduce, chunkIdx: Int, numChunks: Int) {
    out.append("def apply(reduce: ")
    out.append(op.getInputs.head._1.outputType)
    out.append("): ")
    out.append(op.outputType)
    out.append(" = {\n")

    //tree reduction
    //first every chunk performs its primary (reduction
    out.append("val in = reduce.closure.in\n")
    out.append("val size = in.size\n")
    out.append("var idx = size*")
    out.append(chunkIdx)
    out.append('/')
    out.append(numChunks)
    out.append('\n')
    out.append("val end = size*")
    out.append(chunkIdx+1)
    out.append('/')
    out.append(numChunks)
    out.append('\n')
    out.append("var acc = in.dcApply(idx)\n")
    out.append("idx += 1\n")
    out.append("while (idx < end) {\n")
    out.append("acc = reduce.closure.reduce(acc, in.dcApply(idx))\n")
    out.append("idx += 1\n")
    out.append("}\n") //return acc

    var half = chunkIdx
    var step = 1
    while ((half % 2 == 0) && (chunkIdx + step < numChunks)) { //half the chunks quit each iteration
      half = half / 2
      val neighbor = chunkIdx + step //the index of the chunk to reduce with
      step *= 2

      out.append("acc = reduce.closure.reduce(acc, reduce.get")
      out.append(neighbor)
      out.append(')')
      out.append('\n')
    }
    if (chunkIdx == 0) { //chunk 0 returns result
      out.append("acc\n")
    }
    else { //other chunks store result
      out.append("reduce.set")
      out.append(chunkIdx)
      out.append("(acc)\n")
    }
    out.append('}')
    out.append('\n')
  }

  private def kernelName(master: OP_Reduce, idx: Int) = {
    "Reduce_SMP_Array_" + master.id + "_Chunk_" + idx
  }

}

object Reduce_SMP_Array_Header_Generator {

  def makeHeader(op: OP_Reduce, numChunks: Int, graph: DeliteTaskGraph) = {
    val out = new StringBuilder

    //the header
    writeObject(out, op, graph.kernelPath)

    //the kernel
    writeClass(out, op)

    //the sync state
    for (i <- 1 until numChunks) //sync for all chunks except 0
      writeSync(out, i, op.outputType)

    //the footer
    out.append('}')
    out.append('\n')

    //add header for compilation
    ScalaCompile.addSource(out.toString)

    //return header OP
    op.header(kernelName(op), graph)
  }

  private def writeObject(out: StringBuilder, op: OP_Reduce, kernelPath: String) {
    ExecutableGenerator.writePath(kernelPath, out)
    out.append("object ")
    out.append(kernelName(op))
    out.append(" {\n")
    writeObjectApply(out, op)
    out.append("}\n")
  }

  private def writeObjectApply(out: StringBuilder, op: OP_Reduce) {
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
      out.append(dep.outputSlotType(name))
    }
    out.append(") = new ")
    out.append(kernelName(op))
    out.append("(in0")
    for (i <- 1 until inIdx) {
      out.append(", in" + i)
    }
    out.append(")\n")
  }

  private def writeClass(out: StringBuilder, op: OP_Reduce) {
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
      out.append(dep.outputSlotType(name))
    }
    out.append(") {\n")

    out.append("val closure = ")
    out.append(op.function)
    out.append("(in0")
    for (i <- 1 until inIdx) {
      out.append(", in" + i)
    }
    out.append(")\n")
  }

  private def writeSync(out: StringBuilder, chunkIdx: Int, outputType: String) {
    out.append("@volatile private var notReady")
    out.append(chunkIdx)
    out.append(": Boolean = true\n")

    out.append("private var _result")
    out.append(chunkIdx)
    out.append(" : ")
    out.append(outputType)
    out.append(" = _\n")

    out.append("def get")
    out.append(chunkIdx)
    out.append(": ")
    out.append(outputType)
    out.append(" = { while (notReady")
    out.append(chunkIdx)
    out.append(") { }; _result")
    out.append(chunkIdx)
    out.append(" }\n")

    out.append("def set")
    out.append(chunkIdx)
    out.append("(result: ")
    out.append(outputType)
    out.append(") { _result")
    out.append(chunkIdx)
    out.append(" = result; notReady")
    out.append(chunkIdx)
    out.append(" = false }\n")
  }

  private def kernelName(op: OP_Reduce) = {
    "Reduce_SMP_Array_Header" + op.id
  }
}

