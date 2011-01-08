package ppl.delite.runtime.codegen.kernels.scala

import ppl.delite.runtime.graph.ops.OP_Map
import ppl.delite.runtime.codegen.{ExecutableGenerator, ScalaCompile}
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
 * Creates a chunk for OP_Map and generates an executable kernel for that chunk
 * The generated kernels are designed to run in parallel on multiple threads in an SMP system
 * This implementation of Map is optimized for a DSL collection that is backed by an Array
 */

object Map_SMP_Array_Generator {

  def makeChunk(op: OP_Map, chunkIdx: Int, numChunks: Int, kernelPath: String): OP_Map = {
    val chunk = if (chunkIdx == 0) op else op.chunk(chunkIdx)
    ScalaCompile.addSource(makeKernel(chunk, op, chunkIdx, numChunks, kernelPath))
    chunk
  }

  private def makeKernel(op: OP_Map, master: OP_Map, chunkIdx: Int, numChunks: Int, kernelPath: String) = {
    val out = new StringBuilder

    //update the op with this kernel
    updateOP(op, master, chunkIdx)

    //the header
    writeHeader(out, master, chunkIdx, kernelPath)

    //the kernel
    writeKernel(out, op, master, chunkIdx, numChunks)

    //the footer
    out.append('}')
    out.append('\n')

    out.toString
  }

  private def updateOP(op: OP_Map, master: OP_Map, idx: Int) {
    op.setKernelName(kernelName(master, idx))
  }

  private def writeHeader(out: StringBuilder, master: OP_Map, idx: Int, kernelPath: String) {
    ExecutableGenerator.writePath(kernelPath, out)
    out.append("object ")
    out.append(kernelName(master, idx))
    out.append(" {\n")
  }

  private def writeKernel(out: StringBuilder, op: OP_Map, master: OP_Map, chunkIdx: Int, numChunks: Int) {
    out.append("def apply(map: ")
    out.append(op.getInputs.head.outputType)
    out.append("): ")
    out.append(op.outputType)
    out.append(" = {\n")

    out.append("val in = map.closure.in\n")
    out.append("val out = map.out\n")
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
    out.append("out.dcUpdate(idx, map.closure.map(in.dcApply(idx)))\n")
    out.append("idx += 1\n")
    out.append("}\n")
    if (chunkIdx == 0) out.append("out\n")
    out.append("}\n")
  }

  private def kernelName(master: OP_Map, idx: Int) = {
    "Map_SMP_Array_" + master.id + "_Chunk_" + idx
  }

}

object Map_SMP_Array_Header_Generator {

  def makeHeader(op: OP_Map, graph: DeliteTaskGraph) = {
    val out = new StringBuilder

    //the header
    writeObject(out, op, graph.kernelPath)

    //the kernel
    writeClass(out, op)

    //the footer
    out.append('}')
    out.append('\n')

    //add header for compilation
    ScalaCompile.addSource(out.toString)

    //return header OP
    op.header(kernelName(op), graph)
  }

  private def writeObject(out: StringBuilder, op: OP_Map, kernelPath: String) {
    ExecutableGenerator.writePath(kernelPath, out)
    out.append("object ")
    out.append(kernelName(op))
    out.append(" {\n")
    writeObjectApply(out, op)
    out.append("}\n")
  }

  private def writeObjectApply(out: StringBuilder, op: OP_Map) {
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
    out.append(") = new ")
    out.append(kernelName(op))
    out.append("(in0")
    for (i <- 1 until inIdx) {
      out.append(", in" + i)
    }
    out.append(")\n")
  }

  private def writeClass(out: StringBuilder, op: OP_Map) {
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
      out.append(inputs.next.outputType)
    }
    out.append(") {\n")

    out.append("val closure = ")
    out.append(op.function)
    out.append("(in0")
    for (i <- 1 until inIdx) {
      out.append(", in" + i)
    }
    out.append(")\n")

    out.append("val out: ")
    out.append(op.outputType)
    out.append(" = closure.alloc\n")
  }

  private def kernelName(op: OP_Map) = {
    "Map_SMP_Array_Header" + op.id
  }
}
