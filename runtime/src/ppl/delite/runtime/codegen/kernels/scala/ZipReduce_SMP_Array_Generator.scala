package ppl.delite.runtime.codegen.kernels.scala

import ppl.delite.runtime.graph.ops.OP_ZipReduce
import ppl.delite.runtime.codegen.{ExecutableGenerator, ScalaCompile}
import ppl.delite.runtime.graph.DeliteTaskGraph

/**
 * Creates a chunk for OP_ZipReduce and generates an executable kernel for that chunk
 * The generated kernels are designed to run in parallel on multiple threads in an SMP system
 * This implementation of ZipReduce is optimized for a DSL collection that is backed by an Array
 * WARNING: this implementation of ZipReduce assumes that the number of chunks <= the size of the collection:
 *  creating more chunks than there are elements in the collection being reduced will produce an INCORRECT result
 */

object ZipReduce_SMP_Array_Generator {

  def makeChunk(op: OP_ZipReduce, chunkIdx: Int, numChunks: Int, kernelPath: String): OP_ZipReduce = {
    val chunk = if (chunkIdx == 0) op else op.chunk(chunkIdx)
    ScalaCompile.addSource(makeKernel(chunk, op, chunkIdx, numChunks, kernelPath))
    chunk
  }

  private def makeKernel(op: OP_ZipReduce, master: OP_ZipReduce, chunkIdx: Int, numChunks: Int, kernelPath: String) = {
    val out = new StringBuilder

    //update the op with this kernel
    updateOP(op, master, chunkIdx)

    //the header
    writeHeader(out, master, chunkIdx, kernelPath)

    //the kernel
    writeKernel(out, op, master, chunkIdx, numChunks)

    //the first (primary) reduction
    //writeZipReduce(out, op, master.outputType, chunkIdx, numChunks)

    //the footer
    out.append('}')
    out.append('\n')

    out.toString
  }

  private def updateOP(op: OP_ZipReduce, master: OP_ZipReduce, idx: Int) {
    op.setKernelName(kernelName(master, idx))
  }

  private def writeHeader(out: StringBuilder, master: OP_ZipReduce, idx: Int, kernelPath: String) {
    ExecutableGenerator.writePath(kernelPath, out)
    out.append("object ")
    out.append(kernelName(master, idx))
    out.append(" {\n")
  }

  private def writeKernel(out: StringBuilder, op: OP_ZipReduce, master: OP_ZipReduce, chunkIdx: Int, numChunks: Int) {
    out.append("def apply(zipReduce: ")
    out.append(op.getInputs.head.outputType)
    out.append("): ")
    out.append(op.outputType)
    out.append(" = {\n")

    //tree reduction
    //first every chunk performs its primary (zip-)reduction
    out.append("val inA = zipReduce.closure.inA\n")
    out.append("val inB = zipReduce.closure.inB\n")
    out.append("val size = inA.size\n")
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
    out.append("var acc = zipReduce.closure.zip(inA.dcApply(idx), inB.dcApply(idx))\n")
    out.append("idx += 1\n")
    out.append("while (idx < end) {\n")
    //out.append("acc = zipReduce.closure.reduce(acc, zipReduce.closure.zip(in.dcApply(idx)))\n")
    out.append("acc = zipReduce.closure.zipreduce(acc, inA.dcApply(idx), inB.dcApply(idx))\n")
    out.append("idx += 1\n")
    out.append("}\n") //return acc

    var half = chunkIdx
    var step = 1
    while ((half % 2 == 0) && (chunkIdx + step < numChunks)) { //half the chunks quit each iteration
      half = half / 2
      val neighbor = chunkIdx + step //the index of the chunk to reduce with
      step *= 2

      out.append("acc = zipReduce.closure.reduce(acc, zipReduce.get")
      out.append(neighbor)
      out.append(')')
      out.append('\n')
    }
    if (chunkIdx == 0) { //chunk 0 returns result
      out.append("acc\n")
    }
    else { //other chunks store result
      out.append("zipReduce.set")
      out.append(chunkIdx)
      out.append("(acc)\n")
    }
    out.append('}')
    out.append('\n')
  }

  private def kernelName(master: OP_ZipReduce, idx: Int) = {
    "ZipReduce_SMP_Array_" + master.id + "_Chunk_" + idx
  }

}

object ZipReduce_SMP_Array_Header_Generator {

  def makeHeader(op: OP_ZipReduce, numChunks: Int, graph: DeliteTaskGraph) = {
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

  private def writeObject(out: StringBuilder, op: OP_ZipReduce, kernelPath: String) {
    ExecutableGenerator.writePath(kernelPath, out)
    out.append("object ")
    out.append(kernelName(op))
    out.append(" {\n")
    writeObjectApply(out, op)
    out.append("}\n")
  }

  private def writeObjectApply(out: StringBuilder, op: OP_ZipReduce) {
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

  private def writeClass(out: StringBuilder, op: OP_ZipReduce) {
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

  private def kernelName(op: OP_ZipReduce) = {
    "ZipReduce_SMP_Array_Header" + op.id
  }
}
