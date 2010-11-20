package ppl.delite.runtime.walktime.codegen.kernels.scala

import ppl.delite.runtime.walktime.graph.ops.OP_Zip
import ppl.delite.runtime.walktime.codegen.ScalaCompile

/**
 * Author: Kevin J. Brown
 * Date: Nov 17, 2010
 * Time: 9:00:34 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * Creates a chunk for OP_Zip and generates an executable kernel for that chunk
 * The generated kernels are designed to run in parallel on multiple threads in an SMP system
 * This implementation of Zip is optimized for a DSL collection that is backed by an Array
 */

object Zip_SMP_Array_Generator {

  def makeChunk(op: OP_Zip, chunkIdx: Int, numChunks: Int): OP_Zip = {
    val chunk = if (chunkIdx == 0) op else op.chunk
    ScalaCompile.addSource(makeKernel(chunk, chunkIdx, numChunks))
    chunk
  }

  private def makeKernel(op: OP_Zip, chunkIdx: Int, numChunks: Int) = {
    val out = new StringBuilder

    //update the op with this kernel
    updateOP(op, chunkIdx)

    //the header
    writeHeader(out, op, chunkIdx)

    //the kernel
    writeKernel(out, op, chunkIdx, numChunks)

    //the footer
    out.append('}')
    out.append('\n')

    out.toString
  }

  private def updateOP(op: OP_Zip, idx: Int) {
    op.setKernelName(kernelName(op, idx))
  }

  private def writeHeader(out: StringBuilder, op: OP_Zip, idx: Int) {
    out.append("object ")
    out.append(kernelName(op, idx))
    out.append(" {\n")
  }

  private def writeKernel(out: StringBuilder, op: OP_Zip, chunkIdx: Int, numChunks: Int) {
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
    out.append("val size = in1.size\n") //assume the first input to the Zip is the second input to the op and assume a "size" method exists
    out.append("var idx = size*") //var idx = size*chunkIdx/numChunks
    out.append(chunkIdx)
    out.append('/')
    out.append(numChunks)
    out.append('\n')
    out.append("val end = size*") //vl end = size*(chunkIdx+1)/numChunks
    out.append(chunkIdx+1)
    out.append('/')
    out.append(numChunks)
    out.append('\n')
    out.append("while (idx < end) {\n")
    out.append("in0(idx) = ") //assume the output of the Zip is the first input to the op
    out.append(op.function)
    out.append('(')
    out.append("in1(idx)") //in1 is the first input collection
    out.append(", in2(idx)") //in2 is the second input collection
    for (i <- 3 until inIdx) { //rest of inputs passed in as is (free vars)
      out.append(", in")
      out.append(i)
    }
    out.append(')')
    out.append('\n')
    out.append("idx += 1\n")
    out.append("}\n}\n")
  }

  private def kernelName(op: OP_Zip, idx: Int) = {
    "Zip_SMP_Array_" + op.id + "_Chunk_" + idx
  }

}
