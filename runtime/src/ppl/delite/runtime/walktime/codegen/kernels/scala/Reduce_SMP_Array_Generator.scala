package ppl.delite.walktime.codegen.kernels.scala

import ppl.delite.runtime.walktime.graph.ops.OP_Reduce
import ppl.delite.runtime.walktime.codegen.ScalaCompile

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

  def makeChunk(op: OP_Reduce[_], chunkIdx: Int, numChunks: Int): OP_Reduce[_] = {
    val chunk = if (chunkIdx == 0) op else op.chunk
    ScalaCompile.addSource(makeKernel(chunk, op, chunkIdx, numChunks))
    chunk
  }

  private def makeKernel(op: OP_Reduce[_], master: OP_Reduce[_], chunkIdx: Int, numChunks: Int) = {
    val out = new StringBuilder

    //update the op with this kernel
    updateOP(op, master, chunkIdx)

    //the header
    writeHeader(out, master, chunkIdx)

    //the kernel
    writeKernel(out, op, master, chunkIdx, numChunks)

    //the first (primary) reduction
    writeReduce(out, op, master.outputType, chunkIdx, numChunks)

    //the communication
    if (chunkIdx != 0) { //chunk 0 never needs to send result
      writeSync(out, master.outputType)
    }

    //the footer
    out.append('}')
    out.append('\n')

    out.toString
  }

  private def updateOP(op: OP_Reduce[_], master: OP_Reduce[_], idx: Int) {
    op.setKernelName(kernelName(master, idx))
  }

  private def writeHeader(out: StringBuilder, master: OP_Reduce[_], idx: Int) {
    out.append("object ")
    out.append(kernelName(master, idx))
    out.append(" {\n")
  }

  private def writeKernel(out: StringBuilder, op: OP_Reduce[_], master: OP_Reduce[_], chunkIdx: Int, numChunks: Int) {
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
    out.append("): ")
    out.append(op.outputType) //the chunk output type
    out.append(" = {\n")

    //tree reduction
    //first every chunk performs its primary reduction
    out.append("var acc = collReduce(in0")
    writeFreeVars(out, inIdx)
    out.append(')')
    out.append('\n')

    var half = chunkIdx
    var step = 1
    while ((half % 2 == 0) && (step <= numChunks/2)) { //half the chunks quit each iteration
      half = half / 2
      val neighbor = chunkIdx + step //the index of the chunk to reduce with
      step *= 2

      out.append("acc = ")
      out.append(op.function)
      out.append("(acc, ")
      out.append(kernelName(master, neighbor))
      out.append(".get")
      writeFreeVars(out,inIdx)
      out.append(')')
      out.append('\n')
    }
    if (chunkIdx == 0) { //chunk 0 returns result
      out.append("acc\n")
    }
    else { //other chunks store result
      out.append("set(acc)\n")
    }
    out.append('}')
    out.append('\n')
  }

  private def writeReduce(out: StringBuilder, op: OP_Reduce[_], outputType: String, chunkIdx: Int, numChunks: Int) {
    out.append("private def collReduce(")
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
    out.append("): ")
    out.append(outputType) //the master output type
    out.append(" = {\n")

    out.append("val size = in0.size\n") //asume the input collection is the first input to the op and assume a "size" method exits
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
    out.append("var acc = in0(idx)\n")
    out.append("idx += 1\n")
    out.append("while (idx < end) {\n")
    out.append("acc = ")
    out.append(op.function)
    out.append("(acc, in0(idx)")
    writeFreeVars(out, inIdx)
    out.append(')')
    out.append('\n')
    out.append("idx += 1\n")
    out.append("}\n acc\n }\n") //return acc

  }

  private def writeFreeVars(out: StringBuilder, numVars: Int) {
    for (i <- 1 until numVars) { //rest of inputs passed in as is (free vars)
      out.append(", in")
      out.append(i)
    }
  }

  private def writeSync(out: StringBuilder, outputType: String) {
    out.append("@volatile private var notReady: Boolean = true\n")
    out.append("private var _result: ")
    out.append(outputType)
    out.append(" = _\n")

    out.append("def get: ")
    out.append(outputType)
    out.append(" = { while (notReady) { }; _result }\n")

    out.append("private def set(result: ")
    out.append(outputType)
    out.append(") { _result = result; notReady = false }\n")
  }

  private def kernelName(master: OP_Reduce[_], idx: Int) = {
    "Reduce_SMP_Array_" + master.id + "_Chunk_" + idx
  }

}
