package ppl.delite.runtime.scheduler

import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.codegen.kernels.scala._
import ppl.delite.runtime.codegen.kernels._
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.Config

/**
 * Author: Kevin J. Brown
 * Date: 12/18/10
 * Time: 6:04 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object OpHelper {

  def expand(op: DeliteOP, numChunks: Int, graph: DeliteTaskGraph) = op match {
    case multi: OP_MultiLoop => MultiLoop_SMP_Array_Header_Generator.makeHeader(multi, numChunks, graph)
    case foreach: OP_Foreach => Foreach_SMP_Array_Header_Generator.makeHeader(foreach, graph)
    case single: OP_Single => error("OP Single cannot be expanded")
    case external: OP_External => error("OP External cannot be expanded")
    case other => error("OP type not recognized: " + other.getClass.getSimpleName)
  }

  def split(op: DeliteOP, chunkIdx: Int, numChunks: Int, kernelPath: String) = op match {
    case multi: OP_MultiLoop => MultiLoop_SMP_Array_Generator.makeChunk(multi, chunkIdx, numChunks, kernelPath)
    case foreach: OP_Foreach => Foreach_SMP_Array_Generator.makeChunk(foreach, chunkIdx, numChunks, kernelPath)
    case single: OP_Single => error("OP Single cannot be split")
    case external: OP_External => error("OP External cannot be split")
    case other => error("OP type not recognized: " + other.getClass.getSimpleName)
  }

  def splitGPU(op: DeliteOP) = op match {
    case multi: OP_MultiLoop => if(Config.useOpenCL) opencl.MultiLoop_GPU_Array_Generator.makeChunk(multi)
                                else cuda.MultiLoop_GPU_Array_Generator.makeChunk(multi)
    case foreach: OP_Foreach => foreach.setKernelName(foreach.function); foreach
    case single: OP_Single => error("OP Single cannot be split")
    case external: OP_External => error("OP External cannot be split")
    case other => error("OP type not recognized: " + other.getClass.getSimpleName)
  }
}
