package ppl.delite.runtime.scheduler

import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.codegen.kernels.scala._
import ppl.delite.runtime.codegen.kernels._
import cpp.{CppMultiLoopHeaderGenerator, CppMultiLoopGenerator}
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

  def expand(op: DeliteOP, numChunks: Int, graph: DeliteTaskGraph, target: Targets.Value) = op match {
    case multi: OP_MultiLoop => target match {
      case Targets.Scala => (new ScalaMultiLoopHeaderGenerator(multi,numChunks,graph)).makeHeader()
      case Targets.Cpp => (new CppMultiLoopHeaderGenerator(multi,numChunks,graph)).makeHeader()
    }
    case foreach: OP_Foreach => Foreach_SMP_Array_Header_Generator.makeHeader(foreach, graph)
    case single: OP_Single => sys.error("OP Single cannot be expanded")
    case external: OP_External => sys.error("OP External cannot be expanded")
    case other => sys.error("OP type not recognized: " + other.getClass.getSimpleName)
  }

  def split(op: DeliteOP, numChunks: Int, graph: DeliteTaskGraph, target: Targets.Value): Seq[DeliteOP] = op match {
    case multi: OP_MultiLoop => target match {
      case Targets.Scala => ScalaMultiLoopGenerator.makeChunks(multi, numChunks, graph)
      case Targets.Cpp => CppMultiLoopGenerator.makeChunks(multi, numChunks, graph)
      case Targets.Cuda => assert(numChunks == 1); Seq(cuda.MultiLoop_GPU_Array_Generator.makeChunk(multi))
      case Targets.OpenCL => assert(numChunks == 1); Seq(opencl.MultiLoop_GPU_Array_Generator.makeChunk(multi))
    }
    case foreach: OP_Foreach => scheduledTarget(op) match {
      case Targets.Scala => for (chunkIdx <- 0 until numChunks) yield Foreach_SMP_Array_Generator.makeChunk(foreach, chunkIdx, numChunks, graph)
      case _ => foreach.setKernelName(foreach.function); Seq(foreach)
    }
    case single: OP_Single => error("OP Single cannot be split")
    case external: OP_External => error("OP External cannot be split")
    case other => error("OP type not recognized: " + other.getClass.getSimpleName)
  }

  def scheduledTarget(op: DeliteOP):Targets.Value = scheduledTarget(op.scheduledResource)

  def scheduledTarget(location: Int):Targets.Value = {
    if(location < Config.numThreads) Targets.Scala
    else if (location < Config.numThreads+Config.numCpp) Targets.Cpp
    else if (location < Config.numThreads+Config.numCpp+Config.numCuda) Targets.Cuda
    else if (location < Config.numThreads+Config.numCpp+Config.numCuda+Config.numOpenCL) Targets.OpenCL
    else throw new RuntimeException("Cannot find a target for resource ID " + location)
  }

  def remote(op: DeliteOP, graph: DeliteTaskGraph) = op match {
    case multi: OP_MultiLoop => RPC_Generator.makeKernel(multi, graph)
    case file: OP_FileReader => RPC_Generator.makeKernel(file, graph)
    case single: OP_Single => sys.error("OP Single cannot be executed remotely")
    case other => sys.error("OP type not recognized: " + other.getClass.getSimpleName)
  }
}
