package ppl.delite.runtime.scheduler

import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.codegen.kernels.scala._
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.targets.Targets

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
    case map: OP_Map => Map_SMP_Array_Header_Generator.makeHeader(map, graph)
    case reduce: OP_Reduce => Reduce_SMP_Array_Header_Generator.makeHeader(reduce, numChunks, graph)
    case mapReduce: OP_MapReduce => MapReduce_SMP_Array_Header_Generator.makeHeader(mapReduce, numChunks, graph)
    case zip: OP_Zip => Zip_SMP_Array_Header_Generator.makeHeader(zip, graph)
    case foreach: OP_Foreach => Foreach_SMP_Array_Header_Generator.makeHeader(foreach, graph)
    case single: OP_Single => error("OP Single cannot be expanded")
    case other => error("OP type not recognized: " + other.getClass.getSimpleName)
  }

  def split(op: DeliteOP, chunkIdx: Int, numChunks: Int, kernelPath: String) = op match {
    case map: OP_Map => Map_SMP_Array_Generator.makeChunk(map, chunkIdx, numChunks, kernelPath)
    case reduce: OP_Reduce => Reduce_SMP_Array_Generator.makeChunk(reduce, chunkIdx, numChunks, kernelPath)
    case zip: OP_Zip => Zip_SMP_Array_Generator.makeChunk(zip, chunkIdx, numChunks, kernelPath)
    case mapReduce: OP_MapReduce => MapReduce_SMP_Array_Generator.makeChunk(mapReduce, chunkIdx, numChunks, kernelPath)
    case foreach: OP_Foreach => Foreach_SMP_Array_Generator.makeChunk(foreach, chunkIdx, numChunks, kernelPath)
    case single: OP_Single => error("OP Single cannot be split")
    case other => error("OP type not recognized: " + other.getClass.getSimpleName)
  }

  def splitGPU(op: DeliteOP) = op match {
    case map: OP_Map => map.setKernelName(map.function); map
    case reduce: OP_Reduce => reduce.setKernelName(reduce.function); reduce
    case zip: OP_Zip => zip.setKernelName(zip.function); zip
    case mapReduce: OP_MapReduce => mapReduce.setKernelName(mapReduce.function); mapReduce
    case foreach: OP_Foreach => foreach.setKernelName(foreach.function); foreach
    case single: OP_Single => error("OP Single cannot be split")
    case other => error("OP type not recognized: " + other.getClass.getSimpleName)
  }

  def makeVariant(op: DeliteOP, graph: DeliteTaskGraph) {
    op.variant.parse()

    val id = op.id.drop(graph._id.length)
    var resultMap = Map[Targets.Value,String]()
    for (t <- Targets.values) if (op.supportsTarget(t)) resultMap += t -> op.outputType(t)

    //add variant scoping
    val beginOp = new OP_BeginVariantScope(op.variant._id+id+"vb", resultMap)
    val endOp = new OP_EndVariantScope(op.variant._id+id+"v", resultMap, op.variant.result)

    //remove super op from graph
    graph._ops += id -> endOp
    for (dep <- op.getDependencies) {
      dep.removeConsumer(op)
    }
    //consumers need to use result op for the nested graph rather than the outer op
    for (c <- op.getConsumers) {
      c.replaceDependency(op, endOp)
      if (c.getInputs.contains(op)) c.replaceInput(op, endOp)
    }

    //beginning depends on all superOp inputs
    for (dep <- op.getDependencies) {
      beginOp.addDependency(dep)
    }

    val innerOps = op.variant.ops.toList.filter(_.isInstanceOf[OP_Control]).flatMap(o => o.asInstanceOf[OP_Control].bodyOps)
    val outerOps = op.variant.ops.toList.filterNot(innerOps contains)
    for (op <- outerOps) {
      op.addDependency(beginOp)
      beginOp.addConsumer(op)
      op.addConsumer(endOp)
      endOp.addDependency(op)
    }

    op.variant._ops += id+"vb" -> beginOp
    op.variant._ops += id+"v" -> endOp
  }
}
