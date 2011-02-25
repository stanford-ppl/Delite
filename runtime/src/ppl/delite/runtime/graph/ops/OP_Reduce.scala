package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.graph.DeliteTaskGraph

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:27:00 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class OP_Reduce(val id: String, func: String, protected val outputTypesMap: Map[Targets.Value,Map[String,String]]) extends OP_Executable {

  final def isDataParallel = true

  def task = kernelName

  private var kernelName: String = ""

  def setKernelName(name: String) {
    kernelName = name
  }

  def function = func

  /**
   * Since the semantics of Reduce are to return a T, all chunks are necessarily complete before the final T can be returned
   * Therefore additional chunks do not need edges to consumers
   * Chunks require same dependency & input lists
   */
  def chunk(i: Int): OP_Reduce = {
    val r = new OP_Reduce(id+"_"+i, function, Targets.unitTypes(id+"_"+i, outputTypesMap)) //chunks all return Unit
    r.dependencies = dependencies //lists are immutable so can be shared
    r.inputList = inputList
    for (dep <- getDependencies) dep.addConsumer(r)
    r
  }

  def header(kernel: String, graph: DeliteTaskGraph): OP_Single = {
    val h = new OP_Single(id+"_h", kernel, Map(Targets.Scala->Map(id+"_h"->kernel,"functionReturn"->kernel)))
    //header assumes all inputs of map
    h.dependencies = dependencies
    h.inputList = inputList
    h.addConsumer(this)
    for (dep <- getDependencies) dep.replaceConsumer(this, h)
    //map consumes header, map's consumers remain unchanged
    dependencies = Set(h)
    inputList = List((h,h.id))
    graph.registerOp(h)
    h
  }

  def cost = 0
  def size = 0

}
