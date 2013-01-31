package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets._


//TODO: make this more generic... some notion of a partionable op?
class OP_FileReader(val id: String, val function: String, private[graph] var outputTypesMap: Map[Targets.Value,Map[String,String]], private[graph] var inputTypesMap: Map[Targets.Value,Map[String,String]]) extends OP_Executable {

  final def isDataParallel = false

  def task = kernelName

  private var kernelName: String = function

  def setKernelName(name: String) {
    kernelName = name
  }

  def cost = 0
  def size = 0

}
