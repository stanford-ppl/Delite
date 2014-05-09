package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets._

class OP_External(val id: String, kernel: String, private[graph] var outputTypesMap: Map[Targets.Value, Map[String,String]]) extends OP_Executable {

  final def isDataParallel = false

  def task = kernel

  def cost = 0
  def size = 0

}
