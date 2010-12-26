package ppl.delite.runtime.graph.ops

class OP_Condition(val id: String, val thenOp: DeliteOP, val elseOp: DeliteOP) extends DeliteOPBase {

  def nested = null
  def cost = 0
  def size = 0
}