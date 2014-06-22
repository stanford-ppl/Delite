package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets._
import ppl.delite.runtime.graph._


//TODO: make this more generic... some notion of a partitionable op?
class OP_FileReader(val id: String, val function: String, private[graph] var outputTypesMap: Map[Targets.Value,Map[String,String]]) extends OP_Executable {

  final def isDataParallel = false

  override def partition = {
    val outSym = getOutputs.head
    if (stencilMap contains outSym) {
      val outStencil = stencil(outSym)
      if (outStencil == All || outStencil == Empty) Local
      else Distributed(Set(id))
    }
    else { //no partitionable op consumes this, just run locally
      Local
    }
  }

  def task = kernelName

  private var kernelName: String = function

  def setKernelName(name: String) {
    kernelName = name
  }

  def cost = 0
  def size = 0

}
