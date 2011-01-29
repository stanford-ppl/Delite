package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: 1/23/11
 * Time: 5:36 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class OP_Executable(resultType: Map[Targets.Value,String]) extends DeliteOP {

  def supportsTarget(target: Targets.Value) = resultType.contains(target)
  def outputType(target: Targets.Value) = resultType(target)
  override def outputType: String = outputType(Targets.Scala)

}
