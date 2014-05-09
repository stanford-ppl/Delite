package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.Config

/**
 * Author: Kevin J. Brown
 * Date: Nov 30, 2010
 * Time: 3:56:38 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Arguments {
  var args: List[Any] = Nil
  var staticDataMap: Map[String,_] = _
  
  def staticData[T](id: String): T = {
    staticDataMap(id).asInstanceOf[T]
  }
}

final class Arguments(val id: String, var outputTypesMap: Map[Targets.Value,Map[String,String]]) extends OP_Executable {

  def isDataParallel = false

  def task = if (scheduledOn(Targets.Cpp)) "cppArgsGet" 
             else "ppl.delite.runtime.graph.ops.ArgsKernel"

  def cost = 0
  def size = 0

}

object ArgsKernel {
  def apply[T](): T = Arguments.args(0).asInstanceOf[T]
}
