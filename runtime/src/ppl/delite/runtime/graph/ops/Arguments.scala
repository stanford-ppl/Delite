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
  var args: Seq[Any] = Nil
  var staticDataMap: Map[String,_] = _

  def getArg[T](idx: Int): T = {
    if (args.length > idx) args(idx).asInstanceOf[T]
    else throw new RuntimeException("Insufficient input arguments supplied: " + args.mkString(","))
  }
  
  def staticData[T](id: String): T = {
    staticDataMap(id).asInstanceOf[T]
  }
}

final class Arguments(val id: String, val argIdx: Int, var outputTypesMap: Map[Targets.Value,Map[String,String]]) extends OP_Executable {

  def isDataParallel = false

  def task = if (scheduledOn(Targets.Cpp)) "cppArgsGet" //TODO: multi-arg Cpp support
             else "ppl.delite.runtime.graph.ops.ArgsKernel" + argIdx

  def cost = 0
  def size = 0

  assert(argIdx < 5, "Cannot currently support DEG with more than 5 Arguments")

}

object ArgsKernel0 {
  def apply[T](): T = Arguments.getArg(0)
}
object ArgsKernel1 {
  def apply[T](): T = Arguments.getArg(1)
}
object ArgsKernel2 {
  def apply[T](): T = Arguments.getArg(2)
}
object ArgsKernel3 {
  def apply[T](): T = Arguments.getArg(3)
}
object ArgsKernel4 {
  def apply[T](): T = Arguments.getArg(4)
}
