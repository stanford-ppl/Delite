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
  var args: Array[String] = _
  var staticDataMap: Map[String,_] = _
  
  def staticData[T](id: String): T = {
    //System.err.println("*** lookup static data with id " + id + " in " + staticDataMap)    
    staticDataMap(id).asInstanceOf[T]
  }
}

final class Arguments(val id: String) extends OP_Executable {

  var outputTypesMap = Map(Targets.Scala->Map(id -> "Array[java.lang.String]", "functionReturn"->"Array[java.lang.String]"))
  if (Config.numCpp > 0)
    outputTypesMap += Targets.Cpp->Map(id -> "cppDeliteArray< string >", "functionReturn"->"cppDeliteArray< string >")
  var inputTypesMap: Map[Targets.Value, Map[String,String]] = Map()

  def isDataParallel = false

  def task = if (scheduledOn(Targets.Cpp)) "cppArgsGet" 
             else "ppl.delite.runtime.graph.ops.ArgsKernel"

  def cost = 0
  def size = 0

}

object ArgsKernel {
  def apply(): Array[String] = Arguments.args
}
