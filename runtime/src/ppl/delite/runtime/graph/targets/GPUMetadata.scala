package ppl.delite.runtime.graph.targets

import ppl.delite.runtime.graph.ops.DeliteOP
import scala.collection.mutable.{HashMap,ListBuffer}

/**
 * Author: Kevin J. Brown
 * Date: Dec 4, 2010
 * Time: 10:48:52 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait GPUMetadata {

  var libCall: String = null

  //var inputs = Map[(DeliteOP, String), OPData]()
  var temps = List[TempAlloc]()
  var outputs: List[(OPData, String)] = Nil
  val mapping = new ListBuffer[Mapping]()

  /*
  def newInput(op: DeliteOP, sym: String) = {
    val in = new OPData("",Map())
    inputs += (op,sym) -> in
    in
  }
  */
  
  def newTemp(sym: String, tp: String, size: String) = {
    val temp = TempAlloc(sym,tp,size)
    temps ::= temp
  }

  def newOutput(sym: String, elemType: String, types: Map[String,String], funcs: Map[String,List[String]]) = {
    val output = new OPData(elemType, types, funcs)
    outputs ::= (output, sym)
    output
  }
  
  /*
  def replaceInput(old: DeliteOP, op: DeliteOP, sym: String) {
    if (inputs contains (old, sym)) {
      val value = inputs((old, sym))
      inputs -= Pair(old, sym)
      inputs += (op, sym) -> value

      //for ((temp,name) <- temps) temp.replaceInput(old, op, sym)
      for ((output,name) <- outputs) output.replaceInput(old, op, sym)
    }
  }
  */

}

case class Mapping(level: Int, dim: String, size: Int, spanTpe: String, spanSize: String)

final class OPData(val elemType: String, val types: Map[String,String], val funcs: Map[String, List[String]]) {
  
  def getType(tpe: String): String = types.get(tpe) match {
    case Some(tpeName) => tpeName
    case _ => throw new RuntimeException("Cannot find the type " + tpe)
  }

  def getInputs(name: String): List[String] = funcs.get(name) match {
    case Some(list) => list
    case _ => throw new RuntimeException("Cannot find the func name " + name)
  }

  //TODO: Might want to separate for Cuda and OpenCL
  var objFields: Map[String,String] = _

  /*
  private[targets] def replaceInput(old: DeliteOP, op: DeliteOP, sym: String) {
    if (inputs contains (old, sym))
      inputs = inputs.patch(inputs.indexOf((old,sym)), List((op,sym)), 1)
  }
  */

}

case class TempAlloc(sym:String, tp:String, size:String)
