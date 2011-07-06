package ppl.delite.runtime.graph.targets

import ppl.delite.runtime.graph.ops.DeliteOP

/**
 * Author: Kevin J. Brown
 * Date: Dec 4, 2010
 * Time: 10:48:52 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

final class CudaMetadata {

  var libCall: String = null
  val blockSizeX = new OPData
  val blockSizeY = new OPData
  val blockSizeZ = new OPData
  val dimSizeX = new OPData
  val dimSizeY = new OPData
  var inputs = Map[(DeliteOP, String), OPData]()
  var temps: List[OPData] = Nil
  var tempOps: List[DeliteOP] = Nil
  var output = new OPData

  def apply(field: String) = field match {
    case "gpuBlockSizeX" => blockSizeX
    case "gpuBlockSizeY" => blockSizeY
    case "gpuBlockSizeZ" => blockSizeZ
    case "gpuDimSizeX" => dimSizeX
    case "gpuDimSizeY" => dimSizeY
    case "output" => output
    case other => error("unknown field: " + other)
  }

  def newInput(op: DeliteOP, sym: String) = {
    val in = new OPData
    inputs += (op,sym) -> in
    in
  }

  def newTemp = {
    val temp = new OPData
    temps ::= temp
    temp
  }

  def replaceInput(old: DeliteOP, op: DeliteOP, sym: String) {
    if (inputs contains (old, sym)) {
      val value = inputs((old, sym))
      inputs -= Pair(old, sym)
      inputs += (op, sym) -> value

      blockSizeX.replaceInput(old, op, sym)
      blockSizeY.replaceInput(old, op, sym)
      blockSizeZ.replaceInput(old, op, sym)
      dimSizeX.replaceInput(old, op, sym)
      dimSizeY.replaceInput(old, op, sym)
      for (temp <- temps) temp.replaceInput(old, op, sym)
      output.replaceInput(old, op, sym)
    }
  }

}

final class OPData {

  var func: String = _
  var funcReturn: String = _
  var inputs: List[(DeliteOP,String)] = Nil
  var resultType: String = _

  private[targets] def replaceInput(old: DeliteOP, op: DeliteOP, sym: String) {
    if (inputs contains (old, sym))
      inputs = inputs.patch(inputs.indexOf((old,sym)), List((op,sym)), 1)
  }

}
