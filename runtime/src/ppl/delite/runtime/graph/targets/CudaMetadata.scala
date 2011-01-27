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
  var inputs = Map[DeliteOP, OPData]()
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
    case other => system.error("unknown field: " + other)
  }

  def newInput(op: DeliteOP) = {
    val in = new OPData
    inputs += op -> in
    in
  }

  def newTemp = {
    val temp = new OPData
    temps ::= temp
    temp
  }

  def replaceInput(old: DeliteOP, op: DeliteOP) {
    if (inputs contains old) {
      val value = inputs(old)
      inputs -= old
      inputs += op -> value

      blockSizeX.replaceInput(old, op)
      blockSizeY.replaceInput(old, op)
      blockSizeZ.replaceInput(old, op)
      dimSizeX.replaceInput(old, op)
      dimSizeY.replaceInput(old, op)
      for (temp <- temps) temp.replaceInput(old, op)
      output.replaceInput(old, op)
    }
  }

}

final class OPData {

  var func: String = _
  var funcReturn: String = _
  var inputs: List[DeliteOP] = Nil
  var resultType: String = _

  private[targets] def replaceInput(old: DeliteOP, op: DeliteOP) {
    if (inputs contains old)
      inputs = inputs.patch(inputs.indexOf(old), List(op), 1)
  }

}
