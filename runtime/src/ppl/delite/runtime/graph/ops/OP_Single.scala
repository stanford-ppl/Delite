package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets._

/**
 * Author: Kevin J. Brown
 * Date: Nov 14, 2010
 * Time: 10:12:48 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class OP_Single(val id: String, kernel: String, resultType: Map[Targets.Value, String]) extends OP_Executable(resultType) {

  final def isDataParallel = false

  def task = kernel

  def cost = 0
  def size = 0

}
