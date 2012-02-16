package ppl.delite.runtime.graph.ops

/**
 * Author: Kevin J. Brown
 * Date: 1/23/11
 * Time: 5:36 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class OP_Executable extends DeliteOP {
  def opName: String = id
}
