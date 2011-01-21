package ppl.delite.runtime.graph.ops

/**
 * Author: Kevin J. Brown
 * Date: 1/20/11
 * Time: 1:25 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class OP_Nested extends DeliteOP {

  def makeChunks(indices: Seq[Int]): Seq[OP_Nested]

  final def task = functionName

  private var functionName = ""

  def setExecutableName(name: String) {
    functionName = name
  }

  final def isDataParallel = false
  final def size = 0
  final def cost = 0

}
