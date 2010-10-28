package ppl.delite.walktime.graph.ops

import ppl.delite.data.Data

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:26:52 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class OP_Map[A,B] extends DeliteOP {

  final def isDataParallel = true

  val coll: Data[A]

  val out: Data[B]

  def func: A => B

  //TOOD: still need to decide how chunking is executed in the kernel model (how much is on the codegen side, how much on the runtime side)
  def task = in => println("OP_Map")

}