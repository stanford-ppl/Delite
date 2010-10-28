package ppl.delite.walktime.graph.ops

import ppl.delite.data.Data

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:27:00 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class OP_Reduce[A] extends DeliteOP {

  final def isDataParallel = true

  val coll: Data[A]

  def func: (A,A) => A

  def task = in => println("OP_Reduce")

}