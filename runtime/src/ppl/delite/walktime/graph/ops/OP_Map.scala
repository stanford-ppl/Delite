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

  val coll: Data[A]

  val out: Data[B]

  def func: A => B

  def task {
    var i = 0
    while (i < coll.size) {
      out(i) = func(coll(i))
      i += 1
    }
  }

}