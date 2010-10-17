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

  val coll: Data[A]

  def func: (A,A) => A

  def task {
    var res = coll(0)
    var i = 1
    while (i < coll.size) {
      res = func(res, coll(i))
      i += 1
    }
  }

}