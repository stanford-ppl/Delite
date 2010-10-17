package ppl.delite.walktime.graph

import ops.DeliteOP

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 1:06:03 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

abstract class DeliteTaskGraph {

  def root : DeliteOP

}