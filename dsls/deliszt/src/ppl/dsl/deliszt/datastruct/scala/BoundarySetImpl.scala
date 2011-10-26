package ppl.dsl.deliszt.datastruct.scala

import collection.mutable.ArrayBuilder
import collection.immutable.Range

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 05/17/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class BoundarySetImpl extends BoundarySet {
  val _ranges = ArrayBuilder.make[Range]
  var data : Array[Int] = null

  def apply(i : Int) = {
    if(data == null) {
      throw new RuntimeException("Boundary Set not finalized")
    }
    data(i)
  }
  
  override def size = {
    if(data == null) {
      throw new RuntimeException("Boundary Set not finalized")
    }
    data.length
  }

  def freeze() {
    val ids = ArrayBuilder.make[Int]

    for(range <- _ranges.result) {
      ids ++= range
    }

    data = ids.result
  }

  def add(start: Int, end: Int) {
    _ranges += start until end
  }
}
