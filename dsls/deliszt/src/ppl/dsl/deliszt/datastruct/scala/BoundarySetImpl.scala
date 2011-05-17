package ppl.dsl.deliszt.datastruct.scala

import collection.mutable.ArrayBuilder

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 05/17/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class BoundarySetImpl[MO<:MeshObj:Manifest] extends MeshObjSet[MO] {
  var ids = ArrayBuilder.make[Int]()
  var ranges : Array[(Int,Int)] = null

  def apply(i : Int) = null
  def size = 0

  def add(i: Int) = {ids += i}
  def freeze() = {
    val result = ids.result().sorted
    ids = null

    val rangeBuilder = ArrayBuilder.make[(Int,Int)]()

    if(result.size > 0) {
      var start = result.apply(0)
      var cur = start + 1
      var i = 1
      while(i < result.size) {
        val next = result.apply(i)
        if(next != cur) {
          rangeBuilder += (start,cur)
          cur = start = next
        }

        cur++
      }
      rangeBuilder += (start,cur)
    }

    ranges = rangeBuilder.result()
  }
}