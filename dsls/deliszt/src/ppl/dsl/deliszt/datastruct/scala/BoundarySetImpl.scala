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
  var sizes : Array[Int] = null
  var data : Array[Int] = null

  def apply(i : Int) = {
    if(data == null) {
      throw new RuntimeException("Boundary Set not finalized")
    }
    MeshObjImpl[MO](data(i))
  }
  def size = {
    if(data == null) {
      throw new RuntimeException("Boundary Set not finalized")
    }
    data.length
  }

  def add(i: Int) = {ids += i}
  def freeze() {
    data = ids.result().sorted
    ids = null

    val rangeBuilder = ArrayBuilder.make[(Int,Int)]()
    val sizeBuilder = ArrayBuilder.make[Int]()

    if(data.size > 0) {
      var start = data.apply(0)
      var cur = start + 1
      var i = 1
      while(i < data.size) {
        val next = data.apply(i)
        if(next != cur) {
          rangeBuilder += (start,cur)
          sizeBuilder += cur - start
          cur = start = next
        }

        cur++
      }
      rangeBuilder += ((start,cur))
      sizeBuilder += cur - start
    }

    ranges = rangeBuilder.result()
    sizes = sizeBuilder.result()
  }

  def add(start: Int, end: Int) = {}
}
