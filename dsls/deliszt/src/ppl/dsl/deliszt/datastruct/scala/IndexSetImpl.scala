package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object BitReverse {
  val MASK = 0x80000000
}

class IndexSetImpl[MO <: MeshObj : Manifest](data : Array[Int], start: Int, end : Int) extends DeLisztSet[MO] {
  def apply(i : Int) = new MO(data(start + i))
  def size = end - start
}

// Direction bit should get reversed for CW
class CWIndexSetImpl[MO <: MeshObj : Manifest] extends DeLisztSet[MO] {
  def apply(i : Int) = new MO(BitReverse.MASK ^ data(start + i))
  def size = end - start
}