package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */



object IndexSetImpl {
  def apply(crs: CRS, e: MeshObj) : IndexSetImpl = apply(crs, e.internalId)

  def apply(crs: CRS, n: Int) = {
    new IndexSetImpl(crs.values, crs.row(n), crs.row(n+1))
  }
}

object CWIndexSetImpl {
  def apply(crs: CRS, e: MeshObj) : CWIndexSetImpl = apply(crs, e.internalId)

  def apply(crs: CRS, n: Int) = {
    new CWIndexSetImpl(crs.values, crs.row(n), crs.row(n+1))
  }
}

class IndexSetImpl[MO <: MeshObj : Manifest](data : Array[Int], start: Int, end : Int) extends DeLisztSet[MO] {
  def apply(i : Int) : MO = MeshObjImpl[MO](data(start + i))
  def size = end - start
}

// Direction bit should get reversed for CW
class CWIndexSetImpl[MO <: MeshObj : Manifest](data: Array[Int], start: Int, end: Int) extends IndexSetImpl[MO](data, start) {
  def apply(i : Int) : MO = MeshObjImpl[MO](BitReverse.MASK ^ data(start + i))
}