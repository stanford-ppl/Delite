package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */



object IndexSetImpl {
  def apply[MO<:MeshObj:Manifest](crs: CRS, e: MeshObj) : IndexSetImpl[MO] = apply(crs, e.internalId)

  def apply[MO<:MeshObj:Manifest](crs: CRS, n: Int) = {
    new IndexSetImpl[MO](crs.values, crs.row(n), crs.row(n+1))
  }
}

object CWIndexSetImpl {
  def apply[MO<:MeshObj:Manifest](crs: CRS, e: MeshObj) : CWIndexSetImpl[MO] = apply(crs, e.internalId)

  def apply[MO<:MeshObj:Manifest](crs: CRS, n: Int) = {
    new CWIndexSetImpl[MO](crs.values, crs.row(n), crs.row(n+1))
  }
}

class IndexSetImpl[MO <: MeshObj : Manifest](data : Array[Int], start: Int, end : Int) extends DeLisztSet[MO] {
  def apply(i : Int) : MO = MeshObjImpl[MO](data(start + i))
  def size = end - start
}

// Direction bit should get reversed for CW
class CWIndexSetImpl[MO <: MeshObj : Manifest](data: Array[Int], start: Int, end: Int) extends IndexSetImpl[MO](data, start, end) {
  def apply(i : Int) : MO = MeshObjImpl[MO](BitReverse.MASK ^ data(start + i))
}