package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */



object IndexSetImpl {
  def apply[MO<:MeshObj:MeshObjConstruct](crs: CRS, e: MeshObj) : IndexSetImpl[MO] = {
    assert(crs != null)
    assert(e != null)
    apply(crs, e.internalId)
  }

  def apply[MO<:MeshObj:MeshObjConstruct](crs: CRS, n: Int) = {
    new IndexSetImpl[MO](crs.values, crs.row(n), crs.row(n+1))
  }
}

object CWIndexSetImpl {
  def apply[MO<:MeshObj:MeshObjConstruct](crs: CRS, e: MeshObj) : CWIndexSetImpl[MO] = apply(crs, e.internalId)

  def apply[MO<:MeshObj:MeshObjConstruct](crs: CRS, n: Int) = {
    new CWIndexSetImpl[MO](crs.values, crs.row(n), crs.row(n+1))
  }
}

class IndexSetImpl[MO <: MeshObj](data : Array[Int], start: Int, end : Int)(implicit moc: MeshObjConstruct[MO]) extends MeshSet[MO] {
  def apply(i : Int) : MO = MeshObjImpl[MO](data(start + i))(moc)
  override def size = end - start
}

// Direction bit should get reversed for CW
class CWIndexSetImpl[MO <: MeshObj](data: Array[Int], start: Int, end: Int)(implicit moc: MeshObjConstruct[MO]) extends IndexSetImpl[MO](data, start, end) {
  override def apply(i : Int) : MO = MeshObjImpl[MO](BitReverse.MASK ^ data(start + i))(moc)
}