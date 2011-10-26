package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */



object IndexSetImpl {
  def apply(crs: CRS, n: Int) = {
    new IndexSetImpl(crs.values, crs.row(Mesh.internal(n)), crs.row(n+1))
  }
}

object CWIndexSetImpl {
  def apply(crs: CRS, n: Int) = {
    new CWIndexSetImpl(crs.values, crs.row(Mesh.internal(n)), crs.row(n+1))
  }
}

class IndexSetImpl(data : Array[Int], start: Int, end : Int) extends MeshSet {
  def apply(i : Int) : Int = data(start + i)
  override def size = end - start
}

// Direction bit should get reversed for CW
class CWIndexSetImpl(data: Array[Int], start: Int, end: Int) extends IndexSetImpl(data, start, end) {
  override def apply(i : Int) : Int = BitReverse.MASK ^ data(end - i - 1)
}