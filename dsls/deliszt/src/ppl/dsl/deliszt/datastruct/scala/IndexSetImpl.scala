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
    val start = crs.row(Mesh.internal(n))
    val end = crs.row(Mesh.internal(n)+1)
    val size = end - start
    new IndexSetImpl(crs.values, size, start, end, Mesh.FORWARD)
  }
}

object CWIndexSetImpl {
  def apply(crs: CRS, n: Int) = {
    val start = crs.row(Mesh.internal(n)+1) - 1
    val end = crs.row(Mesh.internal(n)) - 1
    val size = start - end
    new IndexSetImpl(crs.values, size, start, end, Mesh.REVERSE)
  }
}

object DirectedIndexSetImpl {
  def apply(crs: CRS, n: Int, dir: Int) = {
    if((n >>> Mesh.SHIFT) == dir) {
      IndexSetImpl(crs, n)
    }
    else {
      CWIndexSetImpl(crs, n)
    }
  }
}

class IndexSetImpl(data : Array[Int], override val size: Int, start: Int, end: Int, dir: Int) extends MeshSet {
  def apply(i : Int) : Int = data(start + i*dir) ^ (dir & Mesh.DMASK)
}