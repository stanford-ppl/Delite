package ppl.dsl.deliszt.datastruct.scala

object Vec3FieldImpl {
  
  def ofCell() : Vec3FieldImpl = {
      new Vec3FieldImpl(Mesh.mesh.ncells, new Array[Double](3*Mesh.mesh.ncells))
  }
  
  def ofEdge() : Vec3FieldImpl = {
      new Vec3FieldImpl(Mesh.mesh.nedges, new Array[Double](3*Mesh.mesh.nedges))
  }
  
  def ofFace() : Vec3FieldImpl = {
      new Vec3FieldImpl(Mesh.mesh.nfaces, new Array[Double](3*Mesh.mesh.nfaces))
  }
  
  def ofVertex() : Vec3FieldImpl = {
      new Vec3FieldImpl(Mesh.mesh.nvertices, new Array[Double](3*Mesh.mesh.nvertices))      
  }

  def cellWithConst[T:ClassManifest](v: T) : Field[T] = {
      val f = Vec3FieldImpl.ofCell()
      f.fill(v.asInstanceOf[Vec[Double]])
      f.asInstanceOf[Field[T]]
  }
  
  def edgeWithConst[T:ClassManifest](v: T) : Field[T] = {
      val f = Vec3FieldImpl.ofEdge()
      f.fill(v.asInstanceOf[Vec[Double]])
      f.asInstanceOf[Field[T]]
  }
  
  def faceWithConst[T:ClassManifest](v: T) : Field[T] = {
      val f = Vec3FieldImpl.ofFace()
      f.fill(v.asInstanceOf[Vec[Double]])
      f.asInstanceOf[Field[T]]
  }
  
  def vertexWithConst[T:ClassManifest](v: T) : Field[T] = {
      val f = Vec3FieldImpl.ofVertex()
      f.fill(v.asInstanceOf[Vec[Double]])
      f.asInstanceOf[Field[T]]
  }
}


class Vec3FieldImpl(val numVec:Int, val data: Array[Double]) extends Field[Vec[Double]] {
  def apply(idx: Int) = {
    new Vec3ViewImpl(data, Mesh.internal(idx)*3)
  }
  
  override def raw_apply(idx: Int, elem: Int) = {
    val offset = idx*3//Mesh.internal(idx)*3
    data(offset+elem)
  }

  def update(idx: Int, x: Vec[Double]) = {
    val offset = idx*3//Mesh.internal(idx)*3
    data(offset) = x(0)
    data(offset+1) = x(1)
    data(offset+2) = x(2)
  }

  override def raw_update(idx: Int, elem: Int, v: Double) = {
    val offset = Mesh.internal(idx)*3
    data(offset+elem) = v
  }
  
  def unsafeSetData(xs: Array[Double], len: Int) = throw new UnsupportedOperationException()
  
  def size = numVec

  def fill(v: Vec[Double]) {
    var i = 0
    while(i < 3*size) {
      data(i) = v(0)
      data(i+1) = v(1)
      data(i+2) = v(2)
      i += 3
    }
  }
}
  
class Vec3ViewImpl(val data: Array[Double], val idx: Int) extends Vec[Double] {
  
  def apply(n: Int) = {
    data(idx + n)
  }
  def update(n: Int, v: Double) = {
    data(idx + n) = v
  }
  override val size = 3
  
  def cloneL = {
    new Vec3Impl[Double](data(idx), data(idx+1), data(idx+2))
  }
  
  override def foreach[U](f: Double => U) = {
    var i = 0
    while (i < size) {
      f(data(idx + i))
      i += 1
    }
  }
}
