package ppl.dsl.optiml.datastruct.scala

class VerticesImpl[V <: Vertex](len: Int)(implicit mV: ClassManifest[V]) extends VectorImpl[V](len, false) with Vertices[V] {

  def this(__data: Array[V])(implicit mV: ClassManifest[V]){
    this(0)
    // unfortunate Scala lack of support for calling non-primary superclass constructors
    _data = __data
    _length = _data.length
  }
}