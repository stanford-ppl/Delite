package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

class VerticesImpl[V <: Vertex](len: Int)(implicit mV: ClassManifest[V]) extends DenseVector[V](len, false) with Vertices[V] {

  def this(__data: Array[V])(implicit mV: ClassManifest[V]){
    this(0)
    // unfortunate Scala lack of support for calling non-primary superclass constructors
    _data = __data
    _length = _data.length
  }
  
  def cloneV : Vertices[V] = { val v = new VerticesImpl[V](0); v.insertAll(0, this); v }
  
  def printBeliefs {
    for(i <- 0 until _length) {
      val data = apply(i).asInstanceOf[MessageVertex].data.asInstanceOf[DenoiseVertexData]
      print(data.id + " " + System.identityHashCode(data.belief) + " [")
      
      for(j <- 0 until data.belief.length) {
        print(" " + data.belief(j))
      }
      
      println("]")
    }
  }
}