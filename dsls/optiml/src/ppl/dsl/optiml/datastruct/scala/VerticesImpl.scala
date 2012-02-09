package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

class Vertices[V <: Vertex](len: Int)(implicit mV: Manifest[V]) extends DenseVector[V](len, false) { //with Vertices[V] {

  def this(__data: Array[V])(implicit mV: Manifest[V]){
    this(0)
    // unfortunate Scala lack of support for calling non-primary superclass constructors
    _data = __data
    _length = _data.length
  }
  
  override def Clone: Vertices[V] = { 
    val v = new Vertices[V](0)
    v._data = _data.clone
    v 
  }
  
  def printBeliefs {
    for(i <- 0 until _length) {
      val data = _data(i).asInstanceOf[MessageVertex].data.asInstanceOf[DenoiseVertexData]
      print(data.id + " " + System.identityHashCode(data.belief) + " [")
      
      for(j <- 0 until data.belief._length) {
        print(" " + data.belief._data(j))
      }
      
      println("]")
    }
  }
  
  def toList = _data.toList
}