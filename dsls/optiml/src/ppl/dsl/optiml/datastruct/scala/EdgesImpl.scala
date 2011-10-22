package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

class EdgesImpl[E <: Edge](len: Int)(implicit mE: ClassManifest[E]) extends DenseVector[E](len, false) with Edges[E] {

  def this(__data: Array[E])(implicit mE: ClassManifest[E]){
    this(0)
    // unfortunate Scala lack of support for calling non-primary superclass constructors
    _data = __data
    _length = _data.length
  }
}