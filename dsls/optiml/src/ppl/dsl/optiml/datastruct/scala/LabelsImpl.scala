package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._

class LabelsImpl[T:Manifest](xs: DenseVector[T]) extends DenseVector[T](xs.data, false) with Labels[T] {}
