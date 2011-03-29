package ppl.dsl.optiml.datastruct.scala

class LabelsImpl[T:Manifest](xs: Vector[T]) extends VectorImpl[T](xs.data, false) with Labels[T] {}
