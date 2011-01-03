package ppl.dsl.optiml.datastruct.scala

class LabelsImpl[T:Manifest](xs: VectorImpl[T]) extends VectorImpl[T](xs.data, false) with Labels[T] {
}
