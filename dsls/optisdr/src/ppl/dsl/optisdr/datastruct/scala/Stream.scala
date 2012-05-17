package ppl.dsl.optisdr.datastruct.scala

trait Stream[T] {
  var _data : Array[T]
  def _length : Int
}