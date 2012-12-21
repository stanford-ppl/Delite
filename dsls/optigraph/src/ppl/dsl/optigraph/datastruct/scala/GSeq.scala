package ppl.dsl.optigraph.datastruct.scala

class GSeq[@specialized T: ClassManifest] {
  var _data: Array[T] = new Array[T](0)
}