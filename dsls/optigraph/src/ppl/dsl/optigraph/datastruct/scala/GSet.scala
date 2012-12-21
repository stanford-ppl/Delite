package ppl.dsl.optigraph.datastruct.scala

import collection.mutable.{HashSet}

class GSet[@specialized T: ClassManifest] {
  var _data: HashSet[T] = new HashSet[T]()

  // TODO Are these needed? GSet doesn't extend DeliteCollection...
  /*
  // DeliteCollection ops
  def dcSize : Int = { dataAsArray.size }
  def dcApply(idx: Int) : T = { dataAsArray(idx) }
  def dcUpdate(idx: Int, x: T) : Unit = {
    //TODO: update does not make sense for a set
    throw new RuntimeException("Set dcUpdate should not be called")
  }
  */
}