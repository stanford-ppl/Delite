package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class LabelFieldImpl[T](data: Array[Object], fn: Object => Object) extends LabelField[T] {
  def size = data.length
  def apply(idx: Int) : T = {  
    (Option(fn) match {
      case Some(fn) => fn(data(idx))
      case None => data(idx)
    }).asInstanceOf[T]
  }
}
