package ppl.dsl.deliszt.datastruct.scala

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 04/24/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class LabelFieldImpl[MO<:MeshObj:Manifest, VT:Manifest](data: Array[Object], fn: Object => Object) extends LabelField[MO,VT] {
  def apply(e: MO) : VT = dcApply(e.internalId)
  def size = data.length
  def dcApply(idx: Int) = {
    (Option(fn) match {
      case Some(fn) => data(idx)
      case None => data(idx)
    }).asInstanceOf[VT]
  }
}