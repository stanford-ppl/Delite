package ppl.dsl.opticvx.common

import scala.collection.immutable.Seq

sealed trait MultiSeq[T] {
  def order: Int
  def apply(at: Seq[Int]): T
  def updated(at: Seq[Int], elem: T): MultiSeq[T]
}

object MultiSeq {
  def apply[T](t: T): MultiSeq[T] = MultiSeqA0(t)
}

private case class MultiSeqA0[T](t: T) extends MultiSeq[T] {
  def order: Int = 0
  def apply(at: Seq[Int]): T = {
    if(at != Seq()) throw new IRValidationException()
    t
  }
  def updated(at: Seq[Int], elem: T): MultiSeq[T] = {
    if(at != Seq()) throw new IRValidationException()
    MultiSeqA0(elem)
  }
}

private case class MultiSeqN[T](o: Int, ar: Seq[MultiSeq[T]]) extends MultiSeq[T] {
  for(a <- ar) {
    if(a.order != o - 1) throw new IRValidationException()
  }
  def order: Int = o
  def apply(at: Seq[Int]): T = {
    ar(at(0)).apply(at.drop(1))
  }
  def updated(at: Seq[Int], elem: T): MultiSeq[T] = {
    MultiSeqN(o, ar.updated(at(0), ar(at(0)).updated(at.drop(1), elem)))
  }
}
