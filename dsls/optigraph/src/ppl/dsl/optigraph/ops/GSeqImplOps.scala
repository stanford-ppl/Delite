package ppl.dsl.optigraph.ops

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base, BooleanOps, ExceptionOps}
import ppl.dsl.optigraph.{GIterable, GSeq}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}
import ppl.delite.framework.datastructures.DeliteArray

trait GSeqImplOps { this: OptiGraph =>
  def gseq_size_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[Int]
  def gseq_apply_impl[A:Manifest](o: Rep[GSeq[A]], i: Rep[Int]): Rep[A]
  def gseq_items_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[GIterable[A]]
  def gseq_contains_impl[A:Manifest](o: Rep[GSeq[A]], x: Rep[A]): Rep[Boolean]
  def gseq_front_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A]
  def gseq_back_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A]
  def gseq_pushback_impl[A:Manifest](o: Rep[GSeq[A]], x: Rep[A]): Rep[Unit]
  def gseq_pushbackorder_impl[A:Manifest](o: Rep[GSeq[A]], xs: Rep[GSeq[A]]): Rep[Unit]
  def gseq_pushfront_impl[A:Manifest](o: Rep[GSeq[A]], x: Rep[A]): Rep[Unit]
  def gseq_pushfrontorder_impl[A:Manifest](o: Rep[GSeq[A]], xs: Rep[GSeq[A]]): Rep[Unit]
  def gseq_popfront_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A]
  def gseq_popback_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A]
}

trait GSeqImplOpsStandard extends GSeqImplOps {
  this: OptiGraphCompiler with OptiGraphLift =>

  def gseq_size_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[Int] = {
    gseq_raw_data(o).length
  }

  def gseq_apply_impl[A:Manifest](o: Rep[GSeq[A]], i: Rep[Int]): Rep[A] = {
    val sz = gseq_size(o)
    if (gseq_empty(o))
      fatal("Tried to access element of empty GSeq")
    else if (!(i >= 0 && i < sz)) {
      fatal("GSeq apply: Index " + i + " out of bounds (size=" + sz + ")")
    }
    val d = gseq_raw_data(o)
    d(i)
  }

  def gseq_items_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[GIterable[A]] = {
    if (gseq_empty(o))
      return GIterable[A]()
    val d = gseq_raw_data(o)
    val gi = GIterable[A](d)
    gi
  }

  def gseq_contains_impl[A:Manifest](o: Rep[GSeq[A]], x: Rep[A]): Rep[Boolean] = {
    var i = 0
    val sz = gseq_size(o)
    val d = gseq_raw_data(o)
    while (i < sz) {
      if (d.apply(i) == x)
        return true
    }
    return false
  }

  def gseq_front_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A] = {
    if (gseq_empty(o))
      fatal("Tried to access front of empty GSeq")
    gseq_apply(o, 0)
  }

  def gseq_back_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A] = {
    if (gseq_empty(o))
      fatal("Tried to access back of empty GSeq")
    gseq_apply(o, gseq_size(o) - 1)
  }

  def gseq_pushback_impl[A:Manifest](o: Rep[GSeq[A]], x: Rep[A]): Rep[Unit] = {
    if (gseq_empty(o) || !gseq_contains(o, x)) {
      val sz = gseq_size(o)
      val r = DeliteArray[A](sz + 1)
      val d = gseq_raw_data(o)
      darray_unsafe_copy(d, 0, r, 0, sz)
      darray_unsafe_update(r, sz, x)
      gseq_set_raw_data(o, r)
    }
  }

  def gseq_pushbackorder_impl[A:Manifest](o: Rep[GSeq[A]], xs: Rep[GSeq[A]]): Rep[Unit] = {
    for (i <- 0 until gseq_size(xs)) {
      gseq_pushback(o, gseq_apply(xs, i))
    }
  }

  def gseq_pushfront_impl[A:Manifest](o: Rep[GSeq[A]], x: Rep[A]): Rep[Unit] = {
    if(gseq_empty(o) || !gseq_contains(o, x)) {
      val sz = gseq_size(o)
      val r = DeliteArray[A](sz + 1)
      val d = gseq_raw_data(o)
      darray_unsafe_copy(d, 0, r, 1, sz)
      darray_unsafe_update(r, 0, x)
      gseq_set_raw_data(o, r)
    }
  }

  def gseq_pushfrontorder_impl[A:Manifest](o: Rep[GSeq[A]], xs: Rep[GSeq[A]]): Rep[Unit] = {
    var i = gseq_size(xs) - 1
    while (i >= 0) {
      gseq_pushfront(o, gseq_apply(xs, i))
      i -= 1
    }
  }

  def gseq_popfront_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A] = {
    val f = gseq_front(o)
    val sz = gseq_size(o)
    val r = DeliteArray[A](sz - 1)
    val d = gseq_raw_data(o)
    darray_unsafe_copy(d, 1, r, 0, sz - 1)
    gseq_set_raw_data(o, r)
    f
  }

  def gseq_popback_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A] = {
    val l = gseq_back(o)
    val sz = gseq_size(o)
    val r = DeliteArray[A](sz - 1)
    val d = gseq_raw_data(o)
    darray_unsafe_copy(d, 0, r, 0, sz - 1)
    gseq_set_raw_data(o, r)
    l
  }

  protected def gseq_empty[A:Manifest](o: Rep[GSeq[A]]): Rep[Boolean] = {
    gseq_size(o) == 0
  }
}