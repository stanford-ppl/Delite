package ppl.dsl.optigraph.ops

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base, BooleanOps, ExceptionOps, SetOps}
import scala.collection.mutable.Set
import ppl.dsl.optigraph.{GIterable, GSet}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}
import ppl.delite.framework.datastructures.DeliteArray

trait GSetImplOps { this: OptiGraph =>
  def gset_items_impl[A:Manifest](s: Rep[GSet[A]]): Rep[GIterable[A]]
  def gset_contains_impl[A:Manifest](s: Rep[GSet[A]], e: Rep[A]): Rep[Boolean]
  def gset_size_impl[A:Manifest](s: Rep[GSet[A]]): Rep[Int]
  def gset_add_impl[A:Manifest](s: Rep[GSet[A]], e: Rep[A]): Rep[Unit]
  def gset_add_set_impl[A:Manifest](s: Rep[GSet[A]], s2: Rep[GSet[A]]): Rep[Unit]
  def gset_remove_impl[A:Manifest](s: Rep[GSet[A]], e: Rep[A]): Rep[Unit]
  def gset_remove_set_impl[A:Manifest](s: Rep[GSet[A]], s2: Rep[GSet[A]]): Rep[Unit]
  def gset_clear_impl[A:Manifest](s: Rep[GSet[A]]): Rep[Unit]
  def gset_clone_impl[A:Manifest](s: Rep[GSet[A]]): Rep[GSet[A]]
}

trait GSetImplOpsStandard extends GSetImplOps {
  this: OptiGraphCompiler with OptiGraphLift =>

  def gset_items_impl[A:Manifest](s: Rep[GSet[A]]): Rep[GIterable[A]] = {
    //if (gset_empty(s))
    //  return GIterable[A]()
    val d = gset_raw_data(s.unsafeImmutable)
    val gi = GIterable[A](d.toArray.asInstanceOf[Rep[DeliteArray[A]]].unsafeImmutable)
    gi
  }

  def gset_contains_impl[A:Manifest](s: Rep[GSet[A]], e: Rep[A]): Rep[Boolean] = {
    gset_raw_data(s.unsafeImmutable).contains(e)
  }

  def gset_size_impl[A:Manifest](o: Rep[GSet[A]]): Rep[Int] = {
    gset_raw_data(o.unsafeImmutable).size
  }

  def gset_add_impl[A:Manifest](s: Rep[GSet[A]], e: Rep[A]): Rep[Unit] = {
    gset_raw_data(s.unsafeImmutable).add(e.unsafeImmutable)
  }

  def gset_add_set_impl[A:Manifest](s: Rep[GSet[A]], s2: Rep[GSet[A]]): Rep[Unit] = {
    val limit = gset_size(s2)
    var i = 0
    val items = gset_raw_data(s2).toArray.asInstanceOf[Rep[DeliteArray[A]]]
    while (i < limit) {
      gset_add(s, items.apply(i))
      i += 1
    }
  }

  def gset_remove_impl[A:Manifest](s: Rep[GSet[A]], e: Rep[A]): Rep[Unit] = {
    gset_raw_data(s).remove(e)
  }

  def gset_remove_set_impl[A:Manifest](s: Rep[GSet[A]], s2: Rep[GSet[A]]): Rep[Unit] = {
    val limit = gset_size(s2)
    var i = 0
    val items = gset_raw_data(s2).toArray.asInstanceOf[Rep[DeliteArray[A]]]
    while (i < limit) {
      gset_remove(s, items.apply(i))
      i += 1
    }
  }

  def gset_clear_impl[A:Manifest](s: Rep[GSet[A]]): Rep[Unit] = {
    val r = Set[A]()
    gset_set_raw_data(s, r)
  }

  def gset_clone_impl[A:Manifest](s: Rep[GSet[A]]): Rep[GSet[A]] = {
    var d = gset_new()
    gset_set_raw_data(d, gset_raw_data(s))
    d
  }

  protected def gset_empty[A:Manifest](s: Rep[GSet[A]]): Rep[Boolean] = {
    gset_size(s) == 0
  }
}