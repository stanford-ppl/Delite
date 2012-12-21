package ppl.dsl.optigraph.ops

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base, BooleanOps, ExceptionOps}
import scala.collection.mutable.Set
import ppl.dsl.optigraph.{GIterable, GOrder}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}
import ppl.delite.framework.datastructures.DeliteArray

trait GOrderImplOps { this: OptiGraph =>
  def gorder_size_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[Int]
  def gorder_items_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[GIterable[A]]
  def gorder_contains_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Boolean]
  def gorder_front_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A]
  def gorder_back_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A]
  def gorder_pushback_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Unit]
  def gorder_pushbackorder_impl[A:Manifest](o: Rep[GOrder[A]], xs: Rep[GOrder[A]]): Rep[Unit]
  def gorder_pushfront_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Unit]
  def gorder_pushfrontorder_impl[A:Manifest](o: Rep[GOrder[A]], xs: Rep[GOrder[A]]): Rep[Unit]
  def gorder_popfront_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A]
  def gorder_popback_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A]
}

trait GOrderImplOpsStandard extends GOrderImplOps {
  this: OptiGraphCompiler with OptiGraphLift =>

  def gorder_size_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[Int] = {
    gorder_raw_data(o.unsafeImmutable).size
  }

  def gorder_items_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[GIterable[A]] = {
    if (gorder_empty(o))
      return GIterable[A]()
    val d = gorder_raw_data(o.unsafeImmutable)
    val gi = GIterable[A](d.toArray.asInstanceOf[Rep[DeliteArray[A]]])
    gi
  }

  def gorder_contains_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Boolean] = {
    gorder_raw_data(o.unsafeImmutable).contains(x)
  }

  def gorder_front_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A] = {
    if (gorder_empty(o))
      fatal("Tried to access front of empty GOrder")
    // ugly ugly ugly
    val d = gorder_raw_data(o.unsafeImmutable).toArray.asInstanceOf[Rep[DeliteArray[A]]]
    d.apply(0)
  }

  def gorder_back_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A] = {
    if (gorder_empty(o))
      fatal("Tried to access back of empty GOrder")
    // ugly ugly ugly
    val d = gorder_raw_data(o.unsafeImmutable).toArray.asInstanceOf[Rep[DeliteArray[A]]]
    d.apply(gorder_size(o) - 1)
  }

  def gorder_pushback_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Unit] = {
    gorder_raw_data(o.unsafeImmutable).add(x.unsafeImmutable)
  }

  def gorder_pushbackorder_impl[A:Manifest](o: Rep[GOrder[A]], xs: Rep[GOrder[A]]): Rep[Unit] = {
    if (!gorder_empty(o)) {
      // ugly ugly ugly
      // Convert the GOrder we want to push to the back of o to an Array
      val d = gorder_raw_data(o.unsafeImmutable).toArray.asInstanceOf[Rep[DeliteArray[A]]]
      var i = 0
      val sz = gorder_size(o)
      // Then iterate over that array from beginning to end, adding each
      // element to o
      // TODO are we guaranteed that the array maintains the order information
      // from the underlying LinkedHashSet?
      while(i < sz) {
        gorder_pushback(o, d.apply(i))
        i += 1
      }
    }
  }

  def gorder_pushfront_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Unit] = {
    fatal("PushFront is not supported yet because LMS does not support LinkedHashSets")
  }

  def gorder_pushfrontorder_impl[A:Manifest](o: Rep[GOrder[A]], xs: Rep[GOrder[A]]): Rep[Unit] = {
    fatal("PushFrontOrder is not supported yet because LMS does not support LinkedHashSets")
  }

  def gorder_popfront_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A] = {
    // ugly ugly ugly
    // Convert the Set to an Array
    val d = gorder_raw_data(o.unsafeImmutable).toArray.asInstanceOf[Rep[DeliteArray[A]]]
    // Grab the first element of the Array
    val elem = d.apply(0)
    // Remove that element from the Set
    gorder_raw_data(o.unsafeImmutable).remove(elem)
    elem
  }

  def gorder_popback_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A] = {
    // ugly ugly ugly
    // Convert the Set to an Array
    val d = gorder_raw_data(o.unsafeImmutable).toArray.asInstanceOf[Rep[DeliteArray[A]]]
    // Grab the last element of the Array
    val elem = d.apply(gorder_size(o) - 1)
    // Remove that element from the Set
    gorder_raw_data(o.unsafeImmutable).remove(elem)
    elem
  }

  protected def gorder_empty[A:Manifest](o: Rep[GOrder[A]]): Rep[Boolean] = {
    gorder_size(o) == 0
  }
}