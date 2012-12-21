package ppl.dsl.optigraph.ops

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optigraph.{GIterable, GSet}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}

trait GIterableImplOps { this: OptiGraph =>
  //def giterable_tolist_impl[A:Manifest](g: Rep[GIterable[A]]): Rep[List[A]]
  def giterable_toset_impl[A:Manifest](g: Rep[GIterable[A]]): Rep[GSet[A]]
  def giterable_insert_impl[A:Manifest](g: Rep[GIterable[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit]
}

trait GIterableImplOpsStandard extends GIterableImplOps {
  this: OptiGraphCompiler with OptiGraphLift =>

//TODO How to convert a DeliteArray to a Scala List easily?
// Probably just iterate over all the elements and insert it into a list
/*
  def giterable_tolist_impl[A:Manifest](g: Rep[GIterable[A]]): Rep[List[A]] = {
    val d = giterable_raw_data(g)
    d.toList
  }
*/

  def giterable_toset_impl[A:Manifest](g: Rep[GIterable[A]]): Rep[GSet[A]] = {
    val ns = gset_new()
    var i = 0
    while (i < giterable_raw_size(g)) {
      gset_add(ns, giterable_raw_apply(g, i).unsafeImmutable)
      i += 1
    }
    ns.unsafeImmutable
  }

  def giterable_insert_impl[A:Manifest](g: Rep[GIterable[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit] = {
    giterable_insertspace(g, pos, 1)
    giterable_raw_update(g, pos, x)
  }

  protected def giterable_insertspace[A:Manifest](g: Rep[GIterable[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit] = {
    giterable_ensureextra(g, len)
    val data = giterable_raw_data(g)
    darray_unsafe_copy(data, pos, data, pos + len, giterable_raw_size(g) - pos)
    giterable_set_raw_size(g, giterable_raw_size(g) + len)
  }

  protected def giterable_ensureextra[A:Manifest](g: Rep[GIterable[A]], extra: Rep[Int]): Rep[Unit] = {
    val data = giterable_raw_data(g)
    if (data.length - giterable_raw_size(g) < extra) {
      giterable_realloc(g, giterable_raw_size(g) + extra)
    }
  }

  protected def giterable_realloc[A:Manifest](g: Rep[GIterable[A]], minLen: Rep[Int]): Rep[Unit] = {
    val data = giterable_raw_data(g)
    var n = Math.max(4, data.length * 2)
    while (n < minLen) n = n*2
    val d = DeliteArray[A](n)
    darray_unsafe_copy(data, 0, d, 0, giterable_raw_size(g))
    giterable_set_raw_data(g, d.unsafeImmutable)
  }
}