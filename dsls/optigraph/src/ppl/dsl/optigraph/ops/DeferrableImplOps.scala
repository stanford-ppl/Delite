package ppl.dsl.optigraph.ops

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optigraph.{Deferrable}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}

trait DeferrableImplOps { this: OptiGraph =>
  def deferrable_defer_impl[A:Manifest](d: Rep[Deferrable[A]], v: Rep[A]): Rep[Unit]
  def deferrable_assign_impl[A:Manifest](d: Rep[Deferrable[A]]): Rep[Unit]
}

trait DeferrableImplOpsStandard extends DeferrableImplOps {
  this: OptiGraphCompiler with OptiGraphLift =>

  def deferrable_defer_impl[A:Manifest](d: Rep[Deferrable[A]], v: Rep[A]): Rep[Unit] = {
    def_setdefvalue(d, v)
    def_setdeferred(d, true)
  }

  def deferrable_assign_impl[A:Manifest](d: Rep[Deferrable[A]]): Rep[Unit] = {
    if (def_getdeferred(d)) {
      def_setvalue(d, def_getdefvalue(d))
      def_setdeferred(d, false)
    }
  }
}