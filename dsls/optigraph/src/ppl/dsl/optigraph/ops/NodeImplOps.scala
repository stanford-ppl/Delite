package ppl.dsl.optigraph.ops

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base, BooleanOps}
import ppl.dsl.optigraph.{GIterable, Node}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}

trait NodeImplOps { this: OptiGraph =>
  //def node_has_out_nbr_impl[A:Manifest](n: Rep[Node], t: Rep[Node]): Rep[Boolean]
}

trait NodeImplOpsStandard extends NodeImplOps {
  this: OptiGraphCompiler with OptiGraphLift =>

  //def node_has_out_nbr_impl[A:Manifest](n: Rep[Node], t: Rep[Node]): Rep[Boolean] = {
  //  n.OutNbrs.contains(t)
  //}

}
