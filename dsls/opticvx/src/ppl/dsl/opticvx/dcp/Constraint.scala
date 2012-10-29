package ppl.dsl.opticvx.dcp

import scala.collection.immutable.Set
import scala.collection.immutable.Seq

trait DCPConstraint {
  self: DCPShape with DCPExpr with DCPCone =>

  
  case class Constraint(val expr: Expr, val cone: Cone) {
    cone.dcpvalidate(expr.shape)
  }

}

