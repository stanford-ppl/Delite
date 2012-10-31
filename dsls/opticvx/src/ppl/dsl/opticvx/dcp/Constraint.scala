package ppl.dsl.opticvx.dcp

import scala.collection.immutable.Set
import scala.collection.immutable.Seq

trait DCPConstraint {
  self: DCPShape with DCPExpr with DCPCone with DCPAlmap =>

  
  sealed trait Constraint

  case class AffineConstraint(val expr: Expr) extends Constraint

  case class ConicConstraint(val expr: Expr, val cone: Cone) extends Constraint {
    cone.dcpvalidate(expr.shape)
  }

  def constrain_zero(expr: Expr): AffineConstraint = 
    AffineConstraint(expr)

  def constrain_nonnegative(expr: Expr): ConicConstraint =
    ConicConstraint(expr, ConeNonNegative(expr.arity))

  def cfor(len: Size, body: (Size) => Constraint): Constraint = {
    globalArityPromote()
    val eval_body = body(len.next)
    globalArityDemote()
    eval_body match {
      case bsx: ConicConstraint => ConicConstraint(
        Expr(
          XShapeFor(len, bsx.expr.shape),
          AlmapVCatFor(len, bsx.expr.almap),
          AlmapVCatFor(len, bsx.expr.offset)),
        ConeFor(len, bsx.cone))
      case bsx: AffineConstraint => AffineConstraint(
        Expr(
          XShapeFor(len, bsx.expr.shape),
          AlmapVCatFor(len, bsx.expr.almap),
          AlmapVCatFor(len, bsx.expr.offset)))
      case _ => throw new DCPIRValidationException()
    }

  }

}

