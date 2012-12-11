package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Set
import scala.collection.immutable.Seq


sealed trait Constraint extends HasArity[Constraint] {
  val size: IRPoly
}

case class AffineConstraint(val expr: Expr) extends Constraint {
  val arity: Int = expr.arity
  val size: IRPoly = expr.size
  def arityOp(op: ArityOp): Constraint = AffineConstraint(expr.arityOp(op))
}

case class ConicConstraint(val expr: Expr, val cone: Cone) extends Constraint {
  val arity: Int = expr.arity
  val size: IRPoly = expr.size
  if(expr.size != cone.size) {
    println(expr.size)
    println(cone.size)
    throw new IRValidationException()
  }
  def arityOp(op: ArityOp): Constraint = ConicConstraint(expr.arityOp(op), cone.arityOp(op))
}

object Constraint {
  def zero(expr: Expr): AffineConstraint = {
    if(expr.vexity != Signum.Zero) throw new IRValidationException()
    AffineConstraint(expr)
  }

  def nonnegative(expr: Expr): ConicConstraint = {
    if(!(expr.vexity <= Signum.Negative)) throw new IRValidationException()
    ConicConstraint(expr, ConeNonNegative(expr.arity))
  }

  def catfor(len: IRPoly, constraint: Constraint): Constraint = {
    if(len.arity + 1 != constraint.arity) throw new IRValidationException()
    constraint match {
      case AffineConstraint(e) => AffineConstraint(Expr.catfor(len, e))
      case ConicConstraint(e, c) => ConicConstraint(Expr.catfor(len, e), ConeFor(len, c))
    }
  }

  /*
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
  */
}
