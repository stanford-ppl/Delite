package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, MathOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import java.io.PrintWriter


trait ConstraintOps extends Base {
  
  def infix_>=(x: Rep[Expr], y: Rep[Expr]): Rep[Unit]
  def infix_<=(x: Rep[Expr], y: Rep[Expr]): Rep[Unit]
  def infix_===(x: Rep[Expr], y: Rep[Expr]): Rep[Unit]
  def __equal(x: Rep[Expr], y: Rep[Expr]): Rep[Unit]

  def constrain_zero(x: Rep[Expr]): Rep[Unit]

  def constrain_nonnegative(x: Rep[Expr]): Rep[Unit]
  def constrain_secondordercone(x: Rep[Expr], z: Rep[Expr]): Rep[Unit]
  def constrain_rotatedcone(x: Rep[Expr], y: Rep[Expr], z: Rep[Expr]): Rep[Unit]
  def constrain_semidefinite(x: Rep[Expr]): Rep[Unit]
}

trait ConstraintOpsExp extends ConstraintOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprOpsExp with ExprShapeOpsExp with OptVarOpsExp with IfThenElseExp with MathOpsExp with VectorOpsExp =>

  abstract class Constraint {
    def vars(): Set[OptVarTr]
  }
  case class ConstrainZero(x: ExprTr) extends Constraint {
    def vars() = x.vars()
    override def toString() = "0 == " + (x).toString()
  }
  case class ConstrainNonnegative(x: ExprTr) extends Constraint {
    def vars() = x.vars()
    override def toString() = "0 <= " + (x).toString()
  }
  case class ConstrainSecondOrderCone(x: ExprTr, z: ExprTr) extends Constraint {
    def vars() = x.vars() ++ z.vars()
    override def toString() = "norm(" + (x).toString() + ") <= " + (z).toString()
  }
  case class ConstrainRotatedCone(x: ExprTr, y: ExprTr, z: ExprTr) extends Constraint {
    def vars() = x.vars() ++ y.vars() ++ z.vars()
    override def toString() = "norm(" + (x).toString() + ") <= (" + (y).toString() + ")*(" + (z).toString() + ")"
  }
  case class ConstrainSemidefinite(x: ExprTr) extends Constraint {
    def vars() = x.vars()
    override def toString() = "semidefinite(" + (x).toString() + ")"
  }

  def infix_>=(x: Exp[Expr], y: Exp[Expr]): Exp[Unit]
    = constrain_nonnegative(x-y)

  def infix_<=(x: Exp[Expr], y: Exp[Expr]): Exp[Unit]
    = constrain_nonnegative(y-x)

  def infix_===(x: Exp[Expr], y: Exp[Expr]): Exp[Unit] = {
    constrain_zero(x-y)
  }
  def __equal(x: Rep[Expr], y: Rep[Expr]): Rep[Unit] = {
    constrain_zero(x-y)
  }

  def constrain_zero(x: Exp[Expr]): Exp[Unit] = {
    val cx = canonicalize(x)
    if(!(cx.vexity() <= Vexity.affine)) {
      throw new Exception("Could not constrain non-affine expression to zero.")
    }
    val constraint = ConstrainZero(cx)
    for(v <- cx.vars()) {
      v.constraints +:= constraint
    }
  }

  def constrain_nonnegative(x: Exp[Expr]): Exp[Unit] = {
    val cx = canonicalize(x)
    if(!(cx.vexity() <= Vexity.concave)) {
      throw new Exception("Could not constrain non-concave expression to be nonnegative.")
    }
    canonicalize(cx.shape()) match {
      case sh: ExprShapeScalarExp => 
      case _ => throw new Exception("Could not constrain non-scalar expression to be nonnegative.")
    }
    val constraint = ConstrainNonnegative(cx)
    for(v <- cx.vars()) {
      v.constraints +:= constraint
    }
  }

  def constrain_secondordercone(x: Exp[Expr], z: Exp[Expr]): Exp[Unit] = {
    val cx = canonicalize(x)
    val cz = canonicalize(z)
    if(!(cx.vexity() <= Vexity.affine)) {
      throw new Exception("Could not constrain non-affine expression as X-part of second-order cone.")
    }
    canonicalize(cx.shape()) match {
      case sh: ExprShapeVectorExp => 
      case _ => throw new Exception("Could not constrain non-vector expression as X-part of second-order cone.")
    }
    if(!(cz.vexity() <= Vexity.concave)) {
      throw new Exception("Could not constrain non-concave expression as Z-part of second-order cone.")
    }
    canonicalize(cz.shape()) match {
      case sh: ExprShapeScalarExp => 
      case _ => throw new Exception("Could not constrain non-scalar expression as Z-part of second-order cone.")
    }
    val constraint = ConstrainSecondOrderCone(cx,cz)
    for(v <- (cx.vars() ++ cz.vars())) {
      v.constraints +:= constraint
    }
  }

  def constrain_rotatedcone(x: Exp[Expr], y: Exp[Expr], z: Exp[Expr]): Exp[Unit] = {
    val cx = canonicalize(x)
    val cy = canonicalize(y)
    val cz = canonicalize(z)
    if(!(cx.vexity() <= Vexity.affine)) {
      throw new Exception("Could not constrain non-affine expression as X-part of rotated cone.")
    }
    if(!(cy.vexity() <= Vexity.concave)) {
      throw new Exception("Could not constrain non-concave expression as Y-part of rotated cone.")
    }
    if(!(cz.vexity() <= Vexity.concave)) {
      throw new Exception("Could not constrain non-concave expression as Z-part of rotated cone.")
    }
    canonicalize(cx.shape()) match {
      case sh: ExprShapeVectorExp => 
      case _ => throw new Exception("Could not constrain non-vector expression as X-part of rotated cone.")
    }
    canonicalize(cy.shape()) match {
      case sh: ExprShapeScalarExp => 
      case _ => throw new Exception("Could not constrain non-scalar expression as Y-part of rotated cone.")
    }
    canonicalize(cz.shape()) match {
      case sh: ExprShapeScalarExp => 
      case _ => throw new Exception("Could not constrain non-scalar expression as Z-part of rotated cone.")
    }
    val constraint = ConstrainRotatedCone(cx,cy,cz)
    for(v <- (cx.vars() ++ cy.vars() ++ cz.vars())) {
      v.constraints +:= constraint
    }
  }

  def constrain_semidefinite(x: Exp[Expr]): Exp[Unit] = {
    val cx = canonicalize(x)
    if(!(cx.vexity() <= Vexity.affine)) {
      throw new Exception("Could not constrain non-affine expression to be semidefinite.")
    }
    canonicalize(cx.shape()) match {
      case sh: ExprShapeSMatrixExp => 
      case _ => throw new Exception("Could not constrain non-matrix expression to be semidefinite.")
    }
    val constraint = ConstrainSemidefinite(cx)
    for(v <- cx.vars()) {
      v.constraints +:= constraint
    }
  }
}

trait ScalaGenConstraintOps extends ScalaGenBase {
  val IR: ConstraintOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}