package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, OrderingOpsExp, SeqOpsExp, ArrayOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import scala.collection.immutable.Set

import java.io.PrintWriter


trait ConstExprOps extends Base {
  self: ConstraintOps =>

  implicit def const_to_expr(x: Double): Rep[Expr] = inputscalar(unit(x))
  implicit def const_to_expr(x: Float): Rep[Expr] = inputscalar(unit(x.toDouble))
  implicit def const_to_expr(x: Int): Rep[Expr] = inputscalar(unit(x.toDouble))
  implicit def rep_to_expr(x: Rep[Double]): Rep[Expr] = inputscalar(x)

  def __equal(a: Double, b: Rep[Expr]): Rep[Unit] = (inputscalar(unit(a)) == b)
  def __equal(a: Float, b: Rep[Expr]): Rep[Unit] = (inputscalar(unit(a.toDouble)) == b)
  def __equal(a: Int, b: Rep[Expr]): Rep[Unit] = (inputscalar(unit(a.toDouble)) == b)

  def __equal(b: Rep[Expr], a: Double): Rep[Unit] = (inputscalar(unit(a)) == b)
  def __equal(b: Rep[Expr], a: Float): Rep[Unit] = (inputscalar(unit(a.toDouble)) == b)
  def __equal(b: Rep[Expr], a: Int): Rep[Unit] = (inputscalar(unit(a.toDouble)) == b)

  def __equal(a: Rep[Double], b: Rep[Expr])(implicit d: DummyImplicit): Rep[Unit] = (inputscalar(a) == b)
  def __equal(b: Rep[Expr], a: Rep[Double])(implicit d1: DummyImplicit, d2: DummyImplicit): Rep[Unit] = (inputscalar(a) == b)

  def inputscalar(x: Rep[Double]): Rep[Expr]
  def inputvector(xs: Rep[Double]*): Rep[Expr]
  def inputvector(xar: Rep[Array[Double]]): Rep[Expr]
}

trait ConstExprOpsExp extends ConstExprOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with EffectExp {
  self: ExprOpsExp with OptVarOpsExp with ExprShapeOpsExp with VectorOpsExp 
    with ConstraintOpsExp with ArrayOpsExp with SeqOpsExp =>

  case class ExprInputScalarExp(u: Exp[Double]) extends Def[Expr] with ExprTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      vector_zeros(Const(1))
    }
    def get_ATy(y: Exp[CVXVector], sz: Exp[Int]): Exp[CVXVector] = {
      vector_zeros(sz)
    }
    def get_b(): Exp[CVXVector] = {
      vector1(u)
    }
    
    def vexity(): Signum = Vexity.affine
    def shape(): Exp[ExprShape] = scalar()

    def resolve(): Exp[CVXVector] = {
      vector1(u)
    }

    def vars(): Set[OptVarTr] = Set[OptVarTr]()

    override def toString(): String = "input(" + u.toString + ")"

    def asExp: Exp[Expr] = this
  } 
  def inputscalar(u: Exp[Double]): Exp[Expr] = ExprInputScalarExp(u)

  case class ExprInputVectorExp(us: Exp[Seq[Double]]) extends Def[Expr] with ExprTr {
    def get_Ax(x: Exp[CVXVector]): Exp[CVXVector] = {
      vector_zeros(seq_length(us))
    }
    def get_ATy(y: Exp[CVXVector], sz: Exp[Int]): Exp[CVXVector] = {
      vector_zeros(sz)
    }
    def get_b(): Exp[CVXVector] = {
      vector_fromseq(us)
    }
    
    def vexity(): Signum = Vexity.affine
    def shape(): Exp[ExprShape] = vector(seq_length(us))

    def resolve(): Exp[CVXVector] = {
      vector_fromseq(us)
    }

    def vars(): Set[OptVarTr] = Set[OptVarTr]()

    override def toString(): String = "inputvector(...)"

    def asExp: Exp[Expr] = this
  }
  def inputvector(xs: Rep[Double]*): Rep[Expr] = ExprInputVectorExp(seq_new(xs))
  def inputvector(xar: Rep[Array[Double]]): Rep[Expr] = ExprInputVectorExp(array_toseq(xar))
}

trait ScalaGenConstExprOps extends ScalaGenBase {
  val IR: ConstExprOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}