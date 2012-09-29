package ppl.dsl.opticvx

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{NumericOpsExp, MathOpsExp, OrderingOpsExp, BooleanOpsExp, IfThenElseExp}
import scala.virtualization.lms.common.{EffectExp, BaseExp, Base}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.ops.{DeliteOpsExp}

import java.io.PrintWriter


trait ExprShapeOps extends Base {

  def scalar(): Rep[ExprShapeScalar]
  def vector(n: Rep[Int]): Rep[ExprShapeVector]
  def smatrix(n: Rep[Int]): Rep[ExprShapeSMatrix]
  def symmetric_matrix(n: Rep[Int]) = smatrix(n)

}

trait ExprShapeOpsExp extends ExprShapeOps
  with NumericOpsExp with OrderingOpsExp with BooleanOpsExp with MathOpsExp with EffectExp {

  trait ExprShapeTr {
    def size: Exp[Int]
  }

  case class ExprShapeScalarExp extends Def[ExprShapeScalar] with ExprShapeTr {
    def size: Exp[Int] = Const(1)
  }
  case class ExprShapeVectorExp(val n: Exp[Int]) extends Def[ExprShapeVector] with ExprShapeTr {
    def size: Exp[Int] = n
  }
  case class ExprShapeSMatrixExp(val n: Exp[Int]) extends Def[ExprShapeSMatrix] with ExprShapeTr {
    def size: Exp[Int] = (n*(n+1))/Const(2)
    def indexof(i: Exp[Int], j: Exp[Int]): Exp[Int] = {
      val a = Math.max(i,j)
      val b = Math.min(i,j)
      (a*(a+Const(1))/Const(2))+b
    }
  }

  def canonicalize(x: Exp[ExprShape]): ExprShapeTr = {
    x match {
      case e: ExprShapeTr =>
        return e
      case s: Sym[ExprShape] =>
        findDefinition(s) match {
          case None =>
            throw new Exception("Couldn't canonicalize node " + x)
          case Some(TP(sym,rhs)) =>
            rhs match {
              case e: ExprShapeTr =>
                return e
              case _ =>
                throw new Exception("Couldn't canonicalize node " + x)
            }
        }
      case _ =>
        throw new Exception("Couldn't canonicalize node " + x)
    }
  }

  def scalar(): Exp[ExprShapeScalar]
    = ExprShapeScalarExp()

  def vector(n: Exp[Int]): Exp[ExprShapeVector]
    = ExprShapeVectorExp(n)

  def smatrix(n: Exp[Int]): Exp[ExprShapeSMatrix]
    = ExprShapeSMatrixExp(n)
}

trait ScalaGenExprShapeOps extends ScalaGenBase {
  val IR: ExprShapeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case x: ExprShapeScalarExp =>
        stream.println("val " + quote(sym) + " = ")
      case _ => 
        super.emitNode(sym, rhs)
    }
  }
}