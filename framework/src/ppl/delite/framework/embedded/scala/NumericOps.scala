package ppl.delite.framework.embedded.scala

import java.io.PrintWriter
import ppl.delite.framework.{DSLType, DeliteApplication}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}
import scala.virtualization.lms.common.EffectExp

trait NumericOps extends DSLType { this: DeliteApplication with Variables =>
  // TODO: fix or investigate -- this causes a compiler bug
  //implicit val repDoubleToNumericOps : Rep[Double] => NumericOpsCls[Double] = x => new NumericOpsCls[Double](x)
  implicit def varNumericToNumericOps[T:Numeric](x: Var[T]) : NumericOpsCls[T]
  implicit def repNumericToNumericOps[T:Numeric](x: Rep[T]) = new NumericOpsCls(x)
  //implicit def numericToNumericOps[T:Numeric](x: T) = new NumericOpsCls(x)

  def infix_*[T:Numeric](lhs: Rep[T], rhs: Rep[T]) = numeric_times(lhs,rhs)
  //def infix_+(lhs: Rep[Double], rhs: Rep[Double]) = numeric_plus(lhs,rhs)
  def infix_+[T:Numeric](lhs: Rep[T], rhs: Rep[T]) = numeric_plus(lhs,rhs)
  def infix_-[T:Numeric](lhs: Rep[T], rhs: Rep[T]) = numeric_minus(lhs,rhs)

  class NumericOpsCls[T:Numeric](lhs: Rep[T]) {
    def +(rhs: Rep[T]) = numeric_plus(lhs,rhs)
    def -(rhs: Rep[T]) = numeric_minus(lhs,rhs)
    def *(rhs: Rep[T]) = numeric_times(lhs,rhs)
  }

  def numeric_plus[T:Numeric](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def numeric_minus[T:Numeric](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def numeric_times[T:Numeric](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  //def numeric_negate[T:Numeric](x: T): Rep[T]
  //def numeric_abs[T:Numeric](x: T): Rep[T]
  //def numeric_signum[T:Numeric](x: T): Rep[Int]
}

trait NumericOpsExp extends NumericOps { this: DeliteApplication with VariablesExp =>
  implicit def varNumericToNumericOps[T:Numeric](x: Var[T]) = new NumericOpsCls(readVar(x))

  case class NumericPlus[T:Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  case class NumericMinus[T:Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  case class NumericTimes[T:Numeric](lhs: Exp[T], rhs: Exp[T]) extends Def[T]

  def numeric_plus[T:Numeric](lhs: Exp[T], rhs: Exp[T]) : Rep[T] = NumericPlus(lhs, rhs)
  def numeric_minus[T:Numeric](lhs: Exp[T], rhs: Exp[T]) : Rep[T] = NumericMinus(lhs, rhs)
  def numeric_times[T:Numeric](lhs: Exp[T], rhs: Exp[T]) : Rep[T] = NumericTimes(lhs, rhs)

  targets.get("Scala").getOrElse(throw new RuntimeException("Couldn't find Scala code generator"))
    .addGenerator( new CodeGeneratorScalaNumeric { val intermediate: NumericOpsExp.this.type = NumericOpsExp.this })
}

trait CodeGeneratorScalaNumeric extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with NumericOpsExp with EffectExp
  import intermediate._
  
  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case NumericPlus(a,b) => emitValDef(sym, quote(a) + " + " + quote(b))
    case NumericMinus(a,b) => emitValDef(sym, quote(a) + " - " + quote(b))
    case NumericTimes(a,b) => emitValDef(sym, quote(a) + " * " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
