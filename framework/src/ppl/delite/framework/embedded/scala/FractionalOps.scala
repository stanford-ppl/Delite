package ppl.delite.framework.embedded.scala

import java.io.PrintWriter
import ppl.delite.framework.{DSLType, DeliteApplication}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}
import scala.virtualization.lms.common.EffectExp

trait FractionalOps extends DSLType { this: DeliteApplication with ImplicitOps =>

  implicit def repFractionalToFractionalOps[T](x: Rep[T])(implicit f: Fractional[T]) = new FractionalOpsCls(x,f)
  implicit def fractionalToFractionalOps[T](x: T)(implicit f: Fractional[T]) = new FractionalOpsCls(x,f)

  class FractionalOpsCls[T](lhs: Rep[T], implicit val f: Fractional[T]) {
    def /(rhs: Rep[T]) : Rep[T] = fractional_divide(lhs,rhs)
    // TODO: why does this not work, but the uncommented one does?
    //def /[A](rhs: Rep[A])(implicit c: A => T) : Rep[T] = fractional_divide(lhs,implicit_convert[A,T](rhs))
    def /(rhs: Rep[Int])(implicit c: Int => T) : Rep[T] = fractional_divide(lhs,implicit_convert[Int,T](rhs))
  }

  def fractional_divide[T](lhs: Rep[T], rhs: Rep[T])(implicit f: Fractional[T]): Rep[T]
}

trait FractionalOpsExp extends FractionalOps { this: DeliteApplication with ImplicitOpsExp => 
  case class FractionalDivide[T](lhs: Exp[T], rhs: Exp[T], implicit val f: Fractional[T]) extends Def[T]
  
  def fractional_divide[T](lhs: Exp[T], rhs: Exp[T])(implicit f: Fractional[T]) : Rep[T] = FractionalDivide(lhs, rhs, f)

  targets.get("Scala").getOrElse(throw new RuntimeException("Couldn't find Scala code generator"))
    .addGenerator( new CodeGeneratorScalaFractional { val intermediate: FractionalOpsExp.this.type = FractionalOpsExp.this })
}

trait CodeGeneratorScalaFractional extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with FractionalOpsExp with EffectExp
  import intermediate._

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case FractionalDivide(a,b,f) => emitValDef(sym, quote(a) + " / " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
