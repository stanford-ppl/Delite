package ppl.delite.framework.embedded.scala

import java.io.PrintWriter
import ppl.delite.framework.{DSLType, DeliteApplication}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}

trait FractionalOps extends DSLType { this: DeliteApplication with ImplicitOps =>

  def infix_/[A,T](lhs: Rep[T], rhs: Rep[A])(implicit c: A => T, f: Fractional[T]) = fractional_divide(lhs,implicit_convert[A,T](rhs))

  def fractional_divide[T:Fractional](lhs: Rep[T], rhs: Rep[T]): Rep[T]
}

trait FractionalOpsExp extends FractionalOps { this: DeliteApplication with ImplicitOpsExp => 
  case class FractionalDivide[T:Fractional](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  
  def fractional_divide[T:Fractional](lhs: Exp[T], rhs: Exp[T]) : Rep[T] = FractionalDivide(lhs, rhs)

  targets.get("Scala").getOrElse(throw new RuntimeException("Couldn't find Scala code generator"))
    .addGenerator( new CodeGeneratorScalaFractional { val intermediate: FractionalOpsExp.this.type = FractionalOpsExp.this })
}

trait CodeGeneratorScalaFractional extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with FractionalOpsExp
  import intermediate._

  def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Boolean = {
    rhs match {
      case FractionalDivide(a,b) => emitValDef(sym, quote(a) + " / " + quote(b))
      case _ => return false
    }
    true
  }
}
