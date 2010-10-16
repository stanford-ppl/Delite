package ppl.delite.framework.embedded.scala

import java.io.PrintWriter
import ppl.delite.framework.{DSLType, DeliteApplication}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}
import scala.virtualization.lms.common.EffectExp

trait BooleanOps extends DSLType { this: DeliteApplication with ImplicitOps =>

  implicit def repBooleanToBooleanOps(x: Rep[Boolean]) = new BooleanOpsCls(x)
  implicit def booleanToBooleanOps(x: Boolean) = new BooleanOpsCls(x)

  class BooleanOpsCls(lhs: Rep[Boolean]) {
    def unary_! = boolean_negate(lhs)
  }

  def boolean_negate(lhs: Rep[Boolean]): Rep[Boolean]
}

trait BooleanOpsExp extends BooleanOps { this: DeliteApplication with ImplicitOpsExp =>
  case class BooleanNegate(lhs: Exp[Boolean]) extends Def[Boolean]
  
  def boolean_negate(lhs: Exp[Boolean]) : Rep[Boolean] = BooleanNegate(lhs)

  targets.get("Scala").getOrElse(throw new RuntimeException("Couldn't find Scala code generator"))
    .addGenerator( new CodeGeneratorScalaBoolean { val intermediate: BooleanOpsExp.this.type = BooleanOpsExp.this })
}

trait CodeGeneratorScalaBoolean extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with BooleanOpsExp with EffectExp
  import intermediate._

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, "!" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
