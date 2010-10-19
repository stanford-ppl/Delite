package ppl.delite.framework.embedded.scala

import java.io.PrintWriter
import ppl.delite.framework.{DSLType, DeliteApplication}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}

trait BooleanOps extends DSLType { this: DeliteApplication with Variables with ImplicitOps =>

  def infix_unary_!(x: Rep[Boolean]) = boolean_negate(x)

  def boolean_negate(lhs: Rep[Boolean]): Rep[Boolean]
}

trait BooleanOpsExp extends BooleanOps { this: DeliteApplication with Variables with ImplicitOpsExp =>
  case class BooleanNegate(lhs: Exp[Boolean]) extends Def[Boolean]
  
  def boolean_negate(lhs: Exp[Boolean]) : Rep[Boolean] = BooleanNegate(lhs)

  targets.get("Scala").getOrElse(throw new RuntimeException("Couldn't find Scala code generator"))
    .addGenerator( new CodeGeneratorScalaBoolean { val intermediate: BooleanOpsExp.this.type = BooleanOpsExp.this })
}

trait CodeGeneratorScalaBoolean extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with BooleanOpsExp 
  import intermediate._

  def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Boolean = {
    rhs match {
      case BooleanNegate(b) => emitValDef(sym, "!" + quote(b))
      case _ => return false
    }
    true
  }
}
