package ppl.delite.framework.embedded.scala

import java.io.PrintWriter
import ppl.delite.framework.{DSLType, DeliteApplication}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}

trait ImplicitOps extends DSLType { this: DeliteApplication =>
  /**
   *  Implicit conversion from Rep[X] to Rep[Y]
   *
   *  As long as a conversion is in scope, it will be invoked in the generated scala code.
   *  Code-gen for other platforms should implement the conversions.
   **/
  def implicit_convert[X,Y](x: Rep[X])(implicit c: X => Y) : Rep[Y] // = x.asInstanceOf[Rep[Y]
}

trait ImplicitOpsExp extends ImplicitOps { this: DeliteApplication =>
  case class ImplicitConvert[X,Y](x: Exp[X]) extends Def[Y]

  def implicit_convert[X,Y](x: Exp[X])(implicit c: X => Y) : Rep[Y] = ImplicitConvert[X,Y](x)

  targets.get("Scala").getOrElse(throw new RuntimeException("Couldn't find Scala code generator"))
    .addGenerator( new CodeGeneratorScalaImplicit { val intermediate: ImplicitOpsExp.this.type = ImplicitOpsExp.this })
}

trait CodeGeneratorScalaImplicit extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with ImplicitOpsExp
  import intermediate._

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // TODO: this valDef is redundant; we really just want the conversion to be a no-op in the generated code.
    // TODO: but we still need to link the defs together
    case ImplicitConvert(x) => emitValDef(sym, quote(x))
    case _ => super.emitNode(sym, rhs)
  }
}
