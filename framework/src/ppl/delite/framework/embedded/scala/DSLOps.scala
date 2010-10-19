package ppl.delite.framework.embedded.scala

import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}

trait DSLOpsExp { this: DeliteApplication =>

  // representation must be reified! this places the burden on the caller, but allows the caller to avoid the
  // use of function values (which can be uglier).
  case class DSLOp[A](val representation: Exp[A]) extends Def[A]

  targets.get("Scala").getOrElse(throw new RuntimeException("Couldn't find Scala code generator"))
    .addGenerator( new CodeGeneratorScalaDSL { val intermediate: DSLOpsExp.this.type = DSLOpsExp.this })
}

trait CodeGeneratorScalaDSL extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with DSLOpsExp
  import intermediate._
  
  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {    
    case op@DSLOp(b) =>
      stream.println("val " + quote(sym) + " = { ")
      emitBlock(b, intermediate.targets.get("Scala").get)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}
