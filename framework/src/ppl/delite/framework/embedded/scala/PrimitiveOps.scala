package ppl.delite.framework.embedded.scala

import java.io.PrintWriter
import ppl.delite.framework.{DSLType, DeliteApplication}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}

trait PrimitiveOps extends DSLType { this: DeliteApplication =>
  object Double {
    def parseDouble(s: Rep[String]) = obj_double_parse_double(s)
  }

  def obj_double_parse_double(s: Rep[String]) : Rep[Double]
}
trait PrimitiveOpsExp extends PrimitiveOps { this: DeliteApplication =>
  case class ObjDoubleParseDouble(s: Exp[String]) extends Def[Double]

  def obj_double_parse_double(s: Exp[String]) = ObjDoubleParseDouble(s)

  targets.get("Scala").getOrElse(throw new RuntimeException("Couldn't find Scala code generator"))
    .addGenerator( new CodeGeneratorScalaPrimitive { val intermediate: PrimitiveOpsExp.this.type = PrimitiveOpsExp.this })
}

trait CodeGeneratorScalaPrimitive extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with PrimitiveOpsExp
  import intermediate._
  
  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {

    case ObjDoubleParseDouble(s) => emitValDef(sym, "java.lang.Double.parseDouble(" + quote(s) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}