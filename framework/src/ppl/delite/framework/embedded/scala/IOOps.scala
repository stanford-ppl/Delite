package ppl.delite.framework.embedded.scala

import ppl.delite.framework.{DSLType, DeliteApplication}
import java.io.{FileReader, BufferedReader, PrintWriter}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}

trait IOOps extends DSLType { this: DeliteApplication =>

  /**
   * BufferedReader
   */
  object BufferedReader {
    def apply(f: Rep[FileReader]) = obj_br_apply(f)
  }
  def infix_readLine(b: Rep[BufferedReader]) = br_readline(b)
  def infix_close(b: Rep[BufferedReader]) = br_close(b)

  def obj_br_apply(f: Rep[FileReader]) : Rep[BufferedReader]
  def br_readline(b: Rep[BufferedReader]) : Rep[String]
  def br_close(b: Rep[BufferedReader]) : Rep[Unit]

  /**
   * FileReader
   */
  object FileReader {
    def apply(s: Rep[String]) = obj_fr_apply(s)
  }
  def obj_fr_apply(s: Rep[String]) : Rep[FileReader]
}

trait IOOpsExp extends IOOps { this: DeliteApplication with DSLOpsExp =>
  case class ObjBrApply(f: Exp[FileReader])
    extends DSLOp(reifyEffects(External[BufferedReader]("new java.io.BufferedReader(%s)", List(f))))

  case class ObjFrApply(s: Exp[String])
    extends DSLOp(reifyEffects(External[FileReader]("new java.io.FileReader(%s)", List(s))))

  case class BrReadline(b: Exp[BufferedReader]) extends Def[String]
  case class BrClose(b: Exp[BufferedReader]) extends Def[Unit]

  def obj_br_apply(f: Exp[FileReader]) : Rep[BufferedReader] = reflectEffect(ObjBrApply(f))
  def obj_fr_apply(s: Exp[String]) : Rep[FileReader] = reflectEffect(ObjFrApply(s))
  def br_readline(b: Exp[BufferedReader]) : Rep[String] = reflectEffect(BrReadline(b))
  def br_close(b: Exp[BufferedReader]) : Rep[Unit] = reflectEffect(BrClose(b))

  targets.get("Scala").getOrElse(throw new RuntimeException("Couldn't find Scala code generator"))
    .addGenerator( new CodeGeneratorScalaIO { val intermediate: IOOpsExp.this.type = IOOpsExp.this })
}

trait CodeGeneratorScalaIO extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with IOOpsExp
  import intermediate._
  
  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case BrReadline(b) => emitValDef(sym, quote(b) + ".readLine()")
    case BrClose(b) => emitValDef(sym, quote(b) + ".close()")
    case _ => super.emitNode(sym, rhs)
  }
}
