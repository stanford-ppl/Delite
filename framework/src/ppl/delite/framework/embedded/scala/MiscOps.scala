package ppl.delite.framework.embedded.scala

import java.io.PrintWriter
import scala.virtualization.lms.internal.Effects
import ppl.delite.framework.codegen.CodeGenerator
import ppl.delite.framework.codegen.c.{CodeGeneratorCBase, TargetC}
import scala.virtualization.lms.ppl.{ScalaOps, ScalaOpsExp}
import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}
import scala.virtualization.lms.common.{EffectExp, BaseExp}


trait MiscOps extends DSLType { this: DeliteApplication =>

  /**
   * Other things that need to get lifted like exit, there should be
   * a better way to do this
   */

  def print(x: Rep[Any]): Rep[Unit]
  def println(x: Rep[Any]): Rep[Unit]

  // TODO: there is no way to override this behavior
  def exit(status: Int): Rep[Nothing] = exit(unit(status))
  def exit(): Rep[Nothing] = exit(0)
  def exit(status: Rep[Int]): Rep[Nothing]
}



trait MiscOpsExp extends MiscOps { this: DeliteApplication =>

  case class Print(x: Exp[Any]) extends Def[Unit]
  case class PrintLn(x: Exp[Any]) extends Def[Unit]
  case class Exit(s: Exp[Int]) extends Def[Nothing]

  def print(x: Rep[Any]) = reflectEffect(Print(x))
  def println(x: Rep[Any]) = reflectEffect(PrintLn(x))
  def exit(s: Rep[Int]) = reflectEffect(Exit(s))

  targets.get("Scala").getOrElse(throw new RuntimeException("Couldn't find Scala code generator"))
    .addGenerator( new CodeGeneratorScalaMisc { val intermediate: MiscOpsExp.this.type = MiscOpsExp.this })
    
  //targets.get("C").getOrElse(throw new RuntimeException("Couldn't find C code generator"))
  //  .addGenerator( new CodeGeneratorCMisc { val intermediate: MiscOpsExp.this.type = MiscOpsExp.this; })
}

trait CodeGeneratorScalaMisc extends CodeGeneratorScalaBase { 

  val intermediate: DeliteApplication with MiscOpsExp with EffectExp
  import intermediate._

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case PrintLn(s) => emitValDef(sym, "println(" + quote(s) + ")")
    case Print(s) => emitValDef(sym, "print(" + quote(s) + ")")
    case Exit(a) => emitValDef(sym, "exit(" + quote(a) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}


//todo factor out commonality
trait CodeGeneratorCMisc extends CodeGeneratorCBase {

  val intermediate: DeliteApplication with MiscOpsExp with EffectExp
  import intermediate._
  
  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case PrintLn(s) => stream.println("printf(\"%s\\n\"," + quote(s) + ");")
    case Print(s) => stream.println("printf(\"%s\"," + quote(s) + ");")
    case Exit(a) => stream.println("exit(" + quote(a) + ");")

    case _ => super.emitNode(sym, rhs)
  }

}
