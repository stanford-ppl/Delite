package ppl.delite.framework.codegen.scala

import ppl.delite.framework.codegen.CodeGenerator
import java.io.PrintWriter

trait CodeGeneratorScalaBase extends CodeGenerator {

  import intermediate._

  def emitValDef(tp: String="", sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("val " + quote(sym) + " = " + rhs)
  }

  def emitVarDef(tp: String="", sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("var " + quote(sym) + " = " + rhs)
  }

  override def quote(x: Exp[_]) = x match { // TODO: quirk!
    case Sym(-1) => "_"
    case _ => super.quote(x)
  }
}