package ppl.delite.framework.codegen.c

import ppl.delite.framework.codegen.CodeGenerator
import java.io.PrintWriter

trait CodeGeneratorCBase extends CodeGenerator {

  import intermediate._

  def emitValDef(tp: String, sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.print("const ")
    emitVarDef(tp, sym, rhs)
  }
  def emitVarDef(tp: String, sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(tp + " " + quote(sym) + " = " + rhs + ";")
  }
}