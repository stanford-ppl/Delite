package ppl.delite.framework.codegen

import _root_.scala.virtualization.lms.common.BaseExp
import _root_.scala.virtualization.lms.internal.{Effects, GenericNestedCodegen}
import _root_.scala.virtualization.lms.util.GraphUtil
import java.io.PrintWriter

trait CodeGenerator {
  val intermediate: BaseExp with Effects
  import intermediate._

  //Codegen piece
  def emitValDef(tp: String, sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit
  def emitVarDef(tp: String, sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit


  // merged codegen and nested codegen
  def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter):Unit = rhs match {
    case Reflect(s, effects) =>  emitNode(sym, s)
    case Reify(s, effects) =>
      // just ignore -- effects are accounted for in emitBlock
    case _ => throw new Exception("don't know how to generate code for: " + rhs)
  }

  def quote(x: Exp[_]) = x match {
    case Const(s: String) => "\""+s+"\""
    case Const(z) => z.toString
    case Sym(n) => "x"+n
  }
}