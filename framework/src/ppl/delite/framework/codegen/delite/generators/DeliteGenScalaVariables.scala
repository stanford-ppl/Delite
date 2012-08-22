package ppl.delite.framework.codegen.delite.generators

import scala.virtualization.lms.common.{ VariablesExp, ScalaGenEffect, CGenEffect }
import java.io.PrintWriter

trait DeliteGenScalaVariables extends ScalaGenEffect {
  val IR: VariablesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ReadVar(Variable(a)) => emitValDef(sym, quote(a) + ".get")
    case NewVar(init) => emitValDef(sym, "generated.scala.Ref(" + quote(init) + ")")
    case Assign(Variable(a), b) => stream.println(quote(a) + ".set(" + quote(b) + ")")
    case VarPlusEquals(Variable(a), b) => emitValDef(sym, quote(a) + ".set(" + quote(a) + ".get +" + quote(b) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

