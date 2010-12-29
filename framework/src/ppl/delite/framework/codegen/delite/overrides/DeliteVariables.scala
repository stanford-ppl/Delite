package ppl.delite.framework.codegen.delite.overrides

import scala.virtualization.lms.common.VariablesExp
import java.io.PrintWriter
import scala.virtualization.lms.internal.ScalaGenEffect

trait DeliteVariablesExp extends VariablesExp

trait DeliteScalaGenVariables extends ScalaGenEffect {
  val IR: DeliteVariablesExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ReadVar(Variable(a)) => emitValDef(sym, quote(a) + ".get")
    case NewVar(init) => emitValDef(sym, "generated.scala.Ref(" + quote(init) + ")")
    case Assign(Variable(a), b) => stream.println(quote(a) + ".set(" + quote(b) + ")")
    case VarPlusEquals(Variable(a), b) => emitValDef(sym, quote(a) + ".set(" + quote(a) + ".get +" + quote(b) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}