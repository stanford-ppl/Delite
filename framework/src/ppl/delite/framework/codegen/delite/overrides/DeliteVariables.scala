package ppl.delite.framework.codegen.delite.overrides

import scala.virtualization.lms.common.VariablesExp
import java.io.PrintWriter
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.internal.{CGenEffect, CudaGenEffect, CLikeCodegen, ScalaGenEffect}

trait DeliteScalaGenVariables extends ScalaGenEffect {
  val IR: VariablesExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    val symIsResult = (!deliteResult.isEmpty && deliteResult.get == sym)
    var gen = false
    if (symIsResult) {
      rhs match {
        case NewVar(init) => emitValDef(sym, "generated.scala.Ref(" + quote(init) + ")"); gen = true
        case _ => // pass
      }
    }
    if (!(deliteInputs intersect syms(rhs)).isEmpty) {
      rhs match {
        case ReadVar(Variable(a)) => emitValDef(sym, quote(a) + ".get"); gen = true
        case Assign(Variable(a), b) => stream.println(quote(a) + ".set(" + quote(b) + ")"); gen = true
        case VarPlusEquals(Variable(a), b) => emitValDef(sym, quote(a) + ".set(" + quote(a) + ".get +" + quote(b) + ")"); gen = true
        case _ => // pass
      }
    }

    if (!gen) {
      super.emitNode(sym, rhs)
    }
  }
}

trait DeliteCLikeGenVariables extends CLikeCodegen {
  val IR: VariablesExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = super.emitNode(sym, rhs)

}

trait DeliteCudaGenVariables extends CudaGenEffect with DeliteCLikeGenVariables

trait DeliteCGenVariables extends CGenEffect with DeliteCLikeGenVariables
