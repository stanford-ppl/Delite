package ppl.delite.framework.codegen.delite.overrides

import scala.virtualization.lms.common.VariablesBridge
import scala.virtualization.lms.common.{ScalaGenEffect,CudaGenEffect,CGenEffect}
import java.io.PrintWriter
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.internal.{CLikeCodegen}

trait DeliteScalaGenVariables extends ScalaGenEffect {
  val IR: VariablesBridge with DeliteOpsExp // FIXME: imports VariablesExp via DeliteOpsExp
  import IR.{__newVar => _, __assign => _, _}

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    val symIsResult = !deliteResult.isEmpty && (deliteResult.get contains sym)
    var gen = false
    if (symIsResult) {
      rhs match {
        case NewVar(init) => emitValDef(sym, "generated.scala.Ref(" + quote(getBlockResult(init)) + ")"); gen = true
        case _ => // pass
      }
    }
    if (!(deliteInputs intersect syms(rhs)).isEmpty) {
      rhs match {
        case ReadVar(Variable(a)) => emitValDef(sym, quote(a) + ".get"); gen = true
        case Assign(Variable(a), b) => emitValDef(sym, quote(a) + ".set(" + quote(getBlockResult(b)) + ")"); gen = true
        case VarPlusEquals(Variable(a), b) => emitValDef(sym, quote(a) + ".set(" + quote(a) + ".get +" + quote(getBlockResult(b)) + ")"); gen = true
        case _ => // pass
      }
    }

    if (!gen) {
      super.emitNode(sym, rhs)
    }
  }
}

trait DeliteCLikeGenVariables extends CLikeCodegen {
  val IR: VariablesBridge
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = super.emitNode(sym, rhs)

}

trait DeliteCudaGenVariables extends CudaGenEffect with DeliteCLikeGenVariables

trait DeliteCGenVariables extends CGenEffect with DeliteCLikeGenVariables
