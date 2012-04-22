package ppl.delite.framework.codegen.delite.overrides

import java.io.PrintWriter
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.internal.{CLikeCodegen}
import scala.virtualization.lms.common._

trait DeliteScalaGenVariables extends ScalaGenEffect {
  val IR: VariablesExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    val symIsResult = !deliteResult.isEmpty && (deliteResult.get contains sym)
    var gen = false
    if (symIsResult) {
      rhs match {
        case NewVar(init) => emitValDef(sym, "new generated.scala.Ref(" + quote(init) + ")"); gen = true
        case _ => // pass
      }
    }
    if (!(deliteInputs intersect syms(rhs)).isEmpty) {
      rhs match {
        case ReadVar(Variable(a)) => emitValDef(sym, quote(a) + ".get"); gen = true
        case Assign(Variable(a), b) => emitValDef(sym, quote(a) + ".set(" + quote(b) + ")"); gen = true
        case VarPlusEquals(Variable(a), b) => emitValDef(sym, quote(a) + ".set(" + quote(a) + ".get + " + quote(b) + ")"); gen = true
        case VarMinusEquals(Variable(a), b) => emitValDef(sym, quote(a) + ".set(" + quote(a) + ".get - " + quote(b) + ")"); gen = true
        case _ => // pass
      }
    }

    if (!gen) {
      super.emitNode(sym, rhs)
    }
  }
}

trait DeliteCLikeGenVariables extends CLikeCodegen {
  val IR: VariablesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = super.emitNode(sym, rhs)

}

trait DeliteCudaGenVariables extends CudaGenEffect with DeliteCLikeGenVariables

trait DeliteOpenCLGenVariables extends OpenCLGenEffect with DeliteCLikeGenVariables

trait DeliteCGenVariables extends CGenEffect with DeliteCLikeGenVariables {
  val IR: VariablesExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    val symIsResult = !deliteResult.isEmpty && (deliteResult.get contains sym)
    var gen = false
    if (symIsResult) {
      rhs match {
        case NewVar(init) => emitValDef(sym, quote(init)); gen = true
        case _ => // pass
      }
    }
    if (!(deliteInputs intersect syms(rhs)).isEmpty) {
      rhs match {
        case ReadVar(Variable(a)) => emitValDef(sym, quote(a)); gen = true
        case Assign(Variable(a), b) => stream.println(quote(a) + " = " + quote(b) + ";"); gen = true
        case VarPlusEquals(Variable(a), b) => stream.println(quote(a) + " += " + quote(b) + ";"); gen = true
        case VarMinusEquals(Variable(a), b) => stream.println(quote(a) + " -= " + quote(b) + ";"); gen = true
        case _ => // pass
      }
    }

    if (!gen) {
      super.emitNode(sym, rhs)
    }
  }
}
