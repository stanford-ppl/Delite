package ppl.dsl.deliszt.analysis

import java.io.{PrintWriter}

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException}
import ppl.delite.framework.DeliteApplication

import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
import ppl.dsl.deliszt.datastruct.scala._

import ppl.dsl.deliszt._

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 07/29/2011
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait DeLisztCodeGenAnalysis extends DeLisztCodeGenScala {
  val IR: DeliteApplication with DeLisztExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // While loops
    case While(c,b) =>
      stream.print("{")
      emitBlock(c)
      stream.println("}")
      stream.print("val " + quote(sym) + " = ")
      stream.println("{")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
    // Execute both branches
    case IfThenElse(c,a,b) =>
      stream.println("{" + quote(c) + "} {")
      emitBlock(a)
      stream.println("}")
      stream.println("val " + quote(sym) + " = {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
    // Foreach, only apply to top level foreach though...
    case MeshSetForeach(m, f) =>
      stream.println("generated.scala.StencilCollector.mark_foreach(" + sym.id + ")")
      super.emitNode(sym, rhs)
      stream.println("generated.scala.StencilCollector.unmark_foreach(" + sym.id + ")")
    // Just mark accesses
    case FieldApply(f,i) =>
      stream.println("generated.scala.StencilCollector.FieldRead(" + quote(f) + "," + quote(i) + ")")
    case FieldPlusUpdate(f,i,v) =>
      stream.println("generated.scala.StencilCollector.FieldReduce(\"+\"," + quote(f) + "," + quote(i) + "," + quote(v) + ")")
    case FieldTimesUpdate(f,i,v) =>
      stream.println("generated.scala.StencilCollector.FieldReduce(\"*\"," + quote(f) + "," + quote(i) + "," + quote(v) + ")")
    case FieldMinusUpdate(f,i,v) =>
      stream.println("generated.scala.StencilCollector.FieldReduce(\"-\"," + quote(f) + "," + quote(i) + "," + quote(v) + ")")
    case FieldDivideUpdate(f,i,v) =>
      stream.println("generated.scala.StencilCollector.FieldReduce(\"/\"," + quote(f) + "," + quote(i) + "," + quote(v) + ")")
    // Try to get rid of arithmetic?
    case ArithPlus(l,r) => super.emitNode(sym, rhs)
    case ArithMinus(l,r) => super.emitNode(sym, rhs)
    case ArithTimes(l,r) => super.emitNode(sym, rhs)
    case ArithNegate(l) => super.emitNode(sym, rhs)
    case ArithFractionalDivide(l,r) => super.emitNode(sym, rhs)
    case ArithAbs(l) => super.emitNode(sym, rhs)
    case ArithExp(l) => super.emitNode(sym, rhs)
    case OrderingMin(l,r) => super.emitNode(sym, rhs)
    case OrderingMax(l,r) => super.emitNode(sym, rhs)
    case _ => super.emitNode(sym, rhs)
  }
}
