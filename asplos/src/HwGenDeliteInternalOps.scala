package asplos
import asplos._

import ppl.delite.framework.codegen.delite.DeliteKernelCodegen
//import ppl.delite.framework.codegen.hw.HwCodegen
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

// All IR nodes, GenericGenDeliteOps
import ppl.delite.framework.ops._
import ppl.delite.framework.Config
import ppl.delite.framework.datastructures._

// Analysis passes
// import ppl.delite.framework.analysis.MetaPipelineAnalysis
import ppl.delite.framework.analysis.DotPrintAnalysis

import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.Stack


trait HwGenDeliteInternalOps extends HwGenMaps {
  // val IR: DeliteOpsExp with DeliteInternalOpsExp
  val IR: PPLOpsExp with DeliteInternalOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
//    curSym.push(sym)
    stream.println(s"// sym = $sym, syms($rhs) = ${syms(rhs).map(x => aliasMap.getOrElse(x,x))}")
    rhs match {
      case DIntPlus(lhs,rhs) =>
          val lhsAlias = if (lhs.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(lhs.asInstanceOf[Sym[Any]], lhs) else lhs
          val rhsAlias = if (rhs.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(rhs.asInstanceOf[Sym[Any]], rhs) else rhs
          val symAlias = aliasMap.getOrElse(sym.asInstanceOf[Sym[Any]], sym).asInstanceOf[Sym[Any]]
          emitValDef(symAlias, s"${quote(lhsAlias)} + ${quote(rhsAlias)}")

      case DoublePlus(lhs,rhs) =>
          val lhsAlias = if (lhs.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(lhs.asInstanceOf[Sym[Any]], lhs) else lhs
          val rhsAlias = if (rhs.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(rhs.asInstanceOf[Sym[Any]], rhs) else rhs
          val symAlias = aliasMap.getOrElse(sym.asInstanceOf[Sym[Any]], sym).asInstanceOf[Sym[Any]]
          emitValDef(symAlias, s"${quote(lhsAlias)} + ${quote(rhsAlias)}")
      case DIntMinus(lhs,rhs) =>
          val lhsAlias = if (lhs.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(lhs.asInstanceOf[Sym[Any]], lhs) else lhs
          val rhsAlias = if (rhs.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(rhs.asInstanceOf[Sym[Any]], rhs) else rhs
          val symAlias = aliasMap.getOrElse(sym.asInstanceOf[Sym[Any]], sym).asInstanceOf[Sym[Any]]
          emitValDef(symAlias, s"${quote(lhsAlias)} - ${quote(rhsAlias)}")

      case DIntTimes(lhs,rhs) =>
          val lhsAlias = if (lhs.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(lhs.asInstanceOf[Sym[Any]], lhs) else lhs
          val rhsAlias = if (rhs.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(rhs.asInstanceOf[Sym[Any]], rhs) else rhs
          val symAlias = aliasMap.getOrElse(sym.asInstanceOf[Sym[Any]], sym).asInstanceOf[Sym[Any]]
          emitValDef(symAlias, s"${quote(lhsAlias)} * ${quote(rhsAlias)}")

      case DoubleTimes(lhs,rhs) =>
          val lhsAlias = if (lhs.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(lhs.asInstanceOf[Sym[Any]], lhs) else lhs
          val rhsAlias = if (rhs.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(rhs.asInstanceOf[Sym[Any]], rhs) else rhs
          val symAlias = aliasMap.getOrElse(sym.asInstanceOf[Sym[Any]], sym).asInstanceOf[Sym[Any]]
          emitValDef(symAlias, s"${quote(lhsAlias)} * ${quote(rhsAlias)}")

  //      hwgraph.add(CMul()(sym, kernelDeps))
  //      stream.println(s"Added CMul()($sym, $kernelDeps) to hwgraph")
  //      emitValDef(sym, buildClbCall("clb_mul", List(lhs, rhs)));
      case DIntDivide(lhs,rhs) =>
  //      hwgraph.add(CDiv()(sym, kernelDeps))
  //      stream.println(s"Added CDiv()($sym, $kernelDeps) to hwgraph")
  //      emitValDef(sym, buildClbCall("clb_div", List(lhs, rhs)));
      case DLessThan(lhs,rhs) =>
  //      hwgraph.add(Dummy()(sym, kernelDeps))
  //      stream.println(s"Added Dummy()($sym, $kernelDeps) to hwgraph")
  //      emitValDef(sym, buildClbCall("clb_lt", List(lhs, rhs)));
      case DGreaterThan(lhs,rhs) =>
  //      hwgraph.add(Dummy()(sym, kernelDeps))
  //      stream.println(s"Added Dummy()($sym, $kernelDeps) to hwgraph")
  //      emitValDef(sym, buildClbCall("clb_gt", List(lhs, rhs)));
      case _ => super.emitNode(sym,rhs)
  //    case DBooleanNegate(b) => emitValDef(sym, "!" + quote(b))
  //    case DEqual(a,b) =>  emitValDef(sym, quote(a) + " == " + quote(b))
  //    case DNotEqual(a,b) =>  emitValDef(sym, quote(a) + " != " + quote(b))
  //    case DIntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
  //    case DUnsafeImmutable(x) => emitValDef(sym, quote(x) + "// unsafe immutable")
    }
  //  curSym.pop
  }
}
