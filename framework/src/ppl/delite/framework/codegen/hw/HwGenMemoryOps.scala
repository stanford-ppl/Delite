package ppl.delite.framework.codegen.hw

import ppl.delite.framework.codegen.delite.DeliteKernelCodegen
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

// All IR nodes, GenericGenDeliteOps
import ppl.delite.framework.ops._
import ppl.delite.framework.Config
import ppl.delite.framework.datastructures._

// Analysis passes
import ppl.delite.framework.analysis.PrimitiveReduceAnalysis
import ppl.delite.framework.analysis.MetaPipelineAnalysis
import ppl.delite.framework.analysis.DotPrintAnalysis

import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.Stack

trait HwGenDeliteArrayOps extends HwCodegen with BaseGenDeliteArrayOps
{
  val IR: DeliteArrayFatExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    val aliasSym = aliasMap.getOrElse(sym, sym).asInstanceOf[Sym[Any]]
    curSym.push(aliasSym)
    rhs match {
      case DeliteArrayNew(n,m,t) =>
        val aliasN = if (n.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(n.asInstanceOf[Sym[Any]], n) else n
        if (dblBufMap.contains(aliasSym)) {
          baseKernelLibStream.println(s"protected static DblBufKernelLib ${quote(aliasSym)};")
          stream.println(s"""${quote(aliasSym)} = new DblBufKernelLib (this, \"${quote(aliasSym)}\", ${quote(aliasN)});""")
        } else {
          baseKernelLibStream.println(s"protected static BramLib ${quote(aliasSym)};")
          stream.println(s"""${quote(aliasSym)} = new BramLib(this, \"${quote(aliasSym)}\", ${quote(aliasN)});""")
        }

      case DeliteArrayApply(arr, idx) =>
        val aliasArr = aliasMap.getOrElse(arr.asInstanceOf[Sym[Any]], arr)
        val aliasIdx = aliasMap.getOrElse(idx, idx)
        stream.println(s"${quote(aliasArr)}.raddr <== ${quote(aliasIdx)};")
        emitValDef(aliasSym, s"${quote(aliasArr)}.rdata")

      case DeliteArrayUpdate(arr,idx,v) =>
        val aliasArr = aliasMap.getOrElse(arr.asInstanceOf[Sym[Any]], arr)
        val aliasIdx = aliasMap.getOrElse(idx.asInstanceOf[Sym[Any]], idx)
        val aliasV = aliasMap.getOrElse(v.asInstanceOf[Sym[Any]], v)
        stream.println(s"${quote(aliasArr)}.waddr <== ${quote(aliasIdx)};")
        stream.println(s"${quote(aliasArr)}.wdata <== ${quote(aliasV)};")

      case DeliteArrayLength(arr) =>
        val aliasArr = aliasMap.getOrElse(arr.asInstanceOf[Sym[Any]], arr)
        stream.println("// code for DeliteArrayLength goes here")
        emitValDef(aliasSym, "DeliteArrayLength")
      case _=>
        super.emitNode(sym, rhs)
    }
    curSym.pop
  }
}
