// package ppl.delite.framework.codegen.hw
package asplos
import asplos._

//import ppl.delite.framework.codegen.hw.HwCodegen
import ppl.delite.framework.codegen.delite.DeliteKernelCodegen
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._

// All IR nodes, GenericGenDeliteOps
import ppl.delite.framework.ops._
import ppl.delite.framework.Config
import ppl.delite.framework.datastructures._

// Analysis passes
import ppl.delite.framework.analysis.PrimitiveReduceAnalysis
// import ppl.delite.framework.analysis.MetaPipelineAnalysis
import ppl.delite.framework.analysis.DotPrintAnalysis

import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.Stack

trait HwGenDeliteArrayOps extends HwGenMaps with BaseGenDeliteArrayOps
{
  val IR: PPLOpsExp // DeliteArrayFatExp with PPLOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    val aliasSym = aliasMap.getOrElse(sym, sym).asInstanceOf[Sym[Any]]
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
        validMemorySet += aliasSym

      case DeliteArrayApply(arr, idx) =>
        val aliasArr = aliasMap.getOrElse(arr.asInstanceOf[Sym[Any]], arr)
        val aliasIdx = aliasMap.getOrElse(idx, idx)
        stream.println(s"//$aliasSym = DeliteArrayApply($aliasArr, $aliasIdx)")
        if (!validMemorySet.contains(aliasArr)) {
            val d = getdef(aliasArr.asInstanceOf[Sym[Any]])
            d match {
              case FieldApply(_,_) =>
                emitNode(aliasArr.asInstanceOf[Sym[Any]], d)
                emitNode(sym, rhs)
              case _ =>
                stream.println(s"// alias map: $aliasMap")
                stream.println(s"// ERROR generating DeliteArrayApply($arr, $idx): Aliased array $aliasArr (defined by $d) is not present in valid memory set!\nValid memory set: $validMemorySet")
            }
        }
          stream.println(s"${quote(aliasArr)}.raddr <== ${quote(aliasIdx)};")
          emitValDef(aliasSym, s"${quote(aliasArr)}.rdata")

      case DeliteArrayUpdate(arr,idx,v) =>
        val aliasArr = aliasMap.getOrElse(arr.asInstanceOf[Sym[Any]], arr)
        val aliasIdx = aliasMap.getOrElse(idx.asInstanceOf[Sym[Any]], idx)
        val aliasV = aliasMap.getOrElse(v.asInstanceOf[Sym[Any]], v)
        if (!validMemorySet.contains(aliasArr)) {
            val d = getdef(aliasArr.asInstanceOf[Sym[Any]])
            d match {
              case FieldApply(_,_) =>
                emitNode(aliasArr.asInstanceOf[Sym[Any]], d)
                emitNode(sym, rhs)
              case _ =>
                stream.println(s"// alias map: $aliasMap")
                stream.println(s"ERROR generating DeliteArrayUpdate($arr, $idx): Aliased array $aliasArr (defined by $d) is not present in valid memory set!\nValid memory set: $validMemorySet")
            }
        }
        stream.println(s"${quote(aliasArr)}.waddr <== ${quote(aliasIdx)};")
        stream.println(s"${quote(aliasArr)}.wdata <== ${quote(aliasV)};")
        if (dblBufMap.contains(aliasSym)) {
          stream.println(s"${quote(aliasArr)}.wen <== ${quote(curSym.top)}_en;")
        }


      case DeliteArrayLength(arr) =>
        val aliasArr = aliasMap.getOrElse(arr.asInstanceOf[Sym[Any]], arr)
        stream.println(s"// ERROR: $aliasSym = DeliteArrayLength($aliasArr) (aliased from $arr) goes here")
      case _=>
        super.emitNode(sym, rhs)
    }
  }
}
