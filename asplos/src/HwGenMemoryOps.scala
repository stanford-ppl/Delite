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
          val constN = findConst(aliasN)
          val numPorts = dblBufMap(aliasSym)
          baseKernelLibStream.println(s"protected static DblBufKernelLib_np ${quote(aliasSym)};")
          stream.println(s"""${quote(aliasSym)} = new DblBufKernelLib_np (this, \"${quote(aliasSym)}\", ${quote(constN)}, ${numPorts}, ${quote(aliasSym)}_done);""")
        } else {
          baseKernelLibStream.println(s"protected static BramLib ${quote(aliasSym)};")
          stream.println(s"""${quote(aliasSym)} = new BramLib(this, \"${quote(aliasSym)}\", ${quote(aliasSym)});""")
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
        val rdStr = if (dblBufMap.contains(aliasArr.asInstanceOf[Sym[Any]])) {
          if (aliasArr.asInstanceOf[Sym[Any]].id == curSym.top.id) {
            // When a reduce loop is reading from its dest buffer, don't assign rdone signal
            s"${quote(aliasArr)}.connectRport(${quote(aliasIdx)})"
          } else {
            s"${quote(aliasArr)}.connectRport(${quote(aliasIdx)}, ${quote(curSym.top)}_done)"
          }
        } else {
          s"${quote(aliasArr)}.connectRport(${quote(aliasIdx)})"
//          stream.println(s"${quote(aliasArr)}.raddr <== ${quote(aliasIdx)};")
//          emitValDef(aliasSym, s"${quote(aliasArr)}.rdata")
        }

        if (memoryMuxMap.contains(aliasArr)) {
          val muxMem = memoryMuxMap(aliasArr)._1
          val muxStr = memoryMuxMap(aliasArr)._2
          val muxMemRdStr = s"${quote(muxMem)}.connectRport(${quote(aliasIdx)}, ${quote(curSym.top)}_done)"
          emitValDef(aliasSym, s"$muxStr : ${muxMemRdStr} : $rdStr")
        } else {
          emitValDef(aliasSym, s"$rdStr")
        }
        stream.println(s"// DeliteArrayApply")

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
        if (dblBufMap.contains(aliasArr.asInstanceOf[Sym[Any]])) {
          stream.println(s"${quote(aliasArr)}.connectWport(${quote(aliasIdx)}, ${quote(aliasV)}, ${quote(curSym.top)}_en);")
//          stream.println(s"${quote(aliasArr)}.wen <== ${quote(curSym.top)}_en;")
        } else {
          stream.println(s"${quote(aliasArr)}.waddr <== ${quote(aliasIdx)};")
          stream.println(s"${quote(aliasArr)}.wdata <== ${quote(aliasV)};")
        }


      case DeliteArrayLength(arr) =>
        val aliasArr = aliasMap.getOrElse(arr.asInstanceOf[Sym[Any]], arr)
        stream.println(s"// ERROR: $aliasSym = DeliteArrayLength($aliasArr) (aliased from $arr) goes here")
      case _=>
        super.emitNode(sym, rhs)
    }
  }
}
