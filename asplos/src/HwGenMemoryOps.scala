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

  // Scan through the curSym stack, keeping track
  // of the outermost loop that exists in ls. Return when either
  // we've run through the stack, or a sym doesn't exist in ls
  private def findOuterLoop(ls: List[Sym[Any]]) = {
    var found = true
    var i = 0
    var outerLoop: Sym[Any] = null
    while (found && i < curSym.length) {
      if (ls.contains(curSym(i))) {
        outerLoop = curSym(i)
      }
      i += 1
    }
    outerLoop
  }

  private def findOuterLoops(ls: ListBuffer[Sym[Any]]) = {
    val retLoop = ListBuffer[Sym[Any]]()
    for (l1 <- ls) {
      val outerLoops = outerLoopMap(l1)
      var curOuterLoop = l1
      for (l2 <- ls) {
        if (outerLoops.contains(l2)) {
          curOuterLoop = l2
        }
      }
      retLoop += curOuterLoop
    }
    retLoop.distinct.toList
  }
//    var i:Int = 0
//    val lsl = ls.clone
//    val outbuf = ListBuffer[Exp[Any]]()
//    for (l <- lsl) {
//      val outerLoops = outerLoopMap(l)
//      lsl --= outerLoops.asInstanceOf[List[Sym[Any]]]
//      outbuf.appendAll(outerLoops.intersect(ls))
//    }
//    stream.println(s"// Outerloops in $ls: $outbuf")
//    outbuf.toList
//  }


  private def emitMemoryAlloc(meminfo: MemInfo) = {
    val name = quote(meminfo.memSym)
    val loops = findOuterLoops(meminfo.readers.distinct)
    val ports = loops.size
     if (meminfo.isDblBuf) {
       baseKernelLibStream.println(s"protected static DblBufKernelLib_np $name;")
       stream.println(s"""$name = new DblBufKernelLib_np (this, \"$name\", ${meminfo.depth}, ${ports}, ${name}_done);""")
     } else {
        baseKernelLibStream.println(s"protected static BramLib ${name};")
        stream.println(s"""${name} = new BramLib(this, \"${name}\", ${meminfo.depth});""")
     }
  }


  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    val aliasSym = aliasMap.getOrElse(sym, sym).asInstanceOf[Sym[Any]]
    rhs match {
      case DeliteArrayNew(n,m,t) =>
        val meminfo = validMemoryMap(aliasSym)
        meminfo.location.toString match {
          case "OFF_CHIP" =>
            // Keep track of off-chip memories, we need to assign bffer offsets and scalar IO for dimensions later on
            stream.println(s"// ${quote(aliasSym)} is OFF_CHIP ${meminfo.location}, not allocating anything")
          case "ON_CHIP" =>
            emitMemoryAlloc(meminfo)
        }

//        val aliasN = if (n.isInstanceOf[Sym[Any]]) aliasMap.getOrElse(n.asInstanceOf[Sym[Any]], n) else n
//        if (dblBufMap.contains(aliasSym)) {
//          val constN = findConst(aliasN, aliasMap)
//          val numPorts = dblBufMap(aliasSym)
//          baseKernelLibStream.println(s"protected static DblBufKernelLib_np ${quote(aliasSym)};")
//          stream.println(s"""${quote(aliasSym)} = new DblBufKernelLib_np (this, \"${quote(aliasSym)}\", ${quote(constN)}, ${numPorts}, ${quote(aliasSym)}_done);""")
//        } else {
//          baseKernelLibStream.println(s"protected static BramLib ${quote(aliasSym)};")
//          stream.println(s"""${quote(aliasSym)} = new BramLib(this, \"${quote(aliasSym)}\", ${quote(aliasSym)});""")
//        }
//        validMemorySet += aliasSym

      case DeliteArrayApply(arr, idx) =>
        val aliasArr = aliasMap.getOrElse(arr.asInstanceOf[Sym[Any]], arr)
        val aliasIdx = aliasMap.getOrElse(idx, idx)
        stream.println(s"//$aliasSym = DeliteArrayApply($aliasArr, $aliasIdx)")
        val mi = validMemoryMap(aliasArr)
        val reader = findOuterLoop(mi.readers.distinct.toList)
        val rdStr = if (mi.isDblBuf) {
          if (aliasArr.asInstanceOf[Sym[Any]].id == curSym.top.id) {
//          if (mi.readers.contains(curSym.top)) {
            // When a reduce loop is reading from its dest buffer, don't assign rdone signal
            s"${quote(aliasArr)}.connectRport(${quote(aliasIdx)})"
          } else {
            s"${quote(aliasArr)}.connectRport(${quote(aliasIdx)}, ${quote(reader)}_done)"
          }
        } else {
          throw new Exception("Don't know how to read from BRAM yet :-)")
          s"${quote(aliasArr)}.connectRport(${quote(aliasIdx)})"
        }

        if (phantomMemReadMap.contains(aliasArr)) {
          val muxMem = phantomMemReadMap(aliasArr)._1
          val muxStr = phantomMemReadMap(aliasArr)._2
          val muxMemRdStr = s"${quote(muxMem)}.connectRport(${quote(aliasIdx)}, ${quote(reader)}_done)"
          emitValDef(aliasSym, s"$muxStr ? ${muxMemRdStr} : $rdStr")
        } else {
          emitValDef(aliasSym, s"$rdStr")
        }


//        if (memoryMuxMap.contains(aliasArr)) {
//          val muxMem = memoryMuxMap(aliasArr)._1
//          val muxStr = memoryMuxMap(aliasArr)._2
//          val muxMemRdStr = s"${quote(muxMem)}.connectRport(${quote(aliasIdx)}, ${quote(curSym.top)}_done)"
//          emitValDef(aliasSym, s"$muxStr : ${muxMemRdStr} : $rdStr")
//        } else {
//          emitValDef(aliasSym, s"$rdStr")
//        }

      case DeliteArrayUpdate(arr,idx,v) =>
        val aliasArr = aliasMap.getOrElse(arr.asInstanceOf[Sym[Any]], arr)
        val name = quote(aliasArr)
        val aliasIdx = aliasMap.getOrElse(idx.asInstanceOf[Sym[Any]], idx)
        val aliasV = aliasMap.getOrElse(v.asInstanceOf[Sym[Any]], v)
        val mi = validMemoryMap(aliasArr)
        if (delayWriteMap.contains(aliasArr)) {
          val offsetStr = delayWriteMap(aliasArr)
          stream.println(s"""
          // OffsetExpr $offsetStr = stream.makeOffsetAutoLoop(\"$offsetStr\");
          DFEVar ${name}_wdata = stream.offset(${quote(aliasV)}, -$offsetStr);
          DFEVar ${name}_wen = ${name}_en;
          DFEVar ${name}_waddr = stream.offset(${quote(aliasIdx)}, -$offsetStr);""")
        }
        if (mi.isDblBuf) {
          val writer = mi.writers.filter(_ == curSym.top)(0)
          if (delayWriteMap.contains(aliasArr)) {
            stream.println(s"${quote(aliasArr)}.connectWport(${name}_waddr, ${name}_wdata, ${name}_wen);")
          } else {
            stream.println(s"${quote(aliasArr)}.connectWport(${quote(aliasIdx)}, ${quote(aliasV)}, ${quote(writer)}_en);")
          }
        } else {
          throw new Exception("Don't know how to write to BRAM yet :-)")
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
