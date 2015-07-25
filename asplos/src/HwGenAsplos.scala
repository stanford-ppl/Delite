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

trait HwGenAsplos extends HwGenMaps with BaseGenDeliteArrayOps
{
  val IR: PPLOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    val aliasSym = aliasMap.getOrElse(sym, sym).asInstanceOf[Sym[Any]]
    rhs match {
      case op: BlockSlice[_,_,_] =>
        stream.println("// BlockSlice allocation - always allocated to a double buffer")
        dblBufMap += aliasSym
        val array2DAnalysis = new Array2DHackAnalysis{val IR: HwGenAsplos.this.IR.type = HwGenAsplos.this.IR}
        array2DAnalysis.run(op.allocTile, sym)
        aliasMap ++= array2DAnalysis.allocAliasMap
        reverseStructMap ++= array2DAnalysis.reverseStructMap
        structInfoMap ++= array2DAnalysis.structInfoMap
        emitBlock(op.allocTile)

        stream.println("// The command stream")
        if (op.m != 2) {
          stream.println("// Handling only 2-D block slices now")
        } else {
          stream.println(s"""
              BlkLdStLib ${quote(aliasSym)}_blkLoader = new BlkLdStLib(
                      owner,
                      ${quote(aliasSym)}_en,
                      ${quote(aliasSym)}_done,
                      ${quote(op.srcDims(1))},
                      dfeUInt(32), // TODO: Change this to use remap
                      ${quote(op.srcOffsets(op.deltaInds(0)))},
                      ${quote(op.srcOffsets(op.deltaInds(1)))},
                      io.scalarInput(\"${quote(aliasSym)}_boffset\"),  // sBurstOffset,
                      \"${quote(aliasSym)}_cmd\",
                      ${quote(op.destDims(op.deltaInds(0)))},
                      ${quote(op.destDims(op.deltaInds(1)))});""")

          stream.println("// The data stream")
          stream.println(s"""
            Count.Params ${quote(aliasSym)}_writeAddrParams = control.count.makeParams(MathUtils.bitsToAddress(${quote(op.destDims(op.deltaInds(0)))}*${quote(op.destDims(op.deltaInds(1)))})
                                      .withEnable(${quote(aliasSym)}_en)
                                      .withMax(${quote(op.destDims(op.deltaInds(0)))}*${quote(op.destDims(op.deltaInds(1)))});
            Count.Counter ${quote(aliasSym)}_waddr = control.count.makeCounter(${quote(aliasSym)}_writeAddrParams);

            ${quote(aliasSym)}.waddr <== ${quote(aliasSym)}_waddr.getCount();
            ${quote(aliasSym)}.wdata <== io.input(\"${quote(aliasSym)}_data\", dfeUInt(32), ${quote(aliasSym)}_en);
            ${quote(aliasSym)}.wen <== ${quote(aliasSym)}_en;""")
        }
        case Array2DNew(arr, d1, d2) =>
          stream.println(s"//  ${quote(sym)} = Array2DNew(${quote(arr)}, ${quote(d1)}, ${quote(d2)})")
          stream.println(s"// TODO: Need to know max bounds of dimensions $d1 and $d2 in order to allocate array $arr!")

      case FieldApply(struct, field) =>
        def startMemorySearch(s: Exp[Any]): Exp[Any] = {
          val d = getdef(s.asInstanceOf[Sym[Any]])
          d match {
            case BlockSlice(_,_,_,_,_) => aliasMap.getOrElse(s,s)
            case DeliteArrayNew(_,_,_) => aliasMap.getOrElse(s,s)
            case op: AbstractLoop[_] => aliasMap.getOrElse(s,s)
            case op: AbstractLoopNest[_] => aliasMap.getOrElse(s,s)
            case _ =>
              if (aliasMap.contains(s)) {
                startMemorySearch(aliasMap(s))
              } else {
                stream.println(s"// Could not find memory, last sym found: $s, $d")
                s
              }
          }
        }

        stream.println(s"// ${quote(sym)} = FieldApply(${quote(struct)}, $field)")
        val aliasSym = aliasMap.getOrElse(struct,struct)
        if (field != "data") {
          Console.println(s"$sym (aliasSym = $sym), $rhs")
          if (!reverseStructMap.contains(aliasSym)) {
            stream.println(s"// Could not find symbol $aliasSym (aliased from $sym) in reverseStructMap!")
          } else {
            val aliasStruct = reverseStructMap(aliasSym)
            val structInfo = structInfoMap((aliasStruct, field))
            aliasMap += sym -> structInfo
          }
        } else {
          // Trace the aliasSym to find the first memory node that is present in the valid memory set
          val origMemSym = startMemorySearch(aliasSym)
          aliasMap += sym -> origMemSym
        }

      case NestedAtomicWrite(struct, tracer, atomicWrite) =>
        val aliasSym = aliasMap.getOrElse(struct,struct)
        stream.println(s"// ${quote(sym)} NestedAtomicWrite(${quote(struct)}, $tracer, $atomicWrite")
        stream.println(s"// Alias ${quote(sym)}: $aliasSym")
        atomicWrite match {
          case DeliteArrayUpdate(arr,_,_) =>
            aliasMap += arr -> aliasSym
            emitNode(sym, atomicWrite)
          case _ => stream.println("// Something else")
        }

      case RangeVectorNew(start, stride, length) =>
        stream.println(s"// RangeVector($start, $stride, $length)")

      case IntToLong(lhs) =>
        stream.println(s"DFEVar ${quote(sym)} = ${quote(lhs)}.cast(dfeUInt(64));")
      case LongToDouble(lhs) =>
        stream.println(s"DFEVar ${quote(sym)} = ${quote(lhs)}.cast(dfeFloat(11,53));")
      case MathMin(v1, v2) =>
        stream.println(s"DFEVar ${quote(sym)} = KernelMath.min(${quote(v1)}, ${quote(v2)})")

      case _=>
        super.emitNode(sym, rhs)
    }
  }
}
