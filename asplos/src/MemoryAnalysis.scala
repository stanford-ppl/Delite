package asplos
import asplos._

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{AbstractSubstTransformer,Transforming,FatBlockTraversal}
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.Config

import scala.collection.mutable.ListBuffer

/*
 * @MemoryAnalysis: Traverse the IR and collect memory
 * information. In order to be able to allocate resources on-chip,
 * we have to be certain that a memory allocation fits. If the
 * size is compile-time known and is below a threshold, or if
 * we are given a hint that a data structure will fit (with bounds
 * on max size), only then will we allocate memory on-chip.
 * Else, we will mark a memory as 'off-chip'. Note that we should
 * not see a "DeliteArrayApply" directly on off-chip memories; this
 * is currently not supported. The only operations permitted on
 * off-chip memories are BlockLoad and BlockStore
 */
trait MemoryAnalysis extends FatBlockTraversal with GenericHelper {
  val IR: PPLOpsExp
  import IR._

  val validMemoryMap = Map[Exp[Any], MemInfo]()
  var aliasMap = Map[Exp[Any], Exp[Any]]()

  def run[A](b: Block[Any], amap: Map[Exp[Any], Exp[Any]]) = {
    Console.println(s"[MemoryAnalysis - Begin]")
    aliasMap = amap
    traverseBlock(b)
    Console.println(s"[MemoryAnalysis - End]")
  }

  // Helper function to lookup the alias for a symbol
  private def asl(e: Exp[Any]): Sym[Any] = {
    aliasMap.getOrElse(e,e).asInstanceOf[Sym[Any]]
  }

  private def handleCollectElem(asym: Sym[Any], elem: DeliteCollectElem[_,_,_]) = {
    traverseBlock(elem.buf.alloc)
    traverseBlock(elem.func)
    traverseBlock(elem.buf.update)
  }

  private def handleReduceElem(asym: Sym[Any], elem: DeliteReduceElem[_]) = {
    traverseBlock(elem.zero)
    traverseBlock(elem.func)
    traverseBlock(elem.rFunc)
  }

  private def handleTileElem(asym: Sym[Any], elem: DeliteTileElem[_,_,_]) = {
//    Console.println(s"Handling tileElem allocBuff: $asym")
    traverseBlock(elem.buf.allocBuff)
//    Console.println(s"End $asym allocBuff")

    traverseBlock(elem.tile)
    for (k <- 0 until elem.keys.length) {
      traverseBlock(elem.keys(k))
    }
    if (!elem.rFunc.isEmpty) {
      traverseBlock(elem.buf.allocTile)  // Accumuator copy-in tile
      traverseBlock(elem.rFunc.get)
    }
  }

  private def handleLoopBody(asym: Sym[Any], rhs: Def[Any]): Unit = {
    rhs match {
      case elem: DeliteCollectElem[_,_,_] =>
        handleCollectElem(asym, elem)
      case elem: DeliteReduceElem[_] =>
        handleReduceElem(asym, elem)
      case elem: DeliteTileElem[_,_,_] =>
        handleTileElem(asym, elem)
      case _ =>
        throw new Exception(s"Unknown loop body $rhs")
    }
  }

  private def handleTP(asym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case op: AbstractLoop[_] =>
        handleLoopBody(asym, op.body)

      case op: AbstractLoopNest[_] =>
        handleLoopBody(asym, op.body)

      case op: BlockSlice[_,_,_] =>
        traverseBlock(op.allocTile)

      case DeliteArrayNew(n, m, t) =>
        val an = if (isSym(n)) asl(n) else n
        var mi = new MemInfo()
        mi.memSym = asym
        mi.typeName = m.toString
//        Console.println(s"In $asym, DeliteArrayNew($an)")
//        Console.println(s"Trying to find const for $an")
        val constn = findConst(an, aliasMap)
//        Console.println(s"Const found = $constn")
        if (constn.x == -1) { // Constant not found, this must be off-chip memory
          // if size is known to be small, allocate max possible size here
          // else mark as off-chip. Most of the fields don't make sense
          // for off-chip memory
          mi.location = Location.OFF_CHIP
        } else {
          mi.depth = constn.x
        }
//        Console.println(s"Adding the following entry to validMemoryMap($asym): $mi")
        // Assumption made here: Sym name for array == sym of loop producing the buffer
        // Hence, every buffer has one default writer: The loop producing it
        // This is true for both on and off-chip memories
        mi.writers += asym
        validMemoryMap += asym -> mi

      case DeliteArrayApply(arr, idx) =>
        val aarr = asl(arr)
        val m = validMemoryMap(aarr)
        m.readers += asym

      case NestedAtomicWrite(struct, tracer, atomicWrite) =>
        val astruct = asl(struct)
        atomicWrite match {
          case DeliteArrayUpdate(arr, _,_) =>
            traverseStm(TP(asym, atomicWrite))
        }

      case DeliteArrayUpdate(arr, idx, v) =>
        val aarr = asl(arr)
        val aidx = asl(idx)
        val av = asl(v)
        val m = validMemoryMap(aarr)
        Console.println(s"Found writer $asym for aarr=$aarr (d = ${getdef(aarr)})")
        if (m.location == Location.OFF_CHIP) {
          Console.println("We do not support loads/stores to off-chip memory directly!")
        }
        m.writers += asym

      case _ =>
    }
  }

//  private def handleIgnoreNodes(asym: Sym[Any], rhs: Def[Any]) = {
//    val isIgnorable = rhs match {
//      case DeliteFileInputStreamNew(_,_,_,_) => true
//      case DeliteFileInputStreamReadLine(_,_) => true
//      case _: SimpleRead[_] => true
//      case _: DeliteIfThenElse[_] => true
//      case _: MatrixMkString[_] => true
//      case PrintLn(_) => true
//      case _ => false
//    }
//    if (isIgnorable) ignoreSet += asym
//  }

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(sym, Reflect(d,_,_)) => traverseStm(TP(aliasMap.getOrElse(sym,sym).asInstanceOf[Sym[Any]], d.asInstanceOf[Def[Any]]))
      case TP(sym, rhs) =>
        if (sym.isInstanceOf[Sym[Any]]) {
          val s = aliasMap.getOrElse(sym, sym)
          if (isSym(s)) {
            handleTP(asSym(s), rhs)
          }
        }
      case TTP(_,_,_) =>
        throw new Exception("TTPs not handled yet!\n")
      case _ =>
    }
  }
}
