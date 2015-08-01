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
 * @SymAliasAnalysis: Given a block, recursively travels
 * through the block and populates a symbol 'alias' map
 * that will later be used in code generation.
 * This is ideally called on the block representing the entire
 * program. This pass also populates the 'ignoreSym' map,
 * for which we will explicitly not generate code and not
 * generate an exception
 */
trait SymAliasAnalysis extends FatBlockTraversal with GenericHelper {
  val IR: PPLOpsExp
  import IR._

  private var curLoop: Sym[Any] = null

  val aliasMap = Map[Exp[Any], Exp[Any]]()
  val ignoreSet = Set[Exp[Any]]()
  var reverseStructMap = Map[Exp[Any],Exp[Any]]()
  var structInfoMap = Map[(Exp[Any],String), Exp[Any]]()

  def run[A](b: Block[Any]) = {
    Console.println(s"[SymAliasAnalysis - Begin]")
    traverseBlock(b)
    Console.println(s"[SymAliasAnalysis - End]")
  }

  // Helper functions to do a transitive insert into aliasMap
  // When inserting (k -> v), if there are already other terms
  // like (k1 -> k, k2 -> k), we can transitively update the
  // aliasMap to reflect ((k -> v), (k1 -> v), (k2 -> v))
  private def getKeysFor(v: Exp[Any]) = {
    val keys = aliasMap.filterKeys(aliasMap(_) == v).map(_._1)
//    if (v.asInstanceOf[Sym[Any]].id == 277) {
//      Console.println(s"Keys for $v : $keys")
//    }
    keys
  }

  private def transitiveInsert(k: Exp[Any], v: Exp[Any]) : Unit= {
    // 1. Update all existing instances (k1 -> k, k2 -> k..) to (k1 -> v, k2 -> v)
    val keys = getKeysFor(k)
//    if (v.asInstanceOf[Sym[Any]].id == 277) {
//      Console.println(s"Keys for $v : $keys")
//    }
    keys.map(aliasMap(_) = v)

    // 2. Does v exist as a key? (v -> foo)
    // If so, add (k -> aliasMap(v))
    if (aliasMap.contains(v)) {
      aliasMap(k) = aliasMap(v)
    } else {
      aliasMap(k) = v
    }

    // 3. Update all instances 


  }

  private def transitiveInsert(m: Map[Exp[Any], Exp[Any]]): Unit = {
    m.foreach { tuple =>
      val key = tuple._1
      val v = tuple._2
      transitiveInsert(key, v)
    }
  }

  private def handleCollectElem(sym: Sym[Any], elem: DeliteCollectElem[_,_,_]) = {
//    aliasMap(getBlockResult(elem.buf.alloc)) = aliasMap.getOrElse(sym,sym)
//    aliasMap(elem.buf.allocVal) = aliasMap.getOrElse(sym,sym)
    //            aliasMap(elem.buf.sV) = getMemorySize(sym)
//    aliasMap(elem.buf.eV) = aliasMap.getOrElse(getBlockResult(elem.func),getBlockResult(elem.func))
    transitiveInsert(getBlockResult(elem.buf.alloc), sym)
    transitiveInsert(elem.buf.allocVal, sym)
    transitiveInsert(elem.buf.eV, getBlockResult(elem.func))

    val array2DAnalysis = new Array2DHackAnalysis{val IR: SymAliasAnalysis.this.IR.type = SymAliasAnalysis.this.IR}
    array2DAnalysis.run(elem.buf.alloc, sym)
//    aliasMap ++= array2DAnalysis.allocAliasMap
    transitiveInsert(array2DAnalysis.allocAliasMap)
    reverseStructMap ++= array2DAnalysis.reverseStructMap
    structInfoMap ++= array2DAnalysis.structInfoMap

    traverseBlock(elem.buf.alloc)
    traverseBlock(elem.func)
    traverseBlock(elem.buf.update)
  }

  def isPrimitiveReduce(elem: DeliteReduceElem[_]) = {
    val m = elem.mA.toString
    m match {
      case "Int" => true
      case "Float" => true
      case "Double" => true
      case _ => false
    }
  }

  private def handleReduceElem(sym: Sym[Any], elem: DeliteReduceElem[_]) = {
    val array2DAnalysis = new Array2DHackAnalysis{val IR: SymAliasAnalysis.this.IR.type = SymAliasAnalysis.this.IR}
    array2DAnalysis.run(elem.func, sym)
//    aliasMap ++= array2DAnalysis.allocAliasMap
    transitiveInsert(array2DAnalysis.allocAliasMap)
    reverseStructMap ++= array2DAnalysis.reverseStructMap
    structInfoMap ++= array2DAnalysis.structInfoMap

    if (!isPrimitiveReduce(elem)) {
//      aliasMap(elem.rV._1) = sym
//      aliasMap(elem.rV._2) = getBlockResult(elem.func)
//      aliasMap(getBlockResult(elem.rFunc)) = sym
//      aliasMap(getBlockResult(elem.zero)) = sym
      transitiveInsert(elem.rV._1, sym)
      transitiveInsert(elem.rV._2, getBlockResult(elem.func))
      transitiveInsert(getBlockResult(elem.rFunc), sym)
      transitiveInsert(getBlockResult(elem.zero), sym)

    }
    traverseBlock(elem.zero)
    traverseBlock(elem.func)
    traverseBlock(elem.rFunc)
  }

  private def handleTileElem(sym: Sym[Any], elem: DeliteTileElem[_,_,_]) = {
//    aliasMap += elem.buf.buffVal -> aliasMap.getOrElse(sym,sym)
//    aliasMap += getBlockResult(elem.buf.allocBuff) -> aliasMap.getOrElse(sym,sym)
    transitiveInsert(elem.buf.buffVal,sym)
    transitiveInsert(getBlockResult(elem.buf.allocBuff),sym)

    val array2DAnalysis = new Array2DHackAnalysis{val IR: SymAliasAnalysis.this.IR.type = SymAliasAnalysis.this.IR}
    array2DAnalysis.run(elem.buf.allocBuff, sym)
//    aliasMap ++= array2DAnalysis.allocAliasMap
    transitiveInsert(array2DAnalysis.allocAliasMap)
    reverseStructMap ++= array2DAnalysis.reverseStructMap
    structInfoMap ++= array2DAnalysis.structInfoMap

    traverseBlock(elem.buf.allocBuff)
    traverseBlock(elem.tile)
    for (k <- 0 until elem.keys.length) {
      traverseBlock(elem.keys(k))
    }
    if (!elem.rFunc.isEmpty) {
      val array2DAnalysis = new Array2DHackAnalysis{val IR: SymAliasAnalysis.this.IR.type = SymAliasAnalysis.this.IR}
      array2DAnalysis.run(elem.buf.allocTile.asInstanceOf[array2DAnalysis.Block[Any]], getBlockResult(elem.buf.allocTile).asInstanceOf[array2DAnalysis.IR.Sym[Any]])
//      aliasMap ++= array2DAnalysis.allocAliasMap
      transitiveInsert(array2DAnalysis.allocAliasMap)
      reverseStructMap ++= array2DAnalysis.reverseStructMap
      structInfoMap ++= array2DAnalysis.structInfoMap
//      aliasMap(elem.rV._1) = getBlockResult(elem.rFunc.get)
//      aliasMap(elem.rV._2) = getBlockResult(elem.tile)
      transitiveInsert(elem.rV._1, getBlockResult(elem.rFunc.get))
      transitiveInsert(elem.rV._2, getBlockResult(elem.tile))
      traverseBlock(elem.buf.allocTile)
      traverseBlock(elem.rFunc.get)
    }
  }

  private def handleLoopBody(sym: Sym[Any], rhs: Def[Any]): Unit = {
    rhs match {
      case elem: DeliteCollectElem[_,_,_] =>
        handleCollectElem(sym, elem)
      case elem: DeliteReduceElem[_] =>
        handleReduceElem(sym, elem)
      case elem: DeliteTileElem[_,_,_] =>
        handleTileElem(sym, elem)
      case _ =>
        throw new Exception(s"Unknown loop body $rhs")
    }
  }

  private def handleTP(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case op: AbstractLoop[_] =>
        Console.println("[SAA] AbstractLoop")
        handleLoopBody(sym, op.body)

      case op: AbstractLoopNest[_] =>
        Console.println("[SAA] AbstractLoopNest")
        handleLoopBody(sym, op.body)

      case op: BlockSlice[_,_,_] =>
        val array2DAnalysis = new Array2DHackAnalysis{val IR: SymAliasAnalysis.this.IR.type = SymAliasAnalysis.this.IR}
        array2DAnalysis.run(op.allocTile.asInstanceOf[array2DAnalysis.Block[Any]], sym)
//        aliasMap ++= array2DAnalysis.allocAliasMap
        transitiveInsert(array2DAnalysis.allocAliasMap)
        reverseStructMap ++= array2DAnalysis.reverseStructMap
        structInfoMap ++= array2DAnalysis.structInfoMap
        traverseBlock(op.allocTile)


      case FieldApply(struct, field) =>
        // Determine if this struct has been seen before
        // All structs seen before will have an alias
        // and an entry with the alias in reverseStructMap
        val aliasSym = aliasMap.getOrElse(struct,struct)
        if (field != "data") {
          if (!reverseStructMap.contains(aliasSym)) {
              val d = getdef(aliasSym.asInstanceOf[Sym[Any]])
              Console.println(s"[SAA] could not find $aliasSym ($struct -> $aliasSym) in struct map!")
              Console.println(s"aliasMap: $aliasMap")
              Console.println(s"[SAA] traversing ($aliasSym, $d)")
              traverseStm(TP(aliasSym.asInstanceOf[Sym[Any]], d))
          }
          val aliasStruct = reverseStructMap(aliasSym)
          val structInfo = structInfoMap((aliasStruct, field))
          Console.println(s"$aliasSym -> aliasStruct = $aliasStruct")
          Console.println(s"structInfoMap($aliasStruct, $field) = $structInfo")

//          aliasMap += sym -> structInfo
          transitiveInsert(sym, structInfo)
        } else {
//          aliasMap += sym -> aliasSym
          transitiveInsert(sym, aliasSym)
        }

      case NestedAtomicWrite(struct, tracer, atomicWrite) =>
        val aliasSym = aliasMap.getOrElse(struct,struct)
        atomicWrite match {
          case DeliteArrayUpdate(arr,_,_) =>
//            aliasMap += arr -> aliasSym
            transitiveInsert(arr, aliasSym)
          case _ =>
        }

      case _ =>
        handleIgnoreNodes(sym, rhs)
    }
  }

  private def handleIgnoreNodes(sym: Sym[Any], rhs: Def[Any]) = {
    val isIgnorable = rhs match {
      case DeliteFileInputStreamNew(_,_,_,_) => true
      case DeliteFileInputStreamReadLine(_,_) => true
      case _: SimpleRead[_] => true
      case _: DeliteIfThenElse[_] => true
      case _: MatrixMkString[_] => true
      case PrintLn(_) => true
      case _ => false
    }
    if (isIgnorable) ignoreSet += sym
  }

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(sym, Reflect(d,_,_)) => traverseStm(TP(sym, d.asInstanceOf[Def[Any]]))
      case TP(sym, rhs) => handleTP(sym, rhs)
      case TTP(_,_,_) =>
        throw new Exception("TTPs not handled yet!\n")
      case _ =>
    }
  }
}
