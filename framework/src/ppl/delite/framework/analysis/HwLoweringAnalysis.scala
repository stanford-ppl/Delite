package ppl.delite.framework.analysis

import java.io._
import scala.collection.mutable.HashMap
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{AbstractSubstTransformer,Transforming,FatBlockTraversal}
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.HwOpsExp
//import ppl.delite.framework.datastructures.{DeliteArray,DeliteArrayOpsExp}
import ppl.delite.framework.Config

trait HwLoweringAnalysis extends FatBlockTraversal {
  val IR: HwOpsExp
  import IR._

  val loopOutMem = Map[Sym[Any], Sym[Any]]()

  val funcBlocks = Set[Block[Any]]()
  val rfuncBlocks = Set[Block[Any]]()
  val zeroBlocks = Set[Block[Any]]()
  val accInitBlocks = Set[Block[Any]]()
  val allocBlocks = Set[Block[Any]]()
  val applyBlocks = Set[Block[Any]]()
  val updateBlocks = Set[Block[Any]]()
  val appendableBlocks = Set[Block[Any]]()
  val appendBlocks = Set[Block[Any]]()
  val setSizeBlocks = Set[Block[Any]]()
  val allocRawBlocks = Set[Block[Any]]()
  val copyRawBlocks = Set[Block[Any]]()
  val finalizerBlocks = Set[Block[Any]]()
  val hwBlocks = Set[Block[Any]]()

  def run[A](b: Block[A]) = {
    println("Running Hw lowering analysis")
    traverseBlock(b)
    println("Hw lowering analysis complete")
  }

  def processBodyElem(s: Sym[Any], body: Def[_]) = {
    body match {
      case DeliteCollectElem(func,cond,par,buf,iFunc,iF,sF,eF,numDynamicChunks) =>
      case DeliteReduceElem(func,cond,zero,accInit,rV,rFunc,stripFirst,numDynamicChunks) =>
      case DeliteForeachElem(func, _) =>
      case _ =>
    }
  }

  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(s, Reflect(node,_,_)) =>
      traverseStm(TP(s, node.asInstanceOf[Def[Any]]))
    case TP(s,l:AbstractLoop[_]) =>
      val bodyBlock = Block(processBodyElem(s, l.body))
    case TTP(lhs,mhs,rhs@SimpleFatLoop(sz,v,body)) =>
      throw new Exception("TTP not handled yet")
    case _ =>
      super.traverseStm(stm)
  }
}
