package ppl.delite.framework.analysis

import java.io._
import scala.collection.mutable.HashMap
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{AbstractSubstTransformer,Transforming,FatBlockTraversal}
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
//import ppl.delite.framework.datastructures.{DeliteArray,DeliteArrayOpsExp}
import ppl.delite.framework.Config

trait PrimitiveReduceAnalysis extends FatBlockTraversal {
  val IR: DeliteOpsExp
  import IR._

  private val primitiveOpsMap = HashMap[Sym[Any], Def[Any]]()

  def run[A](b: Block[A]) = {
    println("Running Hw lowering analysis")
    traverseBlock(b)
    println("Hw lowering analysis complete")
    primitiveOpsMap
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
      sys.error("Found loop in simple reduce body!")
    case TTP(lhs,mhs,rhs@SimpleFatLoop(sz,v,body)) =>
      sys.error("Found fatloop in simple reduce body")
    case TP(s, l:DIntPlus) => primitiveOpsMap(s) = l
    case TP(s, l:DIntMinus) => primitiveOpsMap(s) = l
    case TP(s, l:DIntTimes) => primitiveOpsMap(s) = l
    case TP(s, l:DIntDivide) => primitiveOpsMap(s) = l
    case TP(s, l:DLessThan[_]) => primitiveOpsMap(s) = l
    case TP(s, l:DGreaterThan[_]) => primitiveOpsMap(s) = l
    case _ =>
      super.traverseStm(stm)
  }
}
