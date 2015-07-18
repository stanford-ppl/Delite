package ppl.delite.framework.analysis

import java.io._
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{AbstractSubstTransformer,Transforming,FatBlockTraversal}
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteOpsExp
//import ppl.delite.framework.datastructures.{DeliteArray,DeliteArrayOpsExp}
import ppl.delite.framework.Config

/*
 * @MetaPipelineAnalysis: Analyze a loop body to collect
 * metapipeline metadata and return the collected metadata. This
 * medatada will be used to generate a metapipeline
 */
trait MetaPipelineAnalysis extends FatBlockTraversal {
  val IR: DeliteOpsExp
  import IR._

  private var bodyMetadata = List[Any]()
  private var curBody: Sym[Any] = null
  private var exclude: Set[Sym[Any]] = null

  def run[A](s : Sym[Any], body: Def[Any], excludeList: Set[Sym[Any]]) = {
    println(s"[MetaPipelineAnalysis - Begin] Loop $s")
    curBody = s
    exclude = excludeList
    processBodyElem(s, body)
    println(s"[MetaPipelineAnalysis - End] Loop $s")
    bodyMetadata
  }

  def processBodyElem(s: Sym[Any], body: Def[_]) = {
    body match {
      case DeliteCollectElem(func,cond,par,buf,iFunc,iF,sF,eF,numDynamicChunks) =>
        traverseBlock(func)
      case DeliteReduceElem(func,cond,zero,accInit,rV,rFunc,stripFirst,numDynamicChunks) =>
        traverseBlock(func)
        traverseBlock(rFunc)
      case DeliteForeachElem(func, _) =>
      case DeliteTileElem (keys, cond, tile, rV, rFunc, buf, numDynamicChunks) =>
      case _ =>
    }
  }


  override def traverseStm(stm: Stm): Unit = {
    println(stm)
    stm match {
    case TP(s, Reflect(node,_,_)) =>
      traverseStm(TP(s, node.asInstanceOf[Def[Any]]))
    case TP(s,l:AbstractLoop[_]) =>
      if (!exclude.contains(s)) {
        bodyMetadata = s :: bodyMetadata
      }
    case TTP(lhs,mhs,rhs@SimpleFatLoop(sz,v,body)) =>
      val seenBefore = lhs.map(x => exclude.contains(x)).reduce(_&_)
      if (!seenBefore) {
        bodyMetadata = lhs :: bodyMetadata
      }
    case _ =>
      super.traverseStm(stm)
    }
  }
}
