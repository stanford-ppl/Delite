//package ppl.delite.framework.analysis
package asplos
import asplos._

import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{AbstractSubstTransformer,Transforming,FatBlockTraversal}
import scala.reflect.SourceContext

import ppl.delite.framework.ops.DeliteFileReaderOpsExp
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
//  val IR: DeliteOpsExp with DeliteFileReaderOpsExp
  val IR: PPLOpsExp
  import IR._

  private var bodyMetadata = List[List[Any]]()
  private var curBody: Sym[Any] = null
  private var exclude: Set[Sym[Any]] = null

  protected def getdef(sym: Sym[Any]) = {
    sym match {
      case Def(d) => d
      case _ => null
    }
  }

  def combineBlockSlices() = {
//    val flattenedStages = bodyMetadata.reverse.flatten.asInstanceOf[List[Sym[Any]]]
//    val blockSliceIndices = flattenedStages.filter(x => getdef(x).isInstanceOf[BlockSlice[_,_,_]]).map( v => flattenedStages.findIndexOf(x => x == v))
//    Console.println(s"Block slice indices: $blockSliceIndices")
  }

  def run[A](s : Sym[Any], body: Def[Any], excludeList: Set[Sym[Any]]) = {
    Console.println(s"[MetaPipelineAnalysis - Begin] Loop $s")
    curBody = s
    exclude = excludeList
    processBodyElem(s, body)
    Console.println(s"[MetaPipelineAnalysis] Stages = $bodyMetadata")
    Console.println(s"[MetaPipelineAnalysis - End] Loop $s")
    if (bodyMetadata.length == 3) {
      sys.exit(0)
    }
    combineBlockSlices()
    bodyMetadata.reverse
  }

  def shouldSkipStm(stm: Stm) : Boolean= {
    stm match {
      case TP(s,d) => d match {
        case Reflect(node, _,_) => shouldSkipStm(TP(s, node))
        case DeliteFileInputStreamNew(_,_,_,_) => true
        case DeliteFileInputStreamReadLine(_,_) => true
        case _ => false
      }
      case _ => false
    }
  }

  def shouldSkipBlock(b: Block[Any]) = {
    val stms = buildScheduleForResult(b)
    Console.println(s"stms: $stms")
    stms.map { shouldSkipStm(_)}.reduce(_ | _)

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
        traverseBlock(tile)
        if (!rFunc.isEmpty) {
          traverseBlock(rFunc.get)
        }
      case _ =>
    }
  }


  override def traverseStm(stm: Stm): Unit = {
//    Console.println(stm)
    stm match {
    case TP(s, Reflect(node,_,_)) =>
      traverseStm(TP(s, node.asInstanceOf[Def[Any]]))
    case TP(s,l:AbstractLoop[_]) =>  // No file readers on the board please
      val shouldSkipLoop = l.body match {
        case DeliteCollectElem(func,cond,par,buf,iFunc,iF,sF,eF,numDynamicChunks) =>
          shouldSkipBlock(func)
        case _ => false
      }
      Console.println(s"Loop $s: shouldSkipLoop = $shouldSkipLoop")
      if (!shouldSkipLoop && !exclude.contains(s)) {
        bodyMetadata = List(s) :: bodyMetadata
      }

    case TP(s,l:BlockSlice[_,_,_]) =>
      if (!exclude.contains(s)) {
        bodyMetadata = List(s) :: bodyMetadata
      }

    case TP(s,l:AbstractLoopNest[_]) =>
      if (!exclude.contains(s)) {
        bodyMetadata = List(s) :: bodyMetadata
      }

    case TTP(lhs,mhs,rhs@SimpleFatLoop(sz,v,body)) =>
      val seenBefore = lhs.map(x => exclude.contains(x)).reduce(_&_)
      if (!seenBefore) {
        bodyMetadata = List(lhs) :: bodyMetadata
      }
    case _ =>
      // Do nothing
      //super.traverseStm(stm)
    }
  }
}
