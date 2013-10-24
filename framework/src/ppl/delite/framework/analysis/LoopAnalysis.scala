package ppl.delite.framework.analysis

import java.io._
import scala.collection.mutable.{HashMap,ArrayBuffer}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{AbstractSubstTransformer,Transforming,FatBlockTraversal}
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollection}
import ppl.delite.framework.datastructures.{DeliteArray,DeliteArrayOpsExp,DeliteArrayFatExp,BaseGenDeliteArrayOps}
import ppl.delite.framework.Config
import ppl.delite.framework.transform.LoopSoAOpt
import ppl.delite.framework.ops.BaseDeliteOpsTraversalFat

trait LoopAnalysis extends FatBlockTraversal with LoopFusionOpt with LoopSoAOpt with BaseGenDeliteArrayOps with BaseDeliteOpsTraversalFat {
  val IR: DeliteOpsExp 
  import IR._
  
  def verbose = true //Config.debug  
  def log(x: String) = if (verbose) Predef.println(x) else ()

  def analyzeBlock(bodies: List[Def[Any]]) { 
    bodies foreach { body =>
      val bodyType = body match {
        case elem: DeliteHashCollectElem[_,_,_,_,_,_] => "hashcollect"
        case elem: DeliteHashReduceElem[_,_,_,_] => "hashreduce"
        case elem: DeliteHashIndexElem[_,_] => "hashindex"
        case elem: DeliteCollectElem[_,_,_] => "collect"
        case elem: DeliteForeachElem[_] => "foreach"
        case elem: DeliteReduceElem[_] => "reduce"
        case elem: DeliteReduceTupleElem[_,_] => "reducetuple"
      }
      currentLoop.body.append(bodyType)
    }
  } 

  // TODO: add more loop info
  case class LoopInfo(level: Int, body: ArrayBuffer[String], next: ArrayBuffer[LoopInfo])

  var currentLevel: Int = 0
  var currentLoop: LoopInfo = LoopInfo(currentLevel, new ArrayBuffer[String](), new ArrayBuffer[LoopInfo])
  val loopInfoBuffer = new ArrayBuffer[LoopInfo]()
  loopInfoBuffer.append(currentLoop)

  def analyzeInnerBlocks(blocks: List[Block[Any]]): Unit = { 
    val saveLevel = currentLevel
    val saveLoop = currentLoop
    currentLevel = currentLevel + 1
    currentLoop = LoopInfo(currentLevel, new ArrayBuffer[String](), new ArrayBuffer[LoopInfo]())

    val rval = traverseFatBlock(blocks)

    if(currentLoop.body.length > 0) 
      saveLoop.next.append(currentLoop)
    currentLevel = saveLevel
    currentLoop = saveLoop
    rval
  }

  def analyze(sym: List[Sym[Any]], rhs: Any): Unit = {
    rhs match {
      case loop@SimpleFatLoop(sz, v, body) =>
        analyzeBlock(body)
        analyzeInnerBlocks(collectLoopBodyFat(body))
      case loop@SimpleLoop(sz, v, body) =>
        analyzeBlock(List(body))
        analyzeInnerBlocks(collectLoopBody(body))
      case Reflect(loop:AbstractLoop[_],u,es) =>
        analyzeBlock(List(loop.body))
        analyzeInnerBlocks(collectLoopBody(loop.body))
      case loop:AbstractLoop[_] =>
        analyzeBlock(List(loop.body))
        analyzeInnerBlocks(collectLoopBody(loop.body))
      case _ => //
    }
  }

  override def traverseStm(stm: Stm): Unit =  { 
    analyze(stm.lhs, stm.rhs)
  }

  def traverseFatBlock(blocks: List[Block[Any]]): Unit = {
    //TODO: which one?
    traverseBlock(Block(Combine(blocks.map(getBlockResultFull)))) 
    /*
    focusFatBlock(blocks) {
      focusExactScopeFat(block) { levelScope =>
        traverseStmsInBlock(levelScope)
      }
    }
    */
  }
 
  def collectLoopBody(body: Def[Any]): List[Block[Any]] = {
    body match {
      case elem: DeliteHashCollectElem[_,_,_,_,_,_] => elem.keyFunc :: elem.valFunc :: elem.cond
      case elem: DeliteHashReduceElem[_,_,_,_] => elem.keyFunc :: elem.valFunc :: elem.cond
      case elem: DeliteHashIndexElem[_,_] => elem.keyFunc :: elem.cond
      case elem: DeliteCollectElem[_,_,_] => elem.func :: elem.cond
      case elem: DeliteForeachElem[_] => List(elem.func)
      case elem: DeliteReduceElem[_] => elem.func :: elem.cond
      case elem: DeliteReduceTupleElem[_,_] => elem.func._1 :: elem.func._2 :: elem.cond
    }
  }

  def collectLoopBodyFat(bodies: List[Def[Any]]): List[Block[Any]] = {
    val elemFuncs = bodies flatMap { collectLoopBody }
    elemFuncs.distinct
  }

  
  def emitValDef(sym: Sym[Any], rhs: String) {}

  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
      traverseBlock(body)
      Nil
  }
}
