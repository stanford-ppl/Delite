package ppl.delite.framework.analysis

import scala.virtualization.lms.internal.Traversal
import scala.collection.mutable.HashMap

trait QuickTraversal extends Traversal {
  import IR._

  // --- Dependencies
  val useSymsCache = true
  val symsCache = HashMap[Any, List[Sym[Any]]]()

  def symDeps(e: Any): List[Sym[Any]] = {
    if (useSymsCache && symsCache.contains(e)) symsCache(e)
    else {
      val s = syms(e)
      if (useSymsCache) symsCache(e) = s
      s
    }
  }


  // --- Quick resume to a given block
  val allowSaving = true
  var savedScope: Option[List[Stm]] = None
  var savedBlock: Option[Block[Any]] = None

  def load() { innerScope = savedScope.get }
  def resume(){ traverseBlock(savedBlock.get) }

  def save(b: Block[Any]) {
    if (allowSaving) savedBlock = Some(b)
    if (allowSaving) savedScope = Some(innerScope)
  }

  override def run[A:Manifest](b: Block[A]): Block[A] = {
    if (savedBlock.isDefined) {
      preprocess(savedBlock.get)
      load()
      resume()
      postprocess(savedBlock.get)
    }
    else super.run(b)
  }


  // --- Scheduling
  val useScopeCache = true
  // Save inner and local scopes to avoid repeated recomputation
  val scopeCache = HashMap[Block[Any],(List[Stm], List[Stm])]()

  override def traverseBlock[A](block: Block[A]): Unit = {
    if (useScopeCache && scopeCache.contains(block)) {
      val scope = scopeCache(block)
      withInnerScope(scope._1) { traverseStmsInBlock(scope._2) }
    }
    else super.traverseBlock(block)
  }

  override def traverseBlockFocused[A](block: Block[A]): Unit = {
    focusExactScope(block) { levelScope =>
      if (useScopeCache) scopeCache(block) = (innerScope, levelScope)
      traverseStmsInBlock(levelScope)
    }
  }

  override def getStmsInBlock[A](block: Block[A]): List[Stm] = {
    if (scopeCache.contains(block)) scopeCache(block)._2
    else super.getStmsInBlock(block)
  }
}