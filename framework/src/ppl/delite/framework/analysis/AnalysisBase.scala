package ppl.delite.framework.analysis

import java.io._
import scala.collection.mutable.HashMap
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.FatBlockTraversal
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollection}
import ppl.delite.framework.datastructures.{DeliteMultiArray,DeliteArrayOpsExp}
import ppl.delite.framework.Config

import ppl.delite.framework.analysis.Slot // change later..



trait AnalysisBase extends FatBlockTraversal with DeliteMetadata {
  val IR: DeliteOpsExp
  import IR._

  val name: String
  val MAX_ITERS: Int = 10
  final var iter = 0
  final var changed: Boolean = false
  final var hadErrors: Boolean = false

  def notifyUpdate() { changed = true }

  var metadata = new HashMap[Exp[Any],SymbolProperties]()

  final def strDef(e: Exp[Any]) = e match {
    case Const(z) => z.toString
    case _ => 
      val z = findDefinition(e.asInstanceOf[Sym[Any]])
      if (!z.isDefined) "(bound " + e.toString + ")" else e.toString + " = " + z.get.toString
  }

  final def isMutable(e: Exp[Any]) = e match {
    case s: Sym[_] => isWritableSym(s)
    case _ => false
  }

  final def verbose = Config.debug
  final def log(x: String) = /* if (verbose) */ Predef.println("[" + name + "] " + x) /* else () */
  final def result(x: String) = Predef.println("[" + name + "] " + x)

  def warn(x: =>Any) { System.err.println("[\u001B[33mwarn\u001B[0m] " + name + ": " + x) }
  def error(x: =>Any) { System.err.println("[\u001B[31merror\u001B[0m] " + name + ": " + x); hadErrors = true }
  def fatalerr(x: =>Any) { System.err.println("[\u001B[31merror\u001B[0m] " + name + ": " + x); System.exit(0) }

  /**
   * Metadata completeness / analysis progress
   * -----------------------------------------
   * Complete: All associated metadata has been filled in [for this symbol]
   * Incomplete: The given [symbol's mapped] metadata is available but not filled in
   * Unknown: No mapping from symbol exists, or metadata is entirely empty
  */
  def isComplete(m: Metadata): Boolean = m match {
    case StructMetadata(children) => 
      children.values().map {
        case Known(c) => isComplete(c)
        case Nix => true
        case Unknown = false
      }.reduce{(a,b) => a && b}
  }
  def isIncomplete(m: Metadata): Boolean = m match {
    case StructMetadata(children) => 
      children.values().map {
        case Known(c) => isIncomplete(c)
        case Nix => false
        case Unknown => true
      }.reduce{(a,b) => a || b}
  }
  def isUnknown[A](m: Metadata): Boolean = m match {
    case _ => false
  }

  def isComplete(e: Exp[Any]) = metadata.get(e) match {
    case Some(data) => isComplete(data)
    case _ => false
  }
  def isIncomplete(e: Exp[Any]) = metadata.get(e) match {
    case Some(data) => isIncomplete(data)
    case _ => false
  }
  def isUnknown[A](e: Exp[A]) = !metadata.contains(e) || isUnknown(metadata(e))

  /**
   * Add metadata mapping for this symbol if it doesn't already exist
   * Use notifyUpdate() to note if a symbol-metadata mapping has changed
  */
  def add(e: Exp[Any], m: Metadata) {
    if (!metadata.contains(e)) {
      metadata += e -> m
      notifyUpdate()
    }
    else {
      val prev = metadata(e)
      if (compatible(prev,m))
        metadata(e) = meet(prev,m)
      else
        fatalerr("Attempted to merge incompatible metadata in update for symbol:\n" + strDef(e) + "\nat: " + quotePos(e))

      if (!equivalent(prev,m)) notifyUpdate()
    }
  }

  def processOp[A](e: Exp[A], d: Def[_]): Unit

  def run[A](b: Block[A]) {
    result("Beginning...")

    do {
      iter += 1
      changed = false
      log("Starting iteration " + iter)
      traverseBlock(b)
    } while (changed && iter <= MAX_ITERS)

    result("Completed.")
  }    

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(s, Reflect(d,_,_)) => processOp(s,d)
      case TP(s, d) => processOp(s,d)
      case _ => ()
    }
    super.traverseStm(stm)
  }
}