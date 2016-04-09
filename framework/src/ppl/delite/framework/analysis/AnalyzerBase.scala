package ppl.delite.framework.analysis

import scala.virtualization.lms.internal.Traversal
import scala.virtualization.lms.common.IterativeAnalyzer

import scala.reflect.SourceContext
import scala.collection.mutable.HashSet

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

trait AnalyzerBase extends IterativeAnalyzer {
  val IR: DeliteOpsExp
  import IR._

  // --- State
  var incomplete: List[Exp[Any]] = Nil

  def completed(e: Exp[Any]): Boolean = true

  def getIncompleteSyms[A:Manifest](b: Block[A]): List[Exp[Any]] = {

    class CompletenessCheck extends Traversal {
      val IR: AnalyzerBase.this.IR.type = AnalyzerBase.this.IR
      val incompleteSet = new HashSet[Exp[Any]]()

      override def traverseStm(stm: Stm) = {
        super.traverseStm(stm)
        stm match {
          case TP(sym,_) => if (!completed(sym)) { incompleteSet += sym }
          case TTP(syms,_,_) => syms foreach {sym => if (!completed(sym)) incompleteSet += sym }
        }
      }
    }

    val checker = new CompletenessCheck()
    checker.run(b)
    (checker.incompleteSet.toList)
  }

  override def getAtomicWriteRHS(d: AtomicWrite[Any])(implicit ctx: SourceContext): Option[SymbolProperties] = d match {
    case DeliteArrayUpdate(_,_,x) => Some(ArrayProperties(getProps(x), NoData))
    case DeliteArrayCopy(src,_,_,_,_) => getProps(src)
    case _ => super.getAtomicWriteRHS(d)
  }

  /*def propagateTP[A](lhs: Exp[A], rhs: Def[_])(implicit ctx: SourceContext): Unit = rhs match {
    // --- Delite Ops
    // TODO: Fill in the remainder of these ops
    case op: AbstractLoop[_] => op.body match {
      case r: DeliteReduceElem[_] =>
        traverseBlock(r.func)
        setProps(r.rV._1, getProps(r.func))
        setProps(r.rV._2, getProps(r.func))
        traverseBlock(r.rFunc)
        setProps(lhs, getProps(r.rFunc))
  }*/

}
