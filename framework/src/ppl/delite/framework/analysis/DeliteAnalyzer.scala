package ppl.delite.framework.analysis

import scala.virtualization.lms.common.IterativeAnalyzer
import scala.virtualization.lms.internal.Traversal

import scala.reflect.SourceContext
import scala.collection.mutable.HashSet

import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures._

/**
 * Analysis with support for metadata completeness checking and rules for propagation
 * on DeliteArray atomic write defs
 **/
trait DeliteAnalyzer extends IterativeAnalyzer {
  val IR: DeliteOpsExp
  import IR._

  // --- State
  var incomplete: List[Exp[Any]] = Nil

  def completed(e: Exp[Any]): Boolean = true

  def getIncompleteSyms[A:Manifest](b: Block[A]): List[Exp[Any]] = {

    class CompletenessCheck extends Traversal {
      val IR: DeliteAnalyzer.this.IR.type = DeliteAnalyzer.this.IR
      val incompleteSet = new HashSet[Exp[Any]]()

      override val recurse = Always

      override def traverse(lhs: Sym[Any], rhs: Def[Any]) {
        if (!completed(lhs)) incompleteSet += lhs
      }
      override def traverse(lhs: List[Sym[Any]], mhs: List[Def[Any]], rhs: FatDef) {
        lhs foreach {sym => if (!completed(sym)) incompleteSet += sym }
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

}
