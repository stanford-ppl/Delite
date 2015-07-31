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
  val ignoreMap = Set[Exp[Any]]()

  def run[A](body: Block[Any]) = {
    Console.println(s"[SymAliasAnalysis - Begin]")
    Console.println(s"[SymAliasAnalysis - End]")
  }

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(sym, Reflect(d,_,_)) => traverseStm(TP(sym, d.asInstanceOf[Def[Any]]))

      case _ =>
    }
  }
}
