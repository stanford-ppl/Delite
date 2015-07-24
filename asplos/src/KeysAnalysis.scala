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

/*
 * @KeysAnalysis: Prints a block
 */
trait KeysAnalysis extends FatBlockTraversal {
  val IR: PPLOpsExp
  import IR._

  private var curLoop: Sym[Any] = null
  val allocAliasMap = Map[Exp[Any], Exp[Any]]()
  val reverseStructMap = Map[Exp[Any], Exp[Any]]()
  val structInfoMap = Map[(Exp[Any], String), Exp[Any]]()

  def run[A](allocBlk: Block[Any], loopSym: Sym[Any]) = {
    Console.println(s"[KeysAnalysis - Begin]")
    curLoop = loopSym
    traverseBlock(allocBlk)
    Console.println(s"[KeysAnalysis - End]")
  }

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(sym, Reflect(d,_,_)) => traverseStm(TP(sym, d.asInstanceOf[Def[Any]]))

      case TP(sym, d@Array2DNew(data, dim0,dim1)) =>
        allocAliasMap += data -> curLoop
        reverseStructMap += curLoop -> sym
        structInfoMap += (sym, "data") -> data
        structInfoMap += (sym, "dim0") -> dim0
        structInfoMap += (sym, "dim1") -> dim1

        Console.println(s"Found Array2D loop: $sym")

      case _ =>
    }
  }
}
