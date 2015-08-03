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
 * @KeysAnalysis: Given a key block, pattern matches
 * on the RangeVectorNew nodes to construct a list of
 * start syms and length exps. Since length has to be
 * a Const, it finds the source of the length node
 * and returns a conservative constant (e.g. if min(foo, 32),
 * returns 32)
 */
trait KeysAnalysis extends FatBlockTraversal with GenericHelper {
  val IR: PPLOpsExp
  import IR._

  private var curLoop: Sym[Any] = null

  val starts = ListBuffer[Exp[Any]]()
  val lengths = ListBuffer[Const[Int]]()
  val aliasMap = Map[Exp[Any], Exp[Any]]()

//  protected def getdef(sym: Sym[Any]) = {
//    sym match {
//      case Def(d) => d
//      case _ => null
//    }
//  }

  def run[A](keyBlks: List[Block[Any]], loopSym: Sym[Any], amap: Map[Exp[Any], Exp[Any]]) = {
    Console.println(s"[KeysAnalysis - Begin]")
    curLoop = loopSym
    aliasMap = amap
    keyBlks.foreach { traverseBlock(_) }
    Console.println(s"[KeysAnalysis - End]")
  }

//  private def findConst(e: Exp[Any]) : Const[Int] = {
//    e match {
//      case c: Const[Int] => c
//      case s: Sym[Int] =>
//        val aliasS = aliasMap.getOrElse(s,s).asInstanceOf[Sym[Any]]
//        val d = getdef(aliasS)
//        d match {
//          case MathMin(v1, v2) =>
//            if (v1.isInstanceOf[Const[Int]]) {
//              v1.asInstanceOf[Const[Int]]
//            } else if (v2.isInstanceOf[Const[Int]]) {
//              v2.asInstanceOf[Const[Int]]
//            } else if (v1.isInstanceOf[Tunable]) {
//              Const(v1.asInstanceOf[Tunable].value.get)  // Assuming that tunables are set by now
//            } else if (v2.isInstanceOf[Tunable]) {
//              Const(v2.asInstanceOf[Tunable].value.get)  // Assuming that tunables are set by now
//            } else {
//              sys.error(s"No const found for expression $e")
//              Const(-1)
//            }
//        }
//      case _ =>
//        sys.error(s"$e is neither a sym nor a const. What is it?")
//        Const(-1)
//    }
//  }

  override def traverseStm(stm: Stm): Unit = {
    stm match {
      case TP(sym, Reflect(d,_,_)) => traverseStm(TP(sym, d.asInstanceOf[Def[Any]]))

      case TP(sym, d@RangeVectorNew(start, stride, length)) =>
        val aliasStart = aliasMap.getOrElse(start, start)
        val aliasStride = aliasMap.getOrElse(stride, stride)
        val aliasLength = aliasMap.getOrElse(length, length)
        val constLength = findConst(aliasLength, aliasMap)

        starts.append(aliasStart)
        lengths.append(constLength)

      case _ =>
    }
  }
}
