package asplos
import asplos._

import ppl.delite.framework.codegen.hw.HwCodegen
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import ppl.delite.framework.analysis.PrintAnalysis

trait GenericHelper {
  val IR: PPLOpsExp
  import IR._

  protected def getdef(sym: Sym[Any]) = {
    sym match {
      case Def(d) => d
      case _ => null
    }
  }

  protected def findConst(e: Exp[Any], aliasMap : Map[Exp[Any], Exp[Any]]) : Const[Int] = {
    e match {
      case c: Const[Int] => c
      case s: Sym[Int] =>
        val aliasS = aliasMap.getOrElse(s,s).asInstanceOf[Sym[Any]]
        val d = getdef(aliasS)
        d match {
          case MathMin(v1, v2) =>
            if (v1.isInstanceOf[Const[Int]]) {
              v1.asInstanceOf[Const[Int]]
            } else if (v2.isInstanceOf[Const[Int]]) {
              v2.asInstanceOf[Const[Int]]
            } else if (v1.isInstanceOf[Tunable]) {
              Const(v1.asInstanceOf[Tunable].value.get)  // Assuming that tunables are set by now
            } else if (v2.isInstanceOf[Tunable]) {
              Const(v2.asInstanceOf[Tunable].value.get)  // Assuming that tunables are set by now
            } else {
              sys.error(s"No const found for expression $e")
              Const(-1)
            }
          case DIntTimes(v1, v2) if (v1.isInstanceOf[Const[Int]] && v2.isInstanceOf[Const[Int]]) =>
              val v1Const = v1.asInstanceOf[Const[Int]]
              val v2Const = v2.asInstanceOf[Const[Int]]
              Const(v1Const.x * v2Const.x)
          case DIntTimes(v1, v2) if (v1.isInstanceOf[Tunable] && v2.isInstanceOf[Tunable]) =>
              val v1T = v1.asInstanceOf[Tunable]
              val v2T = v2.asInstanceOf[Tunable]
              Const(v1T.value.get * v2T.value.get)
          case DIntTimes(v1,v2) =>
            val c1:Const[Int] = findConst(v1, aliasMap)
            val c2:Const[Int] = findConst(v2, aliasMap)
            Const(c1.x * c2.x)
          case _ =>
            sys.error(s"No idea how to find consts, $s, $d")
            Const(-1)
        }
      case _ =>
        sys.error(s"$e is neither a sym nor a const. What is it?")
        Const(-1)
    }
  }
}
