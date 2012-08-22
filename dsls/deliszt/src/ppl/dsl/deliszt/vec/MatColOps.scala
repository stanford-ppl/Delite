package ppl.dsl.deliszt.vec

import java.io.PrintWriter
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import ppl.dsl.deliszt.{DeLiszt, DeLisztExp}
import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

trait MatColOps extends Variables { this: DeLiszt =>
  def infix_index[R<:IntM,A](x: Rep[MatCol[R,A]])(implicit mA: Manifest[A], o: Overloaded1) = matcol_index(x)

  // class defs
  def matcol_index[R<:IntM,A:Manifest](x: Rep[MatCol[R,A]]): Rep[Int]
}

trait MatColOpsExp extends MatColOps with VariablesExp { this: DeLisztExp =>
  // implemented via method on real data structure
  case class MatColIndex[R<:IntM,A:Manifest](x: Exp[MatCol[R,A]]) extends Def[Int]

  def matcol_index[R<:IntM,A:Manifest](x: Exp[MatCol[R,A]]) = reflectPure(MatColIndex(x))
}

trait MatColOpsExpOpt extends MatColOpsExp { this: DeLisztExp =>
}

trait ScalaGenMatColOps extends ScalaGenBase {
  val IR: MatColOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case MatColIndex(x)   => emitValDef(sym, quote(x) + ".index")
    case _ => super.emitNode(sym, rhs)
  }
}