package ppl.dsl.deliszt.vec

import java.io.PrintWriter
import ppl.delite.framework.DSLType
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{BaseExp, Base, ScalaGenBase}
import ppl.dsl.deliszt.datastruct.scala.{MatCol, MatColImpl}
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}

trait MatColOps extends DSLType with Base with MetaInteger with OverloadHack { this: DeLiszt =>
  def infix_index[R<:IntM,A](x: Rep[MatCol[R,A]])(implicit mA: Manifest[A], o: Overloaded1) = matcol_index(x)

  // class defs
  def matcol_index[R<:IntM,A:Manifest](x: Rep[MatCol[R,A]]): Rep[Int]
}

trait MatColOpsExp extends MatColOps with BaseExp { this: DeLisztExp =>
  // implemented via method on real data structure
  case class MatColIndex[R<:IntM,A:Manifest](x: Exp[MatCol[R,A]]) extends Def[Int]

  def matcol_index[R<:IntM,A:Manifest](x: Exp[MatCol[R,A]]) = reflectPure(MatColIndex(x))
}

trait MatColOpsExpOpt extends MatColOpsExp { this: DeLisztExp =>
}

trait ScalaGenMatColOps extends ScalaGenBase {
  val IR: MatColOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case MatColIndex(x)   => emitValDef(sym, quote(x) + ".index")
    case _ => super.emitNode(sym, rhs)
  }
}