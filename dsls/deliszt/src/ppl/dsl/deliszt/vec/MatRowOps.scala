package ppl.dsl.deliszt.vec

import java.io.PrintWriter
import ppl.delite.framework.DSLType
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{BaseExp, Base, ScalaGenBase, CudaGenBase, CGenBase}
import scala.virtualization.lms.internal.GenericCodegen
import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}

// TODO: discuss: do we want to allow "index"? we need it for downsampling as its currently written,
// but this provides more expressivity with which the user can shoot themselves in the foot, which
// we are trying to avoid.

// perhaps we allow this only for Streams (where you can't use the index to do anything bad)
// however, as long as we don't provide access to the underlying mat, it is still relatively constrained...
trait MatRowOps extends DSLType with Base with OverloadHack { this: DeLiszt =>
  def infix_index[C<:IntM,A:Manifest](x: Rep[MatRow[C,A]]) = matrow_index(x)

  // class defs
  def matrow_index[C<:IntM,A:Manifest](x: Rep[MatRow[C,A]]): Rep[Int]
}

trait MatRowOpsExp extends MatRowOps with BaseExp { this: DeLisztExp =>
  // implemented via method on real data structure
  case class MatRowIndex[C<:IntM,A:Manifest](x: Exp[MatRow[C,A]]) extends Def[Int]

  def matrow_index[C<:IntM,A:Manifest](x: Exp[MatRow[C,A]]) = reflectPure(MatRowIndex(x))
}

trait MatRowOpsExpOpt extends MatRowOpsExp { this: DeLisztExp =>
}


trait BaseGenMatRowOps extends GenericCodegen {
  val IR: MatRowOpsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    //case MatGetRow(x, i) => Nil  // this is unsafe unless we can remove all actual allocations of views
    case _ => super.syms(e)
  }
}

trait ScalaGenMatRowOps extends BaseGenMatRowOps with ScalaGenBase {
  val IR: MatRowOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case MatRowIndex(x)   => emitValDef(sym, quote(x) + ".index")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenMatRowOps extends CudaGenBase with BaseGenMatRowOps

trait CGenMatRowOps extends CGenBase with BaseGenMatRowOps