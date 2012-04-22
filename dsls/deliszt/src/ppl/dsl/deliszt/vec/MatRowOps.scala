package ppl.dsl.deliszt.vec

import java.io.PrintWriter
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import ppl.dsl.deliszt.{DeLiszt, DeLisztExp}
import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

// TODO: discuss: do we want to allow "index"? we need it for downsampling as its currently written,
// but this provides more expressivity with which the user can shoot themselves in the foot, which
// we are trying to avoid.

// perhaps we allow this only for Streams (where you can't use the index to do anything bad)
// however, as long as we don't provide access to the underlying mat, it is still relatively constrained...
trait MatRowOps extends Variables { this: DeLiszt =>
  def infix_index[C<:IntM,A:Manifest](x: Rep[MatRow[C,A]]) = matrow_index(x)

  // class defs
  def matrow_index[C<:IntM,A:Manifest](x: Rep[MatRow[C,A]]): Rep[Int]
}

trait MatRowOpsExp extends MatRowOps with VariablesExp { this: DeLisztExp =>
  // implemented via method on real data structure
  case class MatRowIndex[C<:IntM,A:Manifest](x: Exp[MatRow[C,A]]) extends Def[Int]

  def matrow_index[C<:IntM,A:Manifest](x: Exp[MatRow[C,A]]) = reflectPure(MatRowIndex(x))
}

trait MatRowOpsExpOpt extends MatRowOpsExp { this: DeLisztExp =>
}

trait ScalaGenMatRowOps extends ScalaGenBase {
  val IR: MatRowOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case MatRowIndex(x)   => emitValDef(sym, quote(x) + ".index")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenMatRowOps extends CudaGenBase

trait CGenMatRowOps extends CGenBase