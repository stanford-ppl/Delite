package ppl.dsl.optiml.vector

import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common.{BaseExp, Base, ScalaGenBase, CudaGenBase, CGenBase, OpenCLGenBase}
import scala.virtualization.lms.internal.GenericCodegen
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optiml.datastruct.scala.{MatrixRow, MatrixRowImpl}
import ppl.dsl.optiml.{OptiMLExp, OptiML}

// TODO: discuss: do we want to allow "index"? we need it for downsampling as its currently written,
// but this provides more expressivity with which the user can shoot themselves in the foot, which
// we are trying to avoid.

// perhaps we allow this only for Streams (where you can't use the index to do anything bad)
// however, as long as we don't provide access to the underlying matrix, it is still relatively constrained...
trait MatrixRowOps extends DSLType with Base with OverloadHack { this: OptiML =>

  def infix_index[A:Manifest](x: Rep[MatrixRow[A]]) = matrixrow_index(x)

  // class defs
  def matrixrow_index[A:Manifest](x: Rep[MatrixRow[A]]): Rep[Int]
}

trait MatrixRowOpsExp extends MatrixRowOps with BaseExp { this: OptiMLExp =>

  // implemented via method on real data structure
  case class MatrixRowIndex[A:Manifest](x: Exp[MatrixRow[A]]) extends Def[Int]

  def matrixrow_index[A:Manifest](x: Exp[MatrixRow[A]]) = reflectPure(MatrixRowIndex(x))
}

trait MatrixRowOpsExpOpt extends MatrixRowOpsExp { this: OptiMLExp =>

  override def matrixrow_index[A:Manifest](x: Exp[MatrixRow[A]]) = x match {
    case Def(MatrixGetRow(m, i)) => i
    case _ => super.matrixrow_index(x)
  }
}


trait BaseGenMatrixRowOps extends GenericCodegen {
  val IR: MatrixRowOpsExp
  import IR._

}

trait ScalaGenMatrixRowOps extends BaseGenMatrixRowOps with ScalaGenBase {
  val IR: MatrixRowOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case MatrixRowIndex(x)   => emitValDef(sym, quote(x) + ".index")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenMatrixRowOps extends CudaGenBase with BaseGenMatrixRowOps
trait OpenCLGenMatrixRowOps extends OpenCLGenBase with BaseGenMatrixRowOps
trait CGenMatrixRowOps extends CGenBase with BaseGenMatrixRowOps