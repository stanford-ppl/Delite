package ppl.dsl.optiml.matrix

import ppl.dsl.optiml.CudaGenDataStruct
import java.io.{PrintWriter}

import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import scala.virtualization.lms.common.{CudaGenBase, ScalaGenBase, CGenBase, OpenCLGenBase}
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.internal.{GenerationFailedException}
import scala.reflect.SourceContext
import ppl.delite.framework.Config
import ppl.dsl.optiml._
import ppl.delite.framework.extern.lib._

trait MatrixOps extends ppl.dsl.optila.matrix.MatrixOps  {
  this: OptiML =>

}

trait MatrixOpsExp extends ppl.dsl.optila.matrix.MatrixOpsExp with MatrixOps with VariablesExp {
  this: OptiMLExp  =>
  
}

/**
 *  Optimizations for composite MatrixOps operations.
 */

trait MatrixOpsExpOpt extends ppl.dsl.optila.matrix.MatrixOpsExpOpt with MatrixOpsExp {
  this: OptiMLExp =>

  // override def matrix_numrows[A:Manifest](x: Exp[Matrix[A]])(implicit ctx: SourceContext) = x match {
  //   //case Def(TrainingSetObjectFromMat(x,y)) => matrix_numrows(x) // TODO: move to TrainingSetOpsExpOpt ?
  //   case _ => super.matrix_numrows(x)
  // }
  // 
  // override def matrix_numcols[A:Manifest](x: Exp[Matrix[A]])(implicit ctx: SourceContext) = x match {
  //   //case Def(TrainingSetObjectFromMat(x,y)) => matrix_numcols(x) // TODO: move to TrainingSetOpsExpOpt ?
  //   case _ => super.matrix_numcols(x)
  // }

}


trait ScalaGenMatrixOps extends ScalaGenBase {
  val IR: MatrixOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {  
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CudaGenMatrixOps extends CudaGenBase with CudaGenDataStruct {
  val IR: MatrixOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenMatrixOps extends CGenBase {
  val IR: MatrixOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}
