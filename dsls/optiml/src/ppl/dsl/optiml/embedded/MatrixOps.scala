package ppl.dsl.optiml.embedded

/* Description
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Mar 29, 2010
 * modified: Mar 29, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import java.io.{PrintWriter}
import scala.virtualization.lms.internal.ScalaCodegen
import scala.virtualization.lms.common.{EffectExp, Base}

trait Matrix[T]

trait MatrixOps extends Base {
  //type Rep[+X]
  //type Rep[+X <: Matrix[_]]


  object Matrix {
    def apply[A](numRows: Rep[Int], numCols: Rep[Int]) : Rep[Matrix[A]] = matrix_obj_apply(numRows,numCols)
    def zeros(numRows: Rep[Int], numCols: Rep[Int]) : Rep[Matrix[Double]] = matrix_obj_zeros(numRows, numCols)
  }

  //implicit def munit[A](x: Matrix[A]): Rep[Natrix[A]]

  class matRepCls[A](x: Rep[Matrix[A]]) {
    def apply(n: Rep[Int]) = matrix_apply(x,n)
    def +(y: Rep[Matrix[A]]) = matrix_plus(x,y)
    def *(y: Rep[Matrix[A]]) = matrix_times(x,y)
    def inv = matrix_inverse(x)
    def trans = matrix_transpose(x)
    def numCols = matrix_numcols(x)
    def pprint = matrix_pprint(x)
    def +=(y: Rep[Vector[A]]) = matrix_plusequals(x,y)
  }
  implicit def matRepArith[A](x: Rep[Matrix[A]]) = new matRepCls(x)

  // object defs
  def matrix_obj_zeros(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_apply[A](numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[A]]

  // class defs
  def matrix_apply[A](x: Rep[Matrix[A]], n: Rep[Int]): Rep[Vector[A]]
  def matrix_plus[A](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_times[A](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_inverse[A](x: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_transpose[A](x: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_numcols[A](x: Rep[Matrix[A]]): Rep[Int]
  def matrix_pprint[A](x: Rep[Matrix[A]]): Rep[Unit]
  def matrix_plusequals[A](x: Rep[Matrix[A]], y: Rep[Vector[A]]): Rep[Matrix[A]]
}

/* no longer in common embedding
trait MatrixOpsRepString extends MatrixOps with RepString {
  //type Rep[+X <: Matrix[_]] = String
  //implicit def munit[A](x: T) = x.toString

  def matrix_obj_zeros(numRows: String, numCols: String) = "Matrix.zeros(" + numRows + "," + numCols + ")"
  def matrix_obj_apply[A](numRows: String, numCols: String) = "Matrix(" + numRows + "," + numCols + ")"

  def matrix_apply[A](x: String, n: String) = x + "(" + n + ")"
  def matrix_plus[A](x: String, y: String) = x + " + " + y
  def matrix_times[A](x: String, y: String) = x + " * " + y
  def matrix_inverse[A](x: String) = "inv(" + x + ")"
  def matrix_transpose[A](x: String) = "trans(" + x + ")"
  def matrix_numcols[A](x: String) = "numCols(" + x + ")"
  def matrix_pprint[A](x: String) = "pprint(" + x + ")"
  def matrix_plusequals[A](x: String, y: String) = x + "+=" + y
}
*/

trait MatrixOpsRepExp extends MatrixOps with EffectExp {
  //type Rep[+X] = Exp[X]
  //implicit def munit[A](x: T) = Const(x)

  case class MatrixObjectZeros(numRows: Exp[Int], numCols: Exp[Int]) extends Def[Matrix[Double]]
  case class MatrixObjectApply[A](numRows: Exp[Int], numCols: Exp[Int]) extends Def[Matrix[A]]

  case class MatrixApply[A](x: Exp[Matrix[A]], n: Exp[Int]) extends Def[Vector[A]]
  case class MatrixPlus[A](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) extends Def[Matrix[A]]
  case class MatrixTimes[A](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) extends Def[Matrix[A]]
  case class MatrixInverse[A](x: Exp[Matrix[A]]) extends Def[Matrix[A]]
  case class MatrixTranspose[A](x: Exp[Matrix[A]]) extends Def[Matrix[A]]
  case class MatrixNumCols[A](x: Exp[Matrix[A]]) extends Def[Int]
  case class MatrixPPrint[A](x: Exp[Matrix[A]]) extends Def[Unit]
  case class MatrixPlusEquals[A](x: Exp[Matrix[A]], y: Exp[Vector[A]]) extends Def[Matrix[A]]

  // if x is an m x n MatrixOps, Identity(x) is an n x n square MatrixOps with ones on the diagonal and zeroes elsewhere
  case class MatrixIdentity[A](x: Exp[Matrix[A]]) extends Def[Matrix[A]]

  // operations on composites
  def matrix_obj_zeros(numRows: Exp[Int], numCols: Exp[Int]) = MatrixObjectZeros(numRows, numCols)
  def matrix_obj_apply[A](numRows: Exp[Int], numCols: Exp[Int]) = MatrixObjectApply[A](numRows, numCols)

  def matrix_apply[A](x: Rep[Matrix[A]], n: Rep[Int]) = MatrixApply[A](x,n)
  def matrix_plus[A](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = MatrixPlus(x, y)
  def matrix_times[A](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = MatrixTimes(x, y)
  def matrix_inverse[A](x: Exp[Matrix[A]]) = MatrixInverse(x)
  def matrix_transpose[A](x: Exp[Matrix[A]]) = MatrixTranspose(x)
  def matrix_numcols[A](x: Exp[Matrix[A]]) = MatrixNumCols(x)
  def matrix_pprint[A](x: Exp[Matrix[A]]) = reflectEffect(MatrixPPrint(x))
  def matrix_plusequals[A](x: Exp[Matrix[A]], y: Exp[Vector[A]]) = MatrixPlusEquals(x,y)
}

/**
 * Optimizations for composite MatrixOps operations.
 */

trait MatrixOpsRepExpOpt extends MatrixOpsRepExp {
  override def matrix_plus[A](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = (x, y) match {
    // (AB + AD) == A(B + D)
    case (Def(MatrixTimes(a, b)), Def(MatrixTimes(c, d))) if (a == c) => MatrixTimes[A](a.asInstanceOf[Exp[Matrix[A]]], MatrixPlus[A](b.asInstanceOf[Exp[Matrix[A]]],d.asInstanceOf[Exp[Matrix[A]]]))
    // ...
    case _ => super.matrix_plus(x, y)
  }

  override def matrix_times[A](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = (x, y) match {
    // X^-1*X = X*X^-1 = I (if X is non-singular)
    case (Def(MatrixInverse(a)), b) if (a == b) => MatrixIdentity[A](a.asInstanceOf[Exp[Matrix[A]]])
    case (b, Def(MatrixInverse(a))) if (a == b) => MatrixIdentity[A](a.asInstanceOf[Exp[Matrix[A]]])

    // X*I = I*X = X
    case (Def(MatrixIdentity(a)), b) if (a == b) => a.asInstanceOf[Exp[Matrix[A]]]
    case (a, Def(MatrixIdentity(b))) if (a == b) => a.asInstanceOf[Exp[Matrix[A]]]

    // else
    case _ => super.matrix_times(x, y)
  }

  override def matrix_inverse[A](x: Exp[Matrix[A]]) = x match {
    // (X^-1)^-1 = X (if X is non-singular)
    case (Def(MatrixInverse(a))) => a.asInstanceOf[Exp[Matrix[A]]]
    case _ => super.matrix_inverse(x)
  }

  override def matrix_transpose[A](x: Exp[Matrix[A]]) = x match {
    // (X^T)^T = X
    case (Def(MatrixTranspose(a))) => a.asInstanceOf[Exp[Matrix[A]]]
    case _ => super.matrix_transpose(x)
  }


}

/* Code Gen for Matrix DSL */

trait ScalaCodegenMatrix extends ScalaCodegen { this: MatrixOpsRepExp =>
  private val base = "ppl.dsl.optiml.direct.Matrix"

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case MatrixObjectZeros(rows,cols) => emitValDef(sym, base + ".zeros(" + quote(rows) + ","+ quote(cols) +")")

    case MatrixApply(x,n)  => emitValDef(sym, "" + quote(x) + "(" + quote(n) + ")")
    case MatrixNumCols(x)  => emitValDef(sym, "" + quote(x) + ".numCols")

    case MatrixPlus(x, y)  => emitValDef(sym, "" + quote(x) + " + " + quote(y))

    case MatrixPPrint(x) => emitValDef(sym, quote(x) + ".pprint")

    case _ => super.emitNode(sym, rhs)
  }

}
