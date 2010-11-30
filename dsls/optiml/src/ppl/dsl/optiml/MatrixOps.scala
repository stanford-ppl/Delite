package ppl.dsl.optiml

import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import scala.virtualization.lms.internal.ScalaGenBase

trait Matrix[T]

trait MatrixOps extends DSLType with Variables {

  object Matrix {
    def apply[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]) : Rep[Matrix[A]] = matrix_new(numRows, numCols)
    def zeros(numRows: Rep[Int], numCols: Rep[Int]) : Rep[Matrix[Double]] = matrix_new(numRows, numCols)
  }

  implicit def matRepArith[A:Manifest](x: Rep[Matrix[A]]) = new matRepCls(x)
  implicit def varToRepMatOps[A:Manifest](x: Var[Matrix[A]]) : matRepCls[A]

  // could convert to infix, but apply doesn't work with it anyways yet
  class matRepCls[A:Manifest](x: Rep[Matrix[A]]) {
    def apply(i: Rep[Int]) = matrix_apply1(x,i)
    def apply(i: Rep[Int], j: Rep[Int]) = matrix_apply2(x,i,j)
    def update(i: Rep[Int], j: Rep[Int], y: Rep[A]) = matrix_update(x,i,j,y)
    def +(y: Rep[Matrix[A]])(implicit n: Numeric[A]) = matrix_plus(x,y)
    def +=(y: Rep[Matrix[A]])(implicit n: Numeric[A]) = matrix_plusequals(x,y)
    def *(y: Rep[Matrix[A]]) = matrix_times(x,y)
    def inv = matrix_inverse(x)
    def trans = matrix_transpose(x)
    def numRows = matrix_numrows(x)
    def numCols = matrix_numcols(x)
    def pprint = matrix_pprint(x)
    def +=(y: Rep[Vector[A]]) = matrix_insertrow(x,x.numRows,y)
    def insertRow(pos: Rep[Int], v: Rep[Vector[A]]) = matrix_insertrow(x,pos,v)
  }

  // class defs
  def matrix_apply1[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int]): Rep[Vector[A]]
  def matrix_apply2[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int], j: Rep[Int]): Rep[A]
  def matrix_update[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int], j: Rep[Int], y: Rep[A]): Rep[Unit]
  def matrix_plus[A:Manifest:Numeric](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_plusequals[A:Manifest:Numeric](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_times[A:Manifest](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_inverse[A:Manifest](x: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_transpose[A:Manifest](x: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_numrows[A:Manifest](x: Rep[Matrix[A]]): Rep[Int]
  def matrix_numcols[A:Manifest](x: Rep[Matrix[A]]): Rep[Int]
  def matrix_pprint[A:Manifest](x: Rep[Matrix[A]]): Rep[Unit]
  def matrix_insertrow[A:Manifest](x: Rep[Matrix[A]], pos: Rep[Int], v: Rep[Vector[A]]) : Rep[Matrix[A]]

  // impl defs
  def matrix_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]) : Rep[Matrix[A]]

  /**
   * Machinery
   */
  
  implicit val doubleMatArithOps = matrixArithOps[Double]
  implicit def matrixArithOps[T:Numeric:Manifest] : ArithOps[Rep[Matrix[T]]] = new ArithOps[Rep[Matrix[T]]] {
    def +=(a: Rep[Matrix[T]], b: Rep[Matrix[T]]) = matRepArith(a).+=(b)
    /*
    def +(a: Matrix[T], b: Matrix[T]) = a + b
    def -(a: Matrix[T], b: Matrix[T]) = a - b
    def *(a: Matrix[T], b: Matrix[T]) = a dot b
    def /(a: Matrix[T], b: Matrix[T]) = a / b
    def zero = throw new UnsupportedOperationException() //TODO: figure out the size
    def unary_-(a: Matrix[T]) = -a
    def abs(a: Matrix[T]) = a.abs
    def exp(a: Matrix[T]) = a.exp
    def >(a: Matrix[T], b: Matrix[T]) = a > b
    def <(a: Matrix[T], b: Matrix[T]) = a < b
    */
  }

}


trait MatrixOpsExp extends MatrixOps with VariablesExp with DSLOpsExp { this: MatrixImplOps =>
//trait MatrixOpsRepExp extends MatrixOps with MatrixImplOps with DSLOpsExp with FunctionsExp with TupleOpsExp with VariablesExp {
  implicit def varToRepMatOps[A:Manifest](x: Var[Matrix[A]]) = new matRepCls(readVar(x))

  // implemented via method on real data structure
  case class MatrixApply1[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int]) extends Def[Vector[A]]
  case class MatrixApply2[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int]) extends Def[A]
  case class MatrixUpdate[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int], y: Exp[A]) extends Def[Unit]
  case class MatrixNumRows[A:Manifest](x: Exp[Matrix[A]]) extends Def[Int]
  case class MatrixNumCols[A:Manifest](x: Exp[Matrix[A]]) extends Def[Int]
  case class MatrixInsertRow[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], y: Exp[Vector[A]]) extends Def[Matrix[A]]

  // implemented via kernel embedding
  case class MatrixPlus[A:Manifest:Numeric](x: Exp[Matrix[A]], y: Exp[Matrix[A]])
    extends DSLOp(reifyEffects(matrix_plus_impl[A](x,y)))

  case class MatrixPPrint[A:Manifest](x: Exp[Matrix[A]])
    extends DSLOp(reifyEffects(matrix_pprint_impl[A](x)))

  case class MatrixPlusEquals[A:Manifest:Numeric](x: Exp[Matrix[A]], y: Exp[Matrix[A]])
    extends DSLOp(reifyEffects(matrix_plusequals_impl[A](x,y)))

  case class MatrixNew[A:Manifest](numRows: Exp[Int], numCols: Exp[Int])
    extends DSLOp(reifyEffects(matrix_new_impl[A](numRows,numCols)))

  case class MatrixTimes[A:Manifest](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) extends Def[Matrix[A]]
  case class MatrixInverse[A:Manifest](x: Exp[Matrix[A]]) extends Def[Matrix[A]]
  case class MatrixTranspose[A:Manifest](x: Exp[Matrix[A]]) extends Def[Matrix[A]]

  // if x is an m x n MatrixOps, Identity(x) is an n x n square MatrixOps with ones on the diagonal and zeroes elsewhere
  case class MatrixIdentity[A:Manifest](x: Exp[Matrix[A]]) extends Def[Matrix[A]]

  def matrix_apply1[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int]) = MatrixApply1[A](x,i)
  def matrix_apply2[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int]) = MatrixApply2[A](x,i,j)
  def matrix_update[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int], y: Exp[A]) = reflectMutation(MatrixUpdate[A](x,i,j,y))
  def matrix_numrows[A:Manifest](x: Exp[Matrix[A]]) = MatrixNumRows(x)
  def matrix_numcols[A:Manifest](x: Exp[Matrix[A]]) = MatrixNumCols(x)
  def matrix_insertrow[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], y: Exp[Vector[A]]) = reflectMutation(MatrixInsertRow(x,pos,y))

  def matrix_plusequals[A:Manifest:Numeric](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = reflectMutation(MatrixPlusEquals(x,y))
  def matrix_plus[A:Manifest:Numeric](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = MatrixPlus(x, y)
  def matrix_times[A:Manifest](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = MatrixTimes(x, y)
  def matrix_inverse[A:Manifest](x: Exp[Matrix[A]]) = MatrixInverse(x)
  def matrix_transpose[A:Manifest](x: Exp[Matrix[A]]) = MatrixTranspose(x)
  def matrix_pprint[A:Manifest](x: Exp[Matrix[A]]) = reflectEffect(MatrixPPrint(x))
  def matrix_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(MatrixNew[A](numRows,numCols))  
}

/**
 *  Optimizations for composite MatrixOps operations.
 */

trait MatrixOpsExpOpt extends MatrixOpsExp { this: MatrixImplOps =>
  override def matrix_plus[A:Manifest:Numeric](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = (x, y) match {
    // (AB + AD) == A(B + D)
    case (Def(MatrixTimes(a, b)), Def(MatrixTimes(c, d))) if (a == c) => MatrixTimes[A](a.asInstanceOf[Exp[Matrix[A]]], MatrixPlus[A](b.asInstanceOf[Exp[Matrix[A]]],d.asInstanceOf[Exp[Matrix[A]]]))
    // ...
    case _ => super.matrix_plus(x, y)
  }

  override def matrix_times[A:Manifest](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = (x, y) match {
    // X^-1*X = X*X^-1 = I (if X is non-singular)
    case (Def(MatrixInverse(a)), b) if (a == b) => MatrixIdentity[A](a.asInstanceOf[Exp[Matrix[A]]])
    case (b, Def(MatrixInverse(a))) if (a == b) => MatrixIdentity[A](a.asInstanceOf[Exp[Matrix[A]]])

    // X*I = I*X = X
    case (Def(MatrixIdentity(a)), b) if (a == b) => a.asInstanceOf[Exp[Matrix[A]]]
    case (a, Def(MatrixIdentity(b))) if (a == b) => a.asInstanceOf[Exp[Matrix[A]]]

    // else
    case _ => super.matrix_times(x, y)
  }

  override def matrix_inverse[A:Manifest](x: Exp[Matrix[A]]) = x match {
    // (X^-1)^-1 = X (if X is non-singular)
    case (Def(MatrixInverse(a))) => a.asInstanceOf[Exp[Matrix[A]]]
    case _ => super.matrix_inverse(x)
  }

  override def matrix_transpose[A:Manifest](x: Exp[Matrix[A]]) = x match {
    // (X^T)^T = X
    case (Def(MatrixTranspose(a))) => a.asInstanceOf[Exp[Matrix[A]]]
    case _ => super.matrix_transpose(x)
  }


}


trait ScalaGenMatrixOps extends ScalaGenBase {
  val IR: MatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case MatrixApply1(x,i) => emitValDef(sym, quote(x) + "(" + quote(i) + ")")
    case MatrixApply2(x,i,j) => emitValDef(sym, quote(x) + "(" + quote(i) + ", " + quote(j) + ")")
    case MatrixUpdate(x,i,j,y)  => emitValDef(sym, quote(x) + "(" + quote(i) + ", " + quote(j) + ") = " + quote(y))
    case MatrixNumRows(x)  => emitValDef(sym, quote(x) + ".numRows")
    case MatrixNumCols(x)  => emitValDef(sym, quote(x) + ".numCols")
    case MatrixInsertRow(x, pos, y)  => emitValDef(sym, quote(x) + ".insertRow(" + quote(pos) + "," + quote(y) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}