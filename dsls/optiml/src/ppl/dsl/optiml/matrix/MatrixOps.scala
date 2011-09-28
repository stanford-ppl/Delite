package ppl.dsl.optiml.matrix

import ppl.dsl.optiml.datastruct.{OpenCLGenDataStruct, CudaGenDataStruct}
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import scala.virtualization.lms.common.{CudaGenBase, ScalaGenBase, CGenBase, OpenCLGenBase}
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.internal.{GenerationFailedException}
import ppl.delite.framework.Config
import ppl.dsl.optiml.{OptiMLExp, OptiML}
import ppl.dsl.optiml.datastruct.scala._
import ppl.delite.framework.extern.lib._

trait MatrixOps extends DSLType with Variables {
  this: OptiML =>

  object SymmetricMatrix {
    def apply[A:Manifest](n: Rep[Int]) = symmatrix_obj_new(n)
  }

  object Matrix {
    def apply[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_new(numRows, numCols)
    def apply[A:Manifest](xs: Rep[Vector[Vector[A]]]): Rep[Matrix[A]] = matrix_obj_fromvec(xs)
    // Vector is not covariant, so Rep[Vector[MatrixRow[A]]] is unfortunately not a subtype of Rep[Vector[Vector[A]]]
    def apply[A](xs: Rep[Vector[MatrixRow[A]]])(implicit mA: Manifest[A], o: Overloaded1): Rep[Matrix[A]] = matrix_obj_fromvec(xs.asInstanceOf[Rep[Vector[Vector[A]]]])
    def apply[A:Manifest](xs: Rep[Vector[A]]*): Rep[Matrix[A]] = Matrix(Vector(xs: _*))

    def diag[A:Manifest](w: Rep[Int], vals: Rep[Vector[A]]) = matrix_obj_diag(w, vals)
    def identity(w: Rep[Int]) = matrix_obj_identity(w)
    def zeros(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_zeros(numRows, numCols)
    def zerosf(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_zerosf(numRows, numCols)
    def mzerosf(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_mzerosf(numRows, numCols)
    def ones(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_ones(numRows, numCols)
    def onesf(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_onesf(numRows, numCols)
    def rand(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_rand(numRows, numCols)
    def randf(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_randf(numRows, numCols)
    def randn(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_randn(numRows, numCols)
    def randnf(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_randnf(numRows, numCols)
    def mrandnf(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_mrandnf(numRows, numCols)
  }

  implicit def repMatToMatOps[A:Manifest](x: Rep[Matrix[A]]) = new matOpsCls(x)
  implicit def varToMatOps[A:Manifest](x: Var[Matrix[A]]) = new matOpsCls(readVar(x))

  // could convert to infix, but apply doesn't work with it anyways yet
  class matOpsCls[A:Manifest](x: Rep[Matrix[A]]) {
    // conversions
    def toBoolean(implicit conv: Rep[A] => Rep[Boolean]) =  map(e => conv(e))
    def toDouble(implicit conv: Rep[A] => Rep[Double]) = map(e => conv(e))
    def toFloat(implicit conv: Rep[A] => Rep[Float]) = map(e => conv(e))
    def toInt(implicit conv: Rep[A] => Rep[Int]) = map(e => conv(e))
    def toLong(implicit conv: Rep[A] => Rep[Long]) = map(e => conv(e))

    // accessors
    def apply(i: Rep[Int]) = getRow(i)
    def apply(i: Rep[Int], j: Rep[Int]) = matrix_apply(x,i,j)
    def vview(start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean]) = matrix_vview(x,start,stride,length,isRow)
    def getRow(row: Rep[Int]) = matrix_getrow(x,row)
    def getCol(col: Rep[Int]) = matrix_getcol(x,col)
    def slice(startRow: Rep[Int], endRow: Rep[Int], startCol: Rep[Int], endCol: Rep[Int]) = matrix_slice(x,startRow,endRow,startCol,endCol)
    def sliceRows(start: Rep[Int], end: Rep[Int]) = matrix_slicerows(x,start,end)
    def numRows = matrix_numrows(x)
    def numCols = matrix_numcols(x)

    // general
    def t = matrix_transpose(x)
    // TODO: implicit won't trigger
    //override def clone = matrix_clone(x)
    def cloneL() = matrix_clone(x)
    def mutable() = matrix_mutable_clone(x)
    def pprint() = matrix_pprint(x)
    def replicate(i: Rep[Int], j: Rep[Int]) = matrix_repmat(x,i,j)

    // data operations
    def update(i: Rep[Int], j: Rep[Int], y: Rep[A]) = matrix_update(x,i,j,y)
    def update(i: Rep[Int], y: Rep[Vector[A]]) = updateRow(i, y)
    def updateRow(row: Rep[Int], y: Rep[Vector[A]]) = matrix_updaterow(x,row,y)
    def +=(y: Rep[Vector[A]]) = insertRow(x.numRows,y)
    def ++=(y: Rep[Matrix[A]]) = insertAllRows(x.numRows,y)
    def insertRow(pos: Rep[Int], y: Rep[Vector[A]]) = matrix_insertrow(x,pos,y)
    def insertAllRows(pos: Rep[Int], y: Rep[Matrix[A]]) = matrix_insertallrows(x,pos,y)
    def insertCol(pos: Rep[Int], y: Rep[Vector[A]]) = matrix_insertcol(x,pos,y)
    def insertAllCols(pos: Rep[Int], y: Rep[Matrix[A]]) = matrix_insertallcols(x,pos,y)
    def removeRow(pos: Rep[Int]) = removeRows(pos, 1)
    def removeRows(pos: Rep[Int], len: Rep[Int]) = matrix_removerows(x,pos,len)
    def removeCol(pos: Rep[Int]) = removeCols(pos, 1)
    def removeCols(pos: Rep[Int], len: Rep[Int]) = matrix_removecols(x,pos,len)

    // arithmetic operations
    def +(y: Rep[Matrix[A]])(implicit a: Arith[A]) = matrix_plus(x,y)
    def +(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = matrix_plus_scalar(x,y)
    def +=(y: Rep[Matrix[A]])(implicit a: Arith[A]) = { matrix_plusequals(x,y); x }
    def -(y: Rep[Matrix[A]])(implicit a: Arith[A]) = matrix_minus(x,y)
    def -(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = matrix_minus_scalar(x,y)
    def *:*(y: Rep[Matrix[A]])(implicit a: Arith[A]) = matrix_times(x,y)
    def *(y: Rep[Matrix[A]])(implicit a: Arith[A]) = matrix_multiply(x,y)
    def *(y: Rep[Vector[A]])(implicit a: Arith[A], o: Overloaded1) = matrix_times_vector(x,y)
    def *(y: Rep[A])(implicit a: Arith[A], o: Overloaded2) = matrix_times_scalar(x,y)
    def /(y: Rep[Matrix[A]])(implicit a: Arith[A]) = matrix_divide(x,y)
    def /(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = matrix_divide_scalar(x,y)
    //def unary_-(implicit a: Arith[A]) = matrix_unary_minus(x)
    def abs(implicit a: Arith[A]) = matrix_abs(x)
    def exp(implicit a: Arith[A]) = matrix_exp(x)
    def sum(implicit a: Arith[A]) = matrix_sum(x)
    def sumRow(implicit a: Arith[A]) = matrix_sumrow(x)
    def sumCol(implicit a: Arith[A]) = matrix_sumcol(x)
    def inv(implicit conv: Rep[A] => Rep[Double]) = matrix_inverse(x)
    def sigmoid(implicit conv: Rep[A] => Rep[Double]) = matrix_sigmoid(x)
    def sigmoidf(implicit conv: Rep[A] => Rep[Double]) = matrix_sigmoidf(x)

    // ordering operations
    def min(implicit o: Ordering[A], mx: HasMinMax[A]) = matrix_min(x)
    def minRow(implicit o: Ordering[A], mx: HasMinMax[A]) = matrix_minrow(x)
    def max(implicit o: Ordering[A], mx: HasMinMax[A]) = matrix_max(x)
    def maxRow(implicit o: Ordering[A], mx: HasMinMax[A]) = matrix_maxrow(x)
    def :>(y: Rep[Matrix[A]])(implicit o: Ordering[A]) = zip(y) { (a,b) => a > b }
    def :<(y: Rep[Matrix[A]])(implicit o: Ordering[A]) = zip(y) { (a,b) => a < b }

    // bulk operations
    def map[B:Manifest](f: Rep[A] => Rep[B]) = matrix_map(x,f)
    /// TODO: rename to transform?
    def mmap(f: Rep[A] => Rep[A]) = { matrix_mmap(x,f); x }
    def mapRows[B:Manifest](f: Rep[MatrixRow[A]] => Rep[Vector[B]]) = matrix_maprows(x,f)
    def mapRowsToVector[B:Manifest](f: Rep[MatrixRow[A]] => Rep[B], isRow: Rep[Boolean] = unit(false)) = matrix_maprowstovec(x,f,isRow)
    def foreach(block: Rep[A] => Rep[Unit]) = matrix_foreach(x, block)
    def foreachRow(block: Rep[MatrixRow[A]] => Rep[Unit]) = matrix_foreachrow(x, block)
    def zip[B:Manifest,R:Manifest](y: Rep[Matrix[B]])(f: (Rep[A],Rep[B]) => Rep[R]) = matrix_zipwith(x,y,f)
    def reduceRows(f: (Rep[Vector[A]],Rep[Vector[A]]) => Rep[Vector[A]]) = matrix_reducerows(x,f)
    def filterRows(pred: Rep[MatrixRow[A]] => Rep[Boolean]) = matrix_filterrows(x,pred)
    def groupRowsBy[K:Manifest](pred: Rep[Vector[A]] => Rep[K]) = matrix_grouprowsby(x, pred)
    def count(pred: Rep[A] => Rep[Boolean]) = matrix_count(x, pred)
    // def countRows
  }

  def __equal[A](a: Rep[Matrix[A]], b: Rep[Matrix[A]])(implicit o: Overloaded5, mA: Manifest[A]): Rep[Boolean] = matrix_equals(a,b)
  def __equal[A](a: Rep[Matrix[A]], b: Var[Matrix[A]])(implicit o: Overloaded6, mA: Manifest[A]): Rep[Boolean] = matrix_equals(a,b)
  def __equal[A](a: Var[Matrix[A]], b: Rep[Matrix[A]])(implicit o: Overloaded7, mA: Manifest[A]): Rep[Boolean] = matrix_equals(a,b)
  def __equal[A](a: Var[Matrix[A]], b: Var[Matrix[A]])(implicit o: Overloaded8, mA: Manifest[A]): Rep[Boolean] = matrix_equals(a,b)

  // special case overrides
  def infix_:>(x: Rep[Matrix[Float]], y: Rep[Matrix[Float]]): Rep[Matrix[Float]] = x.zip(y) { (a,b) => if (a > b) unit(1f) else unit(0f) }
  def infix_:>(x: Rep[Matrix[Double]], y: Rep[Matrix[Double]])(implicit o: Overloaded1): Rep[Matrix[Double]] = x.zip(y) { (a,b) => if (a > b) unit(1.) else unit(0.) }
  def infix_:>(x: Rep[Matrix[Int]], y: Rep[Matrix[Int]])(implicit o: Overloaded2): Rep[Matrix[Int]] = x.zip(y) { (a,b) => if (a > b) unit(1) else unit(0) }
  def infix_:<(x: Rep[Matrix[Float]], y: Rep[Matrix[Float]]): Rep[Matrix[Float]] = x.zip(y) { (a,b) => if (a > b) unit(1f) else unit(0f) }
  def infix_:<(x: Rep[Matrix[Double]], y: Rep[Matrix[Double]])(implicit o: Overloaded1): Rep[Matrix[Double]] = x.zip(y) { (a,b) => if (a > b) unit(1.) else unit(0.) }
  def infix_:<(x: Rep[Matrix[Int]], y: Rep[Matrix[Int]])(implicit o: Overloaded2): Rep[Matrix[Int]] = x.zip(y) { (a,b) => if (a > b) unit(1) else unit(0) }

  // object defs
  def symmatrix_obj_new[A:Manifest](n: Rep[Int]): Rep[SymmetricMatrix[A]]
  def matrix_obj_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[A]]
  def matrix_obj_fromseq[A:Manifest](xs: Seq[Rep[Vector[A]]]): Rep[Matrix[A]]
  def matrix_obj_fromvec[A:Manifest](xs: Rep[Vector[Vector[A]]]): Rep[Matrix[A]]
  def matrix_obj_diag[A:Manifest](w: Rep[Int], vals: Rep[Vector[A]]): Rep[Matrix[A]]
  def matrix_obj_identity(w: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_zeros(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_zerosf(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_mzerosf(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_ones(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_onesf(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_rand(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_randf(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_randn(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_randnf(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_mrandnf(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]

  // class defs
  def matrix_apply[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int], j: Rep[Int]): Rep[A]
  def matrix_vview[A:Manifest](x: Rep[Matrix[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean]): Rep[Vector[A]]
  def matrix_getrow[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int]): Rep[MatrixRow[A]]
  def matrix_getcol[A:Manifest](x: Rep[Matrix[A]], j: Rep[Int]): Rep[MatrixCol[A]]
  def matrix_slice[A:Manifest](x: Rep[Matrix[A]], startRow: Rep[Int], endRow: Rep[Int], startCol: Rep[Int], endCol: Rep[Int]): Rep[Matrix[A]]
  def matrix_slicerows[A:Manifest](x: Rep[Matrix[A]], start: Rep[Int], end: Rep[Int]): Rep[Matrix[A]]
  def matrix_numrows[A:Manifest](x: Rep[Matrix[A]]): Rep[Int]
  def matrix_numcols[A:Manifest](x: Rep[Matrix[A]]): Rep[Int]

  def matrix_equals[A:Manifest](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Boolean]
  def matrix_transpose[A:Manifest](x: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_clone[A:Manifest](x: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_mutable_clone[A:Manifest](x: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_pprint[A:Manifest](x: Rep[Matrix[A]]): Rep[Unit]
  def matrix_repmat[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int], j: Rep[Int]): Rep[Matrix[A]]

  def matrix_update[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int], j: Rep[Int], y: Rep[A]): Rep[Unit]
  def matrix_updaterow[A:Manifest](x: Rep[Matrix[A]], row: Rep[Int], y: Rep[Vector[A]]): Rep[Unit]
  def matrix_insertrow[A:Manifest](x: Rep[Matrix[A]], pos: Rep[Int], y: Rep[Vector[A]]): Rep[Unit]
  def matrix_insertallrows[A:Manifest](x: Rep[Matrix[A]], pos: Rep[Int], y: Rep[Matrix[A]]): Rep[Unit]
  def matrix_insertcol[A:Manifest](x: Rep[Matrix[A]], pos: Rep[Int], y: Rep[Vector[A]]): Rep[Unit]
  def matrix_insertallcols[A:Manifest](x: Rep[Matrix[A]], pos: Rep[Int], y: Rep[Matrix[A]]): Rep[Unit]
  def matrix_removerows[A:Manifest](x: Rep[Matrix[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit]
  def matrix_removecols[A:Manifest](x: Rep[Matrix[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit]

  def matrix_plus[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_plus_scalar[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[A]): Rep[Matrix[A]]
  def matrix_plusequals[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Unit]
  def matrix_minus[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_minus_scalar[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[A]): Rep[Matrix[A]]
  def matrix_times[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_multiply[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_times_vector[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def matrix_times_scalar[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[A]): Rep[Matrix[A]]
  def matrix_divide[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_divide_scalar[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[A]): Rep[Matrix[A]]
  //def matrix_unary_minus[A:Manifest:Arith](x: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_abs[A:Manifest:Arith](x: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_exp[A:Manifest:Arith](x: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_sum[A:Manifest:Arith](x: Rep[Matrix[A]]): Rep[A]
  def matrix_sumrow[A:Manifest:Arith](x: Rep[Matrix[A]]): Rep[Vector[A]]
  def matrix_sumcol[A:Manifest:Arith](x: Rep[Matrix[A]]): Rep[Vector[A]]
  def matrix_inverse[A](x: Rep[Matrix[A]])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]): Rep[Matrix[Double]]
  def matrix_sigmoid[A](x: Rep[Matrix[A]])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]): Rep[Matrix[Double]]
  def matrix_sigmoidf[A](x: Rep[Matrix[A]])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double]): Rep[Matrix[Float]]

  def matrix_min[A:Manifest:Ordering:HasMinMax](x: Rep[Matrix[A]]): Rep[A]
  def matrix_minrow[A:Manifest:Ordering:HasMinMax](x: Rep[Matrix[A]]): Rep[Vector[A]]
  def matrix_max[A:Manifest:Ordering:HasMinMax](x: Rep[Matrix[A]]): Rep[A]
  def matrix_maxrow[A:Manifest:Ordering:HasMinMax](x: Rep[Matrix[A]]): Rep[Vector[A]]

  def matrix_map[A:Manifest,B:Manifest](x: Rep[Matrix[A]], f: Rep[A] => Rep[B]): Rep[Matrix[B]]
  def matrix_mmap[A:Manifest](x: Rep[Matrix[A]], f: Rep[A] => Rep[A]): Rep[Unit]
  def matrix_maprows[A:Manifest,B:Manifest](x: Rep[Matrix[A]], f: Rep[MatrixRow[A]] => Rep[Vector[B]]): Rep[Matrix[B]]
  def matrix_maprowstovec[A:Manifest,B:Manifest](x: Rep[Matrix[A]], f: Rep[MatrixRow[A]] => Rep[B], isRow: Rep[Boolean]): Rep[Vector[B]]
  def matrix_foreach[A:Manifest](x: Rep[Matrix[A]], block: Rep[A] => Rep[Unit]): Rep[Unit]
  def matrix_foreachrow[A:Manifest](x: Rep[Matrix[A]], block: Rep[MatrixRow[A]] => Rep[Unit]): Rep[Unit]
  def matrix_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Rep[Matrix[A]], y: Rep[Matrix[B]], f: (Rep[A],Rep[B]) => Rep[R]): Rep[Matrix[R]]
  def matrix_reducerows[A:Manifest](x: Rep[Matrix[A]], f: (Rep[Vector[A]],Rep[Vector[A]]) => Rep[Vector[A]]): Rep[Vector[A]]
  def matrix_filterrows[A:Manifest](x: Rep[Matrix[A]], pred: Rep[MatrixRow[A]] => Rep[Boolean]): Rep[Matrix[A]]
  def matrix_grouprowsby[A:Manifest,K:Manifest](x: Rep[Matrix[A]], pred: Rep[Vector[A]] => Rep[K]): Rep[Vector[Matrix[A]]]
  def matrix_count[A:Manifest](x: Rep[Matrix[A]], pred: Rep[A] => Rep[Boolean]): Rep[Int]
}


trait MatrixOpsExp extends MatrixOps with VariablesExp {
  this: MatrixImplOps with OptiMLExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class SymmetricMatrixObjectNew[A:Manifest](n:Exp[Int]) extends Def[SymmetricMatrix[A]] {
     val m = manifest[A]
     val mM = manifest[SymmetricMatrixImpl[A]]
  }

  case class MatrixObjectNew[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) extends Def[Matrix[A]] {
     val m = manifest[A]
     val mM = manifest[MatrixImpl[A]]
  }
  //case class MatrixApply[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int]) extends Def[A]
  case class MatrixVView[A:Manifest](x: Exp[Matrix[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean]) extends Def[Vector[A]]
  case class MatrixGetRow[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int]) extends Def[MatrixRow[A]] {
    val m = manifest[A]
  }
  case class MatrixGetCol[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int]) extends Def[MatrixCol[A]] {
    val m = manifest[A]
  }
  case class MatrixRawData[A:Manifest](x: Exp[Matrix[A]]) extends Def[Array[A]]
  case class MatrixNumRows[A:Manifest](x: Exp[Matrix[A]]) extends Def[Int]
  case class MatrixNumCols[A:Manifest](x: Exp[Matrix[A]]) extends Def[Int]
  case class MatrixClone[A:Manifest](x: Exp[Matrix[A]]) extends Def[Matrix[A]]
  case class MatrixUpdate[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int], y: Exp[A]) extends Def[Unit]
  case class MatrixInsertRow[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], y: Exp[Vector[A]]) extends Def[Unit]
  case class MatrixInsertAllRows[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], y: Exp[Matrix[A]]) extends Def[Unit]
  case class MatrixInsertCol[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], y: Exp[Vector[A]]) extends Def[Unit]
  case class MatrixInsertAllCols[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], y: Exp[Matrix[A]]) extends Def[Unit]
  case class MatrixRemoveRows[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], len: Exp[Int]) extends Def[Unit]
  case class MatrixRemoveCols[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], len: Exp[Int]) extends Def[Unit]

  /////////////////////////////////////
  // implemented via kernel embedding

  case class MatrixObjectFromSeq[A:Manifest](xs: Seq[Rep[Vector[A]]])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_obj_fromseq_impl(xs))) {
      val m = manifest[A]
  }

  case class MatrixObjectFromVec[A:Manifest](xs: Exp[Vector[Vector[A]]])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_obj_fromvec_impl(xs))) {
    val m = manifest[A]
  }

  case class MatrixObjectDiag[A:Manifest](w: Exp[Int], vals: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_obj_diag_impl(w, vals))) {
      val m = manifest[A]
  }

  case class MatrixObjectIdentity(w: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_obj_identity_impl(w)))

  //case class MatrixObjectZeros(numRows: Exp[Int], numCols: Exp[Int])
  //  extends DeliteOpSingleTask(reifyEffectsHere(matrix_obj_zeros_impl(numRows, numCols)))

  //case class MatrixObjectZerosF(numRows: Exp[Int], numCols: Exp[Int])
  //  extends DeliteOpSingleTask(reifyEffectsHere(matrix_obj_zerosf_impl(numRows, numCols)))

  case class MatrixObjectOnes(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_obj_ones_impl(numRows, numCols)))

  case class MatrixObjectOnesF(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_obj_onesf_impl(numRows, numCols)))

  case class MatrixObjectRand(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_obj_rand_impl(numRows, numCols)))

  case class MatrixObjectRandF(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_obj_randf_impl(numRows, numCols)))

  case class MatrixObjectRandn(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_obj_randn_impl(numRows, numCols)))

  case class MatrixObjectRandnF(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_obj_randnf_impl(numRows, numCols)))

  case class MatrixApply[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_apply_impl(x, i, j)))

  case class MatrixSlice[A:Manifest](x: Exp[Matrix[A]], startRow: Exp[Int], endRow: Exp[Int], startCol: Exp[Int], endCol: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_slice_impl(x,startRow,endRow,startCol,endCol)))

  case class MatrixSliceRows[A:Manifest](x: Exp[Matrix[A]], start: Exp[Int], end: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_slicerows_impl(x,start,end)))

//  case class MatrixUpdateRow[A:Manifest](x: Exp[Matrix[A]], row: Exp[Int], y: Exp[Vector[A]])
//    extends DeliteOpSingleTask(reifyEffectsHere(matrix_updaterow_impl(x,row,y)))

  // this is a single task right now because of the likely early exit. should we have a delite op for this?
  case class MatrixEquals[A:Manifest](x: Exp[Matrix[A]], y: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_equals_impl[A](x,y)))

  case class MatrixTranspose[A:Manifest](x: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_transpose_impl(x))) {
    val m = manifest[A]
  }

  case class MatrixPPrint[A:Manifest](x: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_pprint_impl[A](x)))

  case class MatrixRepmat[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_repmat_impl[A](x,i,j)))

  case class MatrixInverse[A](x: Exp[Matrix[A]])(implicit val mA: Manifest[A], val conv: Exp[A] => Exp[Double])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_inverse_impl[A](x)))

  case class MatrixMinRow[A:Manifest:Ordering:HasMinMax](x: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_minrow_impl(x)))

  case class MatrixMaxRow[A:Manifest:Ordering:HasMinMax](x: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_maxrow_impl(x)))

//  case class MatrixMapRows[A:Manifest,B:Manifest](x: Exp[Matrix[A]], f: Exp[MatrixRow[A]] => Exp[Vector[B]])
//    extends DeliteOpSingleTask(reifyEffectsHere(matrix_maprows_impl(x,f)))

//  case class MatrixForeachRow[A:Manifest](x: Exp[Matrix[A]], f: Exp[MatrixRow[A]] => Exp[Unit])
//    extends DeliteOpSingleTask(reifyEffectsHere(matrix_foreachrow_impl(x,f)))

  case class MatrixFilterRows[A:Manifest](x: Exp[Matrix[A]], pred: Exp[MatrixRow[A]] => Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_filterrows_impl(x,pred)))

  case class MatrixSumCol[A:Manifest:Arith](x: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffects(matrix_sumcol_impl(x)))

  case class MatrixGroupRowsBy[A:Manifest,K:Manifest](x: Exp[Matrix[A]], pred: Exp[Vector[A]] => Exp[K])
    extends DeliteOpSingleTask(reifyEffects(matrix_grouprowsby_impl(x,pred)))

  ///////////////////////////////////////////////////////////////////
  // BLAS enabled routines

  // TODO: generalize this so that we can generate fused, delite parallel op, or BLAS variants
  // having separate IR nodes breaks pattern matching optimizations...

  case class MatrixTimesVector[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_times_vector_impl(x,y))) {

    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  case class MatrixTimesVectorBLAS[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Vector[A]]) extends DeliteOpExternal[Vector[A]] {
    def alloc = Vector[A](x.numRows, unit(false))
    val funcName = "matMultV"

    def mV = manifest[VectorImpl[A]]
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  case class MatrixMultiply[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_multiply_impl(x,y))) {

    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  case class MatrixMultiplyBLAS[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) extends DeliteOpExternal[Matrix[A]] {
    def alloc = Matrix[A](x.numRows, y.numCols)
    val funcName = "matMult"

    def mM = manifest[MatrixImpl[A]]
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  case class MatrixSigmoid[A](in: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_sigmoid_impl(in))) {
    // extends DeliteOpMap[A,Double,Matrix[Double]] {
    //
    //     def alloc = Matrix[Double](in.numRows, in.numCols)
    //     val size = in.numRows*in.numCols
    //     def func = e => (1.0/(1.0+Math.exp(conv(e)*(-1))))
  }  

  case class MatrixSigmoidF[A](in: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_sigmoidf_impl(in))) {
    // extends DeliteOpMap[A,Float,Matrix[Float]] {
    //
    // def alloc = Matrix[Float](in.numRows, in.numCols)
    // val size = in.numRows*in.numCols
    // def func = e => (1.0/(1.0+Math.exp(conv(e)*(-1)))).asInstanceOfL[Float]
  }  

  case class MatrixSigmoidVectorized[A:Manifest](in: Exp[Matrix[A]]) extends DeliteOpExternal[Matrix[A]] {
    def alloc = Matrix[A](in.numRows, in.numCols)
    val funcName = "matSigmoid"

    def mM = manifest[MatrixImpl[A]]
  }

  ////////////////////////////////
  // implemented via delite ops
  
  abstract class MatrixArithmeticMap[A:Manifest:Arith](in: Exp[Matrix[A]]) extends DeliteOpMap[A,A,Matrix[A]] {
    def alloc = Matrix[A](in.numRows, in.numCols)
    val size = matrix_dcsize(in)
    
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }
  
  abstract class MatrixArithmeticZipWith[A:Manifest:Arith](inA: Exp[Matrix[A]], inB: Exp[Matrix[A]]) extends DeliteOpZipWith[A,A,A,Matrix[A]] {
    def alloc = Matrix[A](inA.numRows, inA.numCols)
    val size = matrix_dcsize(inA)
    
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }
    
  case class MatrixPlus[A:Manifest:Arith](inA: Exp[Matrix[A]], inB: Exp[Matrix[A]])
    extends MatrixArithmeticZipWith(inA, inB) {

    def func = (a,b) => a + b
  }

  case class MatrixPlusScalar[A:Manifest:Arith](in: Exp[Matrix[A]], y: Exp[A])
    extends MatrixArithmeticMap(in) {

    def func = e => e + y
  }

  case class MatrixPlusEquals[A:Manifest:Arith](inA: Exp[Matrix[A]], inB: Exp[Matrix[A]])
    extends DeliteOpIndexedLoop {

    val size = matrix_dcsize(inA)
    def func = i => dc_update(inA, i, dc_apply(inA,i) + dc_apply(inB,i))
  }

  case class MatrixMinus[A:Manifest:Arith](inA: Exp[Matrix[A]], inB: Exp[Matrix[A]])
    extends MatrixArithmeticZipWith(inA, inB) {

    def func = (a,b) => a - b
  }

  case class MatrixMinusScalar[A:Manifest:Arith](in: Exp[Matrix[A]], y: Exp[A])
    extends MatrixArithmeticMap(in) {

    def func = e => e - y
  }

  case class MatrixTimes[A:Manifest:Arith](inA: Exp[Matrix[A]], inB: Exp[Matrix[A]])
    extends MatrixArithmeticZipWith(inA, inB) {

    def func = (a,b) => a * b
  }

  case class MatrixTimesScalar[A:Manifest:Arith](in: Exp[Matrix[A]], y: Exp[A])
    extends MatrixArithmeticMap(in) {

    def func = e => e * y
  }

  case class MatrixDivide[A:Manifest:Arith](inA: Exp[Matrix[A]], inB: Exp[Matrix[A]])
    extends MatrixArithmeticZipWith(inA, inB) {

    def func = (a,b) => a / b
  }

  case class MatrixDivideScalar[A:Manifest:Arith](in: Exp[Matrix[A]], y: Exp[A])
    extends MatrixArithmeticMap(in) {

    def func = e => e / y
  }
  
  case class MatrixSum[A:Manifest:Arith](in: Exp[Matrix[A]]) 
    extends DeliteOpReduce[A] {
      
    val m = manifest[A] //TODO: externalize?
    val a = implicitly[Arith[A]]

    val size = matrix_dcsize(in)
    val zero = implicitly[Arith[A]].empty
    def func = (a,b) => a + b
  }
  
  /* this would be nice, but case class inheritance is deprecated */
  //case class MatrixSumRow[A:Manifest:Arith](x: Exp[Matrix[A]]) extends MatrixMapRowsToVec[A,A](x, row => row.sum, unit(false))  
  case class MatrixSumRow[A:Manifest:Arith](x: Exp[Matrix[A]])
    extends DeliteOpMap[Int,A,Vector[A]] {

    def alloc = Vector[A](x.numRows, unit(false))
    val in = (0::x.numRows)
    val size = x.numRows
    def func = i => x(i).sum
  } 

/*
  case class MatrixSumCol[A:Manifest:Arith](x: Exp[Matrix[A]])
    extends DeliteOpMap[Vector[A],A,Vector] {

    val alloc = reifyEffects(Vector[A](x.numCols, true))
    val in = reifyEffects {
      val tcoll = Vector[Vector[A]](x.numCols, true)
      for (i <- 0 until x.numCols){
        tcoll(i) = x.getCol(i)
      }
      tcoll
    }

    val v = fresh[Vector[A]]
    val func = v.sum
  }
*/

/*
 case class MatrixUnaryMinus[A:Manifest:Arith](in: Exp[Matrix[A]])
   extends MatrixArithmeticMap {

   val alloc = reifyEffects(Matrix[A](in.numRows, in.numCols))
   val v = fresh[A]
   val func = v.unary_-
 }
*/

  case class MatrixAbs[A:Manifest:Arith](in: Exp[Matrix[A]])
    extends MatrixArithmeticMap(in) {

    def func = e => e.abs
  }

  case class MatrixExp[A:Manifest:Arith](in: Exp[Matrix[A]])
    extends MatrixArithmeticMap(in) {

    def func = e => e.exp
  }

  case class MatrixMin[A:Manifest:Ordering:HasMinMax](in: Exp[Matrix[A]])
    extends DeliteOpReduce[A] {

    val size = matrix_dcsize(in)
    val zero = implicitly[HasMinMax[A]].maxValue
    def func = (a,b) => if (a < b) a else b
  }

  case class MatrixMax[A:Manifest:Ordering:HasMinMax](in: Exp[Matrix[A]])
    extends DeliteOpReduce[A] {

    val size = matrix_dcsize(in)
    val zero = implicitly[HasMinMax[A]].minValue
    def func = (a,b) => if (a > b) a else b
  }

  case class MatrixMap[A:Manifest,B:Manifest](in: Exp[Matrix[A]], func: Exp[A] => Exp[B])
    extends DeliteOpMap[A,B,Matrix[B]] {

    def alloc = Matrix[B](in.numRows, in.numCols)
    val size = matrix_dcsize(in)
    
    def mA = manifest[A]
    def mB = manifest[B]
  }

  case class MatrixMutableMap[A:Manifest](in: Exp[Matrix[A]], block: Exp[A] => Exp[A])
    extends DeliteOpIndexedLoop {

    val size = matrix_dcsize(in)
    def func = i => dc_update(in, i, block(dc_apply(in,i)))
  }

  case class MatrixMapRows[A:Manifest,B:Manifest](x: Exp[Matrix[A]], block: Exp[MatrixRow[A]] => Exp[Vector[B]], out: Exp[Matrix[B]])
    extends DeliteOpIndexedLoop {

    val size = x.numRows
    def func = i => { out(i) = block(x(i)) } // updateRow should be fused with function application
  }

  case class MatrixForeachRow[A:Manifest](x: Exp[Matrix[A]], block: Exp[MatrixRow[A]] => Exp[Unit])
    extends DeliteOpIndexedLoop {

    val size = x.numRows
    def func = i => block(x(i))
  }

  case class MatrixMapRowsToVec[A:Manifest,B: Manifest](x: Exp[Matrix[A]], rowFunc: Exp[MatrixRow[A]] => Exp[B], isRow: Exp[Boolean])
    extends DeliteOpMap[Int,B,Vector[B]] {

    def alloc = Vector[B](x.numRows, isRow)
    val in = (0::x.numRows)
    val size = x.numRows
    def func = i => rowFunc(x(i))   
  }

  case class MatrixForeach[A:Manifest](in: Exp[Matrix[A]], func: Exp[A] => Exp[Unit])
    extends DeliteOpForeach[A] {

    val size = matrix_dcsize(in)
    def sync = n => List()
  }

  case class MatrixUpdateRow[A:Manifest](x: Exp[Matrix[A]], row: Exp[Int], y: Exp[Vector[A]])
    extends DeliteOpIndexedLoop {
    
    val size = copyTransformedOrElse(_.size)(y.length) // TODO: assert y.length == x.numCols
    def func = j => { x(row,j) = y(j) } 
  }

  case class MatrixZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[Matrix[A]], inB: Exp[Matrix[B]],
                                                             func: (Exp[A], Exp[B]) => Exp[R])
    extends DeliteOpZipWith[A,B,R,Matrix[R]] {

    def alloc = Matrix[R](inA.numRows, inA.numCols)
    val size = matrix_dcsize(inA)
  }

  // More efficient (though slightly uglier) to express this as a loop directly. 
  // TODO: nicer DeliteOpLoop templates? e.g. DeliteOpReductionLoop, ...
  case class MatrixReduceRows[A:Manifest](x: Exp[Matrix[A]], func: (Exp[Vector[A]], Exp[Vector[A]]) => Exp[Vector[A]])
    extends DeliteOpReduceLike[Vector[A]] {

    val size = x.numRows
    val zero = EmptyVector[A]
    
    lazy val body: Def[Vector[A]] = copyBodyOrElse(DeliteReduceElem[Vector[A]](
      func = reifyEffects(x(v)),
      Nil,
      zero = this.zero,
      rV = this.rV,
      rFunc = reifyEffects(this.func(rV._1, rV._2)),
      true
    ))
  }

  case class MatrixCount[A:Manifest](in: Exp[Matrix[A]], cond: Exp[A] => Exp[Boolean]) 
    extends DeliteOpFilterReduce[A,Int] {

    val size = matrix_dcsize(in)
    val zero = unit(0)
    def func = e => unit(1)
    def reduce = (a,b) => a + b   
    
    def m = manifest[A]
  } 


  ////////////////////
  // object interface

  def symmatrix_obj_new[A:Manifest](n: Exp[Int]) = reflectMutable(SymmetricMatrixObjectNew[A](n))
  def matrix_obj_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) = reflectMutable(MatrixObjectNew[A](numRows, numCols)) //XXX
  def matrix_obj_fromseq[A:Manifest](xs: Seq[Exp[Vector[A]]]) = reflectPure(MatrixObjectFromSeq(xs)) //XXX
  def matrix_obj_fromvec[A:Manifest](xs: Exp[Vector[Vector[A]]]) = reflectPure(MatrixObjectFromVec(xs))
  def matrix_obj_diag[A:Manifest](w: Exp[Int], vals: Exp[Vector[A]]) = reflectPure(MatrixObjectDiag(w, vals))
  def matrix_obj_identity(w: Exp[Int]) = reflectPure(MatrixObjectIdentity(w))
  def matrix_obj_zeros(numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(MatrixObjectNew[Double](numRows, numCols))//MatrixObjectZeros(numRows, numCols))
  def matrix_obj_zerosf(numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(MatrixObjectNew[Float](numRows, numCols))//MatrixObjectZerosF(numRows, numCols))
  def matrix_obj_mzerosf(numRows: Exp[Int], numCols: Exp[Int]) = reflectMutable(MatrixObjectNew[Float](numRows, numCols))//reflectPure(MatrixObjectZerosF(numRows, numCols))
  def matrix_obj_ones(numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(MatrixObjectOnes(numRows, numCols))
  def matrix_obj_onesf(numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(MatrixObjectOnesF(numRows, numCols))
  def matrix_obj_rand(numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(MatrixObjectRand(numRows, numCols))
  def matrix_obj_randf(numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(MatrixObjectRandF(numRows, numCols))
  def matrix_obj_randn(numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(MatrixObjectRandn(numRows, numCols))
  def matrix_obj_randnf(numRows: Rep[Int], numCols: Rep[Int]) = reflectPure(MatrixObjectRandnF(numRows, numCols))
  def matrix_obj_mrandnf(numRows: Rep[Int], numCols: Rep[Int]) = reflectMutable(MatrixObjectRandnF(numRows, numCols)) //TR was reflectPure (why?)


  ///////////////////
  // class interface

  def matrix_apply[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int]) = reflectPure(MatrixApply[A](x,i,j))
  def matrix_vview[A:Manifest](x: Exp[Matrix[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean]) = reflectPure(MatrixVView(x, start, stride, length, isRow))
  def matrix_getrow[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int]) = reflectPure(MatrixGetRow[A](x,i))
  def matrix_getcol[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int]) = reflectPure(MatrixGetCol[A](x,i))
  def matrix_slice[A:Manifest](x: Exp[Matrix[A]], startRow: Exp[Int], endRow: Exp[Int], startCol: Exp[Int], endCol: Exp[Int]) = reflectPure(MatrixSlice(x,startRow,endRow,startCol,endCol))
  def matrix_slicerows[A:Manifest](x: Exp[Matrix[A]], start: Exp[Int], end: Exp[Int]) = reflectPure(MatrixSliceRows(x,start,end))
  def matrix_numrows[A:Manifest](x: Exp[Matrix[A]]) = reflectPure(MatrixNumRows(x))
  def matrix_numcols[A:Manifest](x: Exp[Matrix[A]]) = reflectPure(MatrixNumCols(x))

  def matrix_update[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int], y: Exp[A]) = reflectWrite(x)(MatrixUpdate[A](x,i,j,y))
  def matrix_updaterow[A:Manifest](x: Exp[Matrix[A]], row: Exp[Int], y: Exp[Vector[A]]) = reflectWrite(x)(MatrixUpdateRow(x,row,y))
  def matrix_insertrow[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], y: Exp[Vector[A]]) = reflectWrite(x)(MatrixInsertRow(x,pos,y))
  def matrix_insertallrows[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], y: Exp[Matrix[A]]) = reflectWrite(x)(MatrixInsertAllRows(x,pos,y))
  def matrix_insertcol[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], y: Exp[Vector[A]]) = reflectWrite(x)(MatrixInsertCol(x,pos,y))
  def matrix_insertallcols[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], y: Exp[Matrix[A]]) = reflectWrite(x)(MatrixInsertAllCols(x,pos,y))
  def matrix_removerows[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], len: Exp[Int]) = reflectWrite(x)(MatrixRemoveRows(x,pos,len))
  def matrix_removecols[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], len: Exp[Int]) = reflectWrite(x)(MatrixRemoveCols(x,pos,len))

  def matrix_equals[A:Manifest](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = reflectPure(MatrixEquals(x,y))
  def matrix_transpose[A:Manifest](x: Exp[Matrix[A]]) = reflectPure(MatrixTranspose(x))
  def matrix_clone[A:Manifest](x: Exp[Matrix[A]]) = reflectPure(MatrixClone(x))
  def matrix_mutable_clone[A:Manifest](x: Exp[Matrix[A]]) = reflectMutable(MatrixClone(x))
  def matrix_pprint[A:Manifest](x: Exp[Matrix[A]]) = reflectEffect(MatrixPPrint(x)) // TODO: simple
  def matrix_repmat[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int]) = reflectPure(MatrixRepmat(x,i,j))

  def matrix_plus[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = reflectPure(MatrixPlus(x, y))
  def matrix_plus_scalar[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[A]) = reflectPure(MatrixPlusScalar(x, y))
  def matrix_minus[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = reflectPure(MatrixMinus(x,y))
  def matrix_minus_scalar[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[A]) = reflectPure(MatrixMinusScalar(x,y))
  def matrix_times[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = reflectPure(MatrixTimes(x,y))
  def matrix_multiply[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = {
    if (Config.useBlas && (manifest[A] == manifest[Double] || manifest[A] == manifest[Float])) reflectPure(MatrixMultiplyBLAS(x,y))
    else reflectPure(MatrixMultiply(x,y))
  }
  def matrix_times_vector[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Vector[A]]) = {
    if (Config.useBlas && (manifest[A] == manifest[Double] || manifest[A] == manifest[Float])) reflectPure(MatrixTimesVectorBLAS(x,y))
    else reflectPure(MatrixTimesVector(x,y))
  }
  def matrix_times_scalar[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[A]) = reflectPure(MatrixTimesScalar(x,y))
  def matrix_divide[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = reflectPure(MatrixDivide(x,y))
  def matrix_divide_scalar[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[A]) = reflectPure(MatrixDivideScalar(x,y))
  //def matrix_unary_minus[A:Manifest:Arith](x: Exp[Matrix[A]]) = MatrixUnaryMinus(x)
  def matrix_abs[A:Manifest:Arith](x: Exp[Matrix[A]]) = reflectPure(MatrixAbs(x))
  def matrix_exp[A:Manifest:Arith](x: Exp[Matrix[A]]) = reflectPure(MatrixExp(x))
  def matrix_sum[A:Manifest:Arith](x: Exp[Matrix[A]]) = reflectPure(MatrixSum(x))
  def matrix_sumrow[A:Manifest:Arith](x: Exp[Matrix[A]]) = reflectPure(MatrixSumRow(x))
  def matrix_sumcol[A:Manifest:Arith](x: Exp[Matrix[A]]) = reflectPure(MatrixSumCol(x))
  def matrix_inverse[A](x: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double]) = reflectPure(MatrixInverse(x))
  def matrix_sigmoid[A](x: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double]) = {
    if (Config.useBlas && manifest[A] == manifest[Double]) reflectPure(MatrixSigmoidVectorized(x))
    reflectPure(MatrixSigmoid(x))
  }
  def matrix_sigmoidf[A](x: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double]) = {
    if (Config.useBlas && manifest[A] == manifest[Float]) reflectPure(MatrixSigmoidVectorized(x))
    reflectPure(MatrixSigmoidF(x))
  }

  def matrix_plusequals[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = reflectWrite(x)(MatrixPlusEquals(x,y))
  
  def matrix_min[A:Manifest:Ordering:HasMinMax](x: Exp[Matrix[A]]) = reflectPure(MatrixMin(x))
  def matrix_minrow[A:Manifest:Ordering:HasMinMax](x: Exp[Matrix[A]]) = reflectPure(MatrixMinRow(x))
  def matrix_max[A:Manifest:Ordering:HasMinMax](x: Exp[Matrix[A]]) = reflectPure(MatrixMax(x))
  def matrix_maxrow[A:Manifest:Ordering:HasMinMax](x: Exp[Matrix[A]]) = reflectPure(MatrixMaxRow(x))

  def matrix_map[A:Manifest,B:Manifest](x: Exp[Matrix[A]], f: Exp[A] => Exp[B]) = reflectPure(MatrixMap(x, f))
  def matrix_mmap[A:Manifest](x: Exp[Matrix[A]], f: Exp[A] => Exp[A]) = reflectWrite(x)(MatrixMutableMap(x, f)) // effect??
  def matrix_maprows[A:Manifest,B:Manifest](x: Exp[Matrix[A]], f: Exp[MatrixRow[A]] => Exp[Vector[B]]) = {
    val out = matrix_obj_new[B](x.numRows, x.numCols)
    reflectWrite(out)(MatrixMapRows(x,f,out))
    out.unsafeImmutable // will this work?
  }
  def matrix_maprowstovec[A:Manifest,B:Manifest](x: Exp[Matrix[A]], f: Exp[MatrixRow[A]] => Exp[B], isRow: Exp[Boolean] = unit(true)) = {
    reflectPure(MatrixMapRowsToVec(x, f, isRow))
  }
  def matrix_foreach[A:Manifest](x: Exp[Matrix[A]], block: Exp[A] => Exp[Unit]) = {
    reflectEffect(MatrixForeach(x, block)) // read??
  }
  def matrix_foreachrow[A:Manifest](x: Exp[Matrix[A]], block: Exp[MatrixRow[A]] => Exp[Unit]) = {
    reflectEffect(MatrixForeachRow(x, block)) // read??
  }
  def matrix_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Exp[Matrix[A]], y: Exp[Matrix[B]], f: (Exp[A],Exp[B]) => Exp[R]) = {
    reflectPure(MatrixZipWith(x, y, f))
  }
  def matrix_reducerows[A:Manifest](x: Exp[Matrix[A]], f: (Exp[Vector[A]],Exp[Vector[A]]) => Exp[Vector[A]]) = {
    reflectPure(MatrixReduceRows(x, f))
  }
  def matrix_filterrows[A:Manifest](x: Exp[Matrix[A]], pred: Exp[MatrixRow[A]] => Exp[Boolean]) = reflectPure(MatrixFilterRows(x, pred))
  def matrix_grouprowsby[A:Manifest,K:Manifest](x: Exp[Matrix[A]], pred: Exp[Vector[A]] => Exp[K]) = reflectPure(MatrixGroupRowsBy(x,pred))
  def matrix_count[A:Manifest](x: Exp[Matrix[A]], pred: Exp[A] => Exp[Boolean]) = reflectPure(MatrixCount(x, pred))

  //////////////////
  // internal

  def matrix_dcsize[A:Manifest](x: Exp[Matrix[A]]) = x.numRows * x.numCols
  def matrix_raw_data[A:Manifest](x: Exp[Matrix[A]]) = reflectPure(MatrixRawData(x))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = {
    (e match {
      case MatrixNumRows(x) => matrix_numrows(f(x))
      case MatrixNumCols(x) => matrix_numcols(f(x))
      case e@MatrixGetRow(x,i) => matrix_getrow(f(x),f(i))(e.m)
      case e@MatrixGetCol(x,i) => matrix_getcol(f(x),f(i))(e.m)
      case MatrixVView(x, start, stride, length, isRow) => matrix_vview(f(x),f(start),f(stride),f(length),f(isRow)) // should set original, too?
      // delite ops
      case e@MatrixObjectIdentity(x) => reflectPure(new { override val original = Some(f,e) } with MatrixObjectIdentity(f(x)))(mtype(manifest[A]))
      case e@MatrixObjectFromVec(x) => reflectPure(new { override val original = Some(f,e) } with MatrixObjectFromVec(f(x))(e.m))(mtype(manifest[A]))
      case e@MatrixObjectDiag(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixObjectDiag(f(x),f(y))(e.m))(mtype(manifest[A]))
      case e@MatrixApply(x,i,j) => reflectPure(new { override val original = Some(f,e) } with MatrixApply(f(x),f(i),f(j)))(mtype(manifest[A]))
      case e@MatrixAbs(x) => reflectPure(new { override val original = Some(f,e) } with MatrixAbs(f(x))(e.m, e.a))(mtype(manifest[A]))
      case e@MatrixSum(x) => reflectPure(new { override val original = Some(f,e) } with MatrixSum(f(x))(e.m, e.a))(mtype(manifest[A]))
      case e@MatrixMinus(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixMinus(f(x),f(y))(e.m, e.a))(mtype(manifest[A]))
      case e@MatrixPlus(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixPlus(f(x),f(y))(e.m, e.a))(mtype(manifest[A]))
      case e@MatrixTimes(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixTimes(f(x),f(y))(e.m, e.a))(mtype(manifest[A]))
      case e@MatrixMap(x,g) => reflectPure(new { override val original = Some(f,e) } with MatrixMap(f(x),f(g))(e.mA, e.mB))(mtype(manifest[A]))
      case e@MatrixTimesVector(x,y) => reflectPure(new {override val original = Some(f,e) } with MatrixTimesVector(f(x),f(y))(e.m,e.a))(mtype(manifest[A]))
      case e@MatrixTimesVectorBLAS(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixTimesVectorBLAS(f(x),f(y))(e.m,e.a))(mtype(manifest[A]))
      case e@MatrixMultiply(x,y) => reflectPure(new {override val original = Some(f,e) } with MatrixMultiply(f(x),f(y))(e.m,e.a))(mtype(manifest[A]))
      case e@MatrixMultiplyBLAS(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixMultiplyBLAS(f(x),f(y))(e.m,e.a))(mtype(manifest[A]))
      case e@MatrixInverse(x) => reflectPure(new {override val original = Some(f,e) } with MatrixInverse(f(x))(e.mA,f(e.conv)))(mtype(manifest[A]))
      case e@MatrixTranspose(x) => reflectPure(new {override val original = Some(f,e) } with MatrixTranspose(f(x))(e.m))(mtype(manifest[A]))
      // reflected
      case Reflect(MatrixNumRows(x), u, es) => reflectMirrored(Reflect(MatrixNumRows(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MatrixNumCols(x), u, es) => reflectMirrored(Reflect(MatrixNumCols(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatrixGetRow(x,i), u, es) => reflectMirrored(Reflect(MatrixGetRow(f(x),f(i))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatrixGetCol(x,i), u, es) => reflectMirrored(Reflect(MatrixGetCol(f(x),f(i))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MatrixApply(x,i,j), u, es) => reflectMirrored(Reflect(MatrixApply(f(x),f(i),f(j)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MatrixClone(x), u, es) => reflectMirrored(Reflect(MatrixClone(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(MatrixUpdate(x,i,j,r), u, es) => reflectMirrored(Reflect(MatrixUpdate(f(x),f(i),f(j),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatrixInsertAllCols(x,y,z), u, es) => reflectMirrored(Reflect(MatrixInsertAllCols(f(x),f(y),f(z)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatrixRemoveCols(x,y,z), u, es) => reflectMirrored(Reflect(MatrixRemoveCols(f(x),f(y),f(z)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatrixObjectNew(x,y), u, es) => reflectMirrored(Reflect(MatrixObjectNew(f(x),f(y))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
      // reflected ops
      case Reflect(e@MatrixUpdateRow(x,r,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixUpdateRow(f(x),f(r),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatrixZipWith(x,y,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixZipWith(f(x),f(y),f(g)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case _ => super.mirror(e, f)
    }).asInstanceOf[Exp[A]] // why??
  }
  
  /////////////////////
  // aliases and sharing

  // TODO: precise sharing info for other IR types (default is conservative)

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case MatrixMultiply(a,b) => Nil
    case MatrixTimes(a,b) => Nil
    case MatrixTimesVector(a,v) => Nil
    case MatrixTimesScalar(a,x) => Nil
    case MatrixRepmat(a,i,j) => Nil
    case MatrixClone(a) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case MatrixMultiply(a,b) => Nil
    case MatrixTimes(a,b) => Nil
    case MatrixTimesVector(a,v) => Nil
    case MatrixTimesScalar(a,x) => Nil
    case MatrixRepmat(a,i,j) => Nil
    case MatrixClone(a) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case MatrixMultiply(a,b) => Nil
    case MatrixTimes(a,b) => Nil
    case MatrixTimesVector(a,v) => Nil
    case MatrixTimesScalar(a,x) => Nil
    case MatrixRepmat(a,i,j) => Nil
    case MatrixClone(a) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case MatrixMultiply(a,b) => Nil
    case MatrixTimes(a,b) => Nil
    case MatrixTimesVector(a,v) => Nil
    case MatrixTimesScalar(a,x) => Nil
    case MatrixRepmat(a,i,j) => syms(a)
    case MatrixClone(a) => syms(a)
    case _ => super.copySyms(e)
  } 
}

/**
 *  Optimizations for composite MatrixOps operations.
 */

trait MatrixOpsExpOpt extends MatrixOpsExp {
  this: MatrixImplOps with OptiMLExp =>

  override def matrix_plus[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = (x, y) match {
    // (AB + AD) == A(B + D)
    case (Def(MatrixTimes(a, b)), Def(MatrixTimes(c, d))) if (a == c) => MatrixTimes[A](a.asInstanceOf[Exp[Matrix[A]]], MatrixPlus[A](b.asInstanceOf[Exp[Matrix[A]]],d.asInstanceOf[Exp[Matrix[A]]]))
    // ...
    case _ => super.matrix_plus(x, y)
  }

  override def matrix_equals[A:Manifest](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = (x, y) match {
    case (a,b) if (a == b) => unit(true) // same symbol
    case _ => super.matrix_equals(x,y)
  }

  override def matrix_times[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = (x, y) match {
    // X^-1*X = X*X^-1 = I (if X is non-singular)
    //case (Def(MatrixInverse(a)), b) if (a == b) => MatrixObjectIdentity(a.numRows).asInstanceOf[Exp[Matrix[A]]]
    //case (b, Def(MatrixInverse(a))) if (a == b) => MatrixObjectIdentity(a.numRows).asInstanceOf[Exp[Matrix[A]]]

    // X*I = I*X = X
    case (Def(MatrixObjectIdentity(a)), b) if (a == b) => a.asInstanceOf[Exp[Matrix[A]]]
    case (a, Def(MatrixObjectIdentity(b))) if (a == b) => a.asInstanceOf[Exp[Matrix[A]]]

    // else
    case _ => super.matrix_times(x, y)
  }

  override def matrix_numrows[A:Manifest](x: Exp[Matrix[A]]) = x match {
    case Def(s@Reflect(MatrixObjectNew(rows,cols), u, es)) if context.contains(s) => rows // only if not modified! // TODO: check writes
    case Def(MatrixObjectNew(rows,cols)) => rows
    case Def(TrainingSetObjectFromMat(x,y)) => matrix_numrows(x) // TODO: move to TrainingSetOpsExpOpt ?
    case _ => super.matrix_numrows(x)
  }

  override def matrix_numcols[A:Manifest](x: Exp[Matrix[A]]) = x match {
    case Def(s@Reflect(MatrixObjectNew(rows,cols), u, es)) if context.contains(s) => cols // only if not modified! // TODO: check writes
    case Def(MatrixObjectNew(rows,cols)) => cols
    case Def(TrainingSetObjectFromMat(x,y)) => matrix_numcols(x) // TODO: move to TrainingSetOpsExpOpt ?
    case _ => super.matrix_numcols(x)
  }

  override def matrix_dcsize[A:Manifest](x: Exp[Matrix[A]]) = x match {
    case Def(e: DeliteOpMap[_,_,_]) => e.size
    case Def(e: DeliteOpZipWith[_,_,_,_]) => e.size
    case _ => super.matrix_dcsize(x)
  }

//  override def matrix_inverse[A:Manifest](x: Exp[Matrix[A]]) = x match {
//    (X^-1)^-1 = X (if X is non-singular)
//    case (Def(MatrixInverse(a))) => a.asInstanceOf[Exp[Matrix[A]]]
//    case _ => super.matrix_inverse(x)
//  }

//  override def matrix_transpose[A:Manifest](x: Exp[Matrix[A]]) = x match {
//    // (X^T)^T = X
//    case (Def(MatrixTranspose(a))) => a.asInstanceOf[Exp[Matrix[A]]]
//    case _ => super.matrix_transpose(x)
//  }


}


trait ScalaGenMatrixOps extends ScalaGenBase {
  val IR: MatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case m@SymmetricMatrixObjectNew(n) => emitValDef(sym, "new " + remap(m.mM) + "(" + quote(n) + ")")
    case m@MatrixObjectNew(numRows, numCols) => emitValDef(sym, "new " + remap(m.mM) + "(" + quote(numRows) + "," + quote(numCols) + ")")
    case MatrixVView(x,start,stride,length,isRow) => emitValDef(sym, quote(x) + ".vview(" + quote(start) + "," + quote(stride) + "," + quote(length) + "," + quote(isRow) + ")")
    //case MatrixApply(x,i,j) => emitValDef(sym, quote(x) + "(" + quote(i) + ", " + quote(j) + ")")
    case MatrixGetRow(x,i) => emitValDef(sym, quote(x) + ".getRow(" + quote(i) + ")")
    case MatrixGetCol(x,j) => emitValDef(sym, quote(x) + ".getCol(" + quote(j) + ")")
    case MatrixNumRows(x)  => emitValDef(sym, quote(x) + ".numRows")
    case MatrixNumCols(x)  => emitValDef(sym, quote(x) + ".numCols")
    case MatrixClone(x) => emitValDef(sym, quote(x) + ".cloneL")
    case MatrixUpdate(x,i,j,y)  => emitValDef(sym, quote(x) + "(" + quote(i) + ", " + quote(j) + ") = " + quote(y))
    case MatrixInsertRow(x,pos,y)  => emitValDef(sym, quote(x) + ".insertRow(" + quote(pos) + "," + quote(y) + ")")
    case MatrixInsertAllRows(x,pos,y) => emitValDef(sym, quote(x) + ".insertAllRows(" + quote(pos) + "," + quote(y) + ")")
    case MatrixInsertCol(x,pos,y) => emitValDef(sym, quote(x) + ".insertCol(" + quote(pos) + "," + quote(y) + ")")
    case MatrixInsertAllCols(x,pos,y) => emitValDef(sym, quote(x) + ".insertAllCols(" + quote(pos) + "," + quote(y) + ")")
    case MatrixRemoveRows(x,pos,len) => emitValDef(sym, quote(x) + ".removeRows(" + quote(pos) + "," + quote(len) + ")")
    case MatrixRemoveCols(x,pos,len) => emitValDef(sym, quote(x) + ".removeCols(" + quote(pos) + "," + quote(len) + ")")
    case MatrixRawData(x) => emitValDef(sym, quote(getBlockResult(x)) + ".data")  // getBlockResult necessary?? should it be everywhere?
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenMatrixOps extends CudaGenBase with CudaGenDataStruct {
  val IR: MatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

    /* CUBLAS calls */
    /*
    case MatrixMultiplyBLAS(x,y) =>
      val callStream = "cublasSetKernelStream(stream);"
      val callKernel = if(remap(x.Type.typeArguments(0)) == "double")
        "cublasDgemm('n','n',%s.numCols,%s.numRows,%s.numRows,1.0,%s.data,%s.numCols,%s.data,%s.numCols,0.0,%s.data,%s.numCols);".format(quote(y),quote(x),quote(y),quote(y),quote(y),quote(x),quote(x),quote(sym),quote(sym))
      else if(remap(x.Type.typeArguments(0)) == "float")
        "cublasSgemm('n','n',%s.numCols,%s.numRows,%s.numRows,1.0,%s.data,%s.numCols,%s.data,%s.numCols,0.0,%s.data,%s.numCols);".format(quote(y),quote(x),quote(y),quote(y),quote(y),quote(x),quote(x),quote(sym),quote(sym))
      else
        throw new RuntimeException("CudaGen: Not GPUable (Type %s is not supported for MatrixMulitply CUBLAS library)".format(remap(x.Type.typeArguments(0))))
      emitMatrixAlloc(sym,"%s.numRows".format(quote(x)),"%s.numCols".format(quote(y)),false)
      emitLibCall(sym,List(callStream,callKernel))
    
    case MatrixTimesVectorBLAS(x,y) =>
      val callStream = "cublasSetKernelStream(stream);"
      val callKernel = if(remap(x.Type.typeArguments(0)) == "double")
        "cublasDgemv('t', %s.numCols, %s.numRows, 1.0, %s.data, %s.numCols, %s.data, 1, 0.0, %s.data, 1);".format(quote(x),quote(x),quote(x),quote(x),quote(y),quote(sym))
      else if(remap(x.Type.typeArguments(0)) == "float")
        "cublasSgemv('t', %s.numCols, %s.numRows, 1.0, %s.data, %s.numCols, %s.data, 1, 0.0, %s.data, 1);".format(quote(x),quote(x),quote(x),quote(x),quote(y),quote(sym))
      else
        throw new RuntimeException("CudaGen: Not GPUable (Type %s is not supported for Matrix*Vector CUBLAS library)".format(remap(x.Type.typeArguments(0))))
      emitVectorAlloc(sym,"%s.numRows".format(quote(x)),"false",false)
      emitLibCall(sym,List(callStream,callKernel))
    */

    case MatrixObjectNew(numRows,numCols) =>
      stream.println(addTab()+"%s *devPtr_%s;".format(remap(sym.Type.typeArguments(0)),quote(sym)))
      stream.println(addTab()+"DeliteCudaMalloc((void**)&devPtr_%s,%s*%s*sizeof(%s));".format(quote(sym),quote(numRows),quote(numCols),remap(sym.Type.typeArguments(0))))
      stream.println("%s *%s_ptr = new %s(%s,%s,devPtr_%s);".format(remap(sym.Type),quote(sym),remap(sym.Type),quote(numRows),quote(numCols),quote(sym)))
      stream.println("%s %s = *(%s_ptr);".format(remap(sym.Type),quote(sym),quote(sym)))
      //stream.println("%s.numRows = %s;".format(quote(sym),quote(numRows)))
      //stream.println("%s.numCols = %s;".format(quote(sym),quote(numCols)))
      //stream.println("%s.data = %s_data;".format(quote(sym),quote(sym)))

    case MatrixClone(x) =>
      stream.println(addTab()+"%s *devPtr_%s;".format(remap(sym.Type.typeArguments(0)),quote(sym)))
      stream.println(addTab()+"DeliteCudaMalloc((void**)&devPtr_%s,%s.numRows*%s.numCols*sizeof(%s));".format(quote(sym),quote(x),quote(x),remap(sym.Type.typeArguments(0))))
      stream.println("%s *%s_ptr = new %s(%s.num,%s,%s_devPtr);".format(remap(sym.Type),quote(sym),remap(sym.Type),quote(x),quote(x),quote(sym)))
      stream.println("%s %s = *(%s_ptr);".format(remap(sym.Type),quote(sym),quote(sym)))

	  /* The ops that call through to the underlying data structure */
    //case MatrixDCApply(x,i) =>
    //  emitValDef(sym, "%s.dcApply(%s)".format(quote(x),quote(i)))
    //case MatrixApply(x,i,j) =>
    //  emitValDef(sym, "%s.apply(%s,%s)".format(quote(x),quote(i),quote(j)))
    case MatrixUpdate(x,i,j,y)  =>
      stream.println(addTab() + "%s.update(%s,%s,%s);".format(quote(x),quote(i),quote(j),quote(y)))
    case MatrixNumRows(x)  =>
      emitValDef(sym, quote(x) + ".numRows")
    case MatrixNumCols(x)  =>
      emitValDef(sym, quote(x) + ".numCols")

    /* Specialized CUDA code generations for DeliteOpSingleTasks */
    case MatrixUpdateRow(x, row, y) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->length".format(quote(y)))
      stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(y)))
      tabWidth += 1
      stream.println(addTab()+"%s.update(%s,%s,%s.apply(%s));".format(quote(x),quote(row),currDimStr,quote(y),currDimStr))
      tabWidth -= 1
      stream.println(addTab()+"}")
      currDim -= 1

	  /*
    case MatrixObjectDiag(w, vals) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s * %s".format(quote(w),quote(w)))
      stream.println(addTab()+"if( %s < %s*%s ) {".format(currDimStr,quote(w),quote(w)))
      tabWidth += 1
      stream.println(addTab()+"int i = %s / %s;".format(currDimStr,quote(w)))
      stream.println(addTab()+"int j = " + currDimStr + " % "  + quote(w) + ";")
      stream.println(addTab()+"%s.update(i,j,0);".format(quote(sym)))
      stream.println(addTab()+"if(i == j) {")
      tabWidth += 1
      stream.println(addTab()+"%s.update(i, j, %s.apply(i));".format(quote(sym),quote(vals)))
      tabWidth -= 1
      stream.println(addTab()+"}")
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s".format(quote(w)),"%s".format(quote(w)),false)
      currDim -= 1
*/
    case MatrixTranspose(x) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->size()".format(quote(x)))
      stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(x)))
      tabWidth += 1
      stream.println(addTab()+"int i = %s / %s.numCols;".format(currDimStr,quote(x)))
      stream.println(addTab()+"int j = " + currDimStr + " % " + "%s.numCols;".format(quote(x)))
      stream.println(addTab()+"%s.update(j, i, %s.apply(i,j));".format(quote(sym),quote(x)))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s.numCols".format(quote(x)),"%s.numRows".format(quote(x)),false)
      currDim -= 1

    case MatrixSumCol(x) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->numCols".format(quote(x)))
      stream.println(addTab()+"if( %s < %s.numCols ) {".format(currDimStr,quote(x)))
      tabWidth += 1
      stream.println(addTab()+"%s reducVal = 0;".format(remap(x.Type.typeArguments(0))))
      stream.println(addTab()+"for(int i=0; i<%s.numRows; i++) {".format(quote(x)))
      tabWidth += 1
      stream.println(addTab()+"reducVal += %s.apply(i,%s);".format(quote(x),currDimStr))
      tabWidth -= 1
      stream.println(addTab()+"}")
      stream.println(addTab()+"%s.update(%s,reducVal);".format(quote(sym),currDimStr))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitVectorAlloc(sym,"%s.numCols".format(quote(x)),"true",false)
      currDim -= 1

    /*
    case m@MatrixSigmoidF(x) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->size()".format(quote(x)))
      stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(x)))
      tabWidth += 1
      val (sigmoidFunc,freeVars) = emitDevFunc(m.func,List(m.v))
      stream.println(addTab()+"int i = %s / %s.numCols;".format(currDimStr,quote(x)))
      stream.println(addTab()+"int j = " + currDimStr + " % " + "%s.numCols;".format(quote(x)))
      if(freeVars.length == 0)
        stream.println(addTab()+"%s.update(i,j,%s(%s.apply(i,j)));".format(quote(sym),sigmoidFunc,quote(x)))
      else
        stream.println(addTab()+"%s.update(i,j,%s(%s.apply(i,j)),%s);".format(quote(sym),sigmoidFunc,quote(x),freeVars.map(quote).mkString(",")))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s.numRows".format(quote(x)),"%s.numCols".format(quote(x)),false)
      currDim -= 1

	  case m@MatrixSigmoidFBLAS(x) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->size()".format(quote(x)))
      stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(x)))
      tabWidth += 1
	  stream.println(addTab()+"float %s_result = 1.0f/(1.0f + exp(-1*%s.dcApply(%s)));".format(quote(sym),quote(x),currDimStr)) 
      stream.println(addTab()+"%s.dcUpdate(%s,%s_result);".format(quote(sym),currDimStr,quote(sym)))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s.numRows".format(quote(x)),"%s.numCols".format(quote(x)),false)
      currDim -= 1
  */
  /*
	case MatrixPlusEquals(x,y) =>
		throw new GenerationFailedException("CudaGen: No dimension specified for this kernel.")
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->numCols".format(quote(x)))
      stream.println(addTab()+"if( %s < %s.numCols ) {".format(currDimStr,quote(x)))
      tabWidth += 1
      stream.println(addTab()+"for(int i=0; i<%s.numRows; i++) {".format(quote(x)))
      tabWidth += 1
      stream.println(addTab()+"%s.update(i,%s,%s.apply(i,%s)+%s.apply(i,%s));".format(quote(x),currDimStr,quote(x),currDimStr,quote(y),currDimStr))
      tabWidth -= 1
      stream.println(addTab()+"}")
      tabWidth -= 1
      stream.println(addTab()+"}")
      currDim -= 1

    case MatrixPlusEquals(x,y) if(useLocalVar) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength(quote(x)+"->size()")
      val varX = if(hasLocalVar(x,currDimStr)) getLocalVar(x,currDimStr)
                 else "NOT FOUND X"
      val varY = if(hasLocalVar(y,currDimStr)) getLocalVar(y,currDimStr)
                 else "NOT FOUND Y"
      stream.println(addTab()+"%s = %s + %s;".format(varX,varX,varY))
      currDim -= 1
	*/
  /*
    case MatrixGetRow(x,i) =>
      if(kernelSymbol != sym) {
        //stream.println(addTab()+"%s %s;".format(remap(sym.Type),quote(sym)))
        stream.println(addTab()+"%s.length = %s.numCols;".format(quote(sym),quote(x)))
        stream.println(addTab()+"%s.isRow = true;".format(quote(sym)))
        stream.println(addTab()+"%s.data = %s.data+%s*%s.numCols;".format(quote(sym),quote(x),quote(i),quote(x)))
        emitVectorAlloc(sym,"%s.numCols".format(quote(x)),"true",false,"%s->data".format(quote(x)))
      }
  */

    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenMatrixOps extends OpenCLGenBase with OpenCLGenDataStruct {
  val IR: MatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    /* Object creation */
    //TODO: Assertion to check if this is generated only within the alloc function
    case MatrixObjectNew(numRows,numCols) =>
      val elemType = sym.Type.typeArguments(0)
      assert(isPrimitiveType(elemType))
      stream.println(addTab()+"cl_mem %s_data = DeliteOpenCLMalloc(%s*%s*sizeof(%s));".format(quote(sym),quote(numRows),quote(numCols),remap(elemType)))
      stream.println(addTab()+"%s *%s_ptr = new %s(%s,%s,%s_data);".format(remap(sym.Type),quote(sym),remap(sym.Type),quote(numRows),quote(numCols),quote(sym)))
    
    /* Member functions/fields on matrix datastructure */
    //case MatrixDCApply(x,i) =>
    //  emitValDef(sym, "%s_dcApply(%s,%s)".format(remap(x.Type),quote(x),quote(i)))
    case MatrixUpdate(x,i,j,y)  =>
      stream.println(addTab() + "%s_dcUpdate(%s,%s*%s.numCols+%s,%s);".format(remap(x.Type),quote(x),quote(i),quote(x),quote(j),quote(y)))
    case MatrixNumRows(x)  =>
      emitValDef(sym, quote(x) + ".numRows")
    case MatrixNumCols(x)  =>
      emitValDef(sym, quote(x) + ".numCols")
    case MatrixGetRow(x,i) =>
      stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
      stream.println("%s.isRow = true;".format(quote(sym)))
      stream.println("%s.length = %s.numCols;".format(quote(sym),quote(x)))
      stream.println("%s.data = %s.data + %s.numCols*%s;".format(quote(sym),quote(x),quote(x),quote(i)))

    /* BLAS Implementations */
    /*
    case MatrixMultiplyBLAS(x,y) =>
      //val setQueue = "clblasSetQueue(command_queue);"
      val callKernel = if(remap(x.Type.typeArguments(0)) == "double")
        "clblasDgemm('n','n',%s.numCols,%s.numRows,%s.numRows,1.0,%s.data,%s.numCols,%s.data,%s.numCols,0.0,%s.data,%s.numCols);".format(quote(y),quote(x),quote(y),quote(y),quote(y),quote(x),quote(x),quote(sym),quote(sym))
      else if(remap(x.Type.typeArguments(0)) == "float")
        "clblasSgemm('n','n',%s.numCols,%s.numRows,%s.numRows,1.0f,%s.data,%s.numCols,%s.data,%s.numCols,0.0f,%s.data,%s.numCols);".format(quote(y),quote(x),quote(y),quote(y),quote(y),quote(x),quote(x),quote(sym),quote(sym))
      else
        throw new RuntimeException("CudaGen: Not GPUable (Type %s is not supported for MatrixMulitply CUBLAS library)".format(remap(x.Type.typeArguments(0))))
      emitMatrixAlloc(sym,"%s.numRows".format(quote(x)),"%s.numCols".format(quote(y)),false)
      emitLibCall(sym,List(callKernel))
    
    case MatrixTimesVectorBLAS(x,y) =>
      //val setQueue = "clblasSetQueue(command_queue);"
      val callKernel = if(remap(x.Type.typeArguments(0)) == "double")
        "clblasDgemv('t', %s.numCols, %s.numRows, 1.0, %s.data, %s.numCols, %s.data, 1, 0.0, %s.data, 1);".format(quote(x),quote(x),quote(x),quote(x),quote(y),quote(sym))
      else if(remap(x.Type.typeArguments(0)) == "float")
        "clblasSgemv('t', %s.numCols, %s.numRows, 1.0, %s.data, %s.numCols, %s.data, 1, 0.0, %s.data, 1);".format(quote(x),quote(x),quote(x),quote(x),quote(y),quote(sym))
      else
        throw new RuntimeException("CudaGen: Not GPUable (Type %s is not supported for Matrix*Vector CUBLAS library)".format(remap(x.Type.typeArguments(0))))
      emitVectorAlloc(sym,"%s.numRows".format(quote(x)),"false",false)
      emitLibCall(sym,List(callKernel))
    */
    /* Specialized OpenCL code generations for DeliteOpSingleTasks */
      /*
    case MatrixUpdateRow(x, row, y) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->length".format(quote(y)))
      stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(y)))
      tabWidth += 1
      stream.println(addTab()+"%s.update(%s,%s,%s.apply(%s));".format(quote(x),quote(row),currDimStr,quote(y),currDimStr))
      tabWidth -= 1
      stream.println(addTab()+"}")
      currDim -= 1
    */
    case MatrixTranspose(x) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->dcSize()".format(quote(x)))
      stream.println(addTab()+"if( %s < %s_dcSize(%s) ) {".format(currDimStr,remap(x.Type),quote(x)))
      tabWidth += 1
      stream.println(addTab()+"int i = %s / %s.numCols;".format(currDimStr,quote(x)))
      stream.println(addTab()+"int j = " + currDimStr + " % " + "%s.numCols;".format(quote(x)))
      stream.println(addTab()+"%s_dcUpdate(%s, j*%s.numCols+i, %s_dcApply(%s,i*%s.numCols+j));".format(remap(sym.Type),quote(sym),quote(sym),remap(x.Type),quote(x),quote(x)))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s.numCols".format(quote(x)),"%s.numRows".format(quote(x)),false)
      currDim -= 1

    case MatrixSumCol(x) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->numCols".format(quote(x)))
      stream.println(addTab()+"if( %s < %s.numCols ) {".format(currDimStr,quote(x)))
      tabWidth += 1
      stream.println(addTab()+"%s reducVal = 0;".format(remap(x.Type.typeArguments(0))))
      stream.println(addTab()+"for(int i=0; i<%s.numRows; i++) {".format(quote(x)))
      tabWidth += 1
      stream.println(addTab()+"reducVal += %s_dcApply(%s,i*%s.numCols+%s);".format(remap(x.Type),quote(x),quote(x),currDimStr))
      tabWidth -= 1
      stream.println(addTab()+"}")
      stream.println(addTab()+"%s_dcUpdate(%s,%s,reducVal);".format(remap(sym.Type),quote(sym),currDimStr))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitVectorAlloc(sym,"%s.numCols".format(quote(x)),"true",false)
      currDim -= 1
    /*
    case m@MatrixSigmoidFBLAS(x) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->dcSize()".format(quote(x)))
      stream.println(addTab()+"if( %s < %s_dcSize(%s) ) {".format(currDimStr,remap(x.Type),quote(x)))
      tabWidth += 1
      stream.println(addTab()+"float %s_result = 1.0f/(1.0f + exp(-1*%s_dcApply(%s,%s)));".format(quote(sym),remap(sym.Type),quote(x),currDimStr)) 
      stream.println(addTab()+"%s_dcUpdate(%s,%s,%s_result);".format(remap(sym.Type),quote(sym),currDimStr,quote(sym)))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s.numRows".format(quote(x)),"%s.numCols".format(quote(x)),false)
      currDim -= 1
    */
    case _ => super.emitNode(sym,rhs)
  }
}

trait CGenMatrixOps extends CGenBase {
  val IR: MatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

    case MatrixObjectNew(numRows,numCols) =>
      stream.println("%s *%s_data = malloc(sizeof(%s)*%s*%s);".format(remap(sym.Type.typeArguments(0)),quote(sym),remap(sym.Type.typeArguments(0)),quote(numRows),quote(numCols)))
      stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
      stream.println("%s.numRows = %s;".format(quote(sym),quote(numRows)))
      stream.println("%s.numCols = %s;".format(quote(sym),quote(numCols)))
      stream.println("%s.data = %s_data;".format(quote(sym),quote(sym)))
    case MatrixGetRow(x,i) =>
      stream.println("Vector<%s> %s;".format(remap(sym.Type.typeArguments(0)),quote(sym)))
      stream.println("%s.len = %s.numCols;".format(quote(sym),quote(x)))
      stream.println("%s.isRow = true;".format(quote(sym)))
      stream.println("%s.data = %s.data+%s.numCols*%s;".format(quote(sym),quote(x),quote(x),quote(i)))
    //case MatrixDCApply(x,i) =>
    //  emitValDef(sym, "%s.apply(%s)".format(quote(x),quote(i)))
    //case MatrixApply(x,i,j) =>
    //  emitValDef(sym, "%s.apply(%s,%s)".format(quote(x),quote(i),quote(j)))
    case MatrixUpdate(x,i,j,y)  =>
      stream.println("%s.update(%s,%s,%s);".format(quote(x),quote(i),quote(j),quote(y)))
    case MatrixNumRows(x)  =>
      emitValDef(sym, quote(x) + ".numRows")
    case MatrixNumCols(x)  =>
      emitValDef(sym, quote(x) + ".numCols")
    case MatrixInsertRow(x, pos, y)  =>
      stream.println("%s.data = (%s *)realloc(%s.data,sizeof(%s)*(%s.numRows+1)*%s.numCols);".format(quote(x),remap(x.Type.typeArguments(0)),quote(x),remap(x.Type.typeArguments(0)),quote(x),quote(x)))
      stream.println("memcpy(%s.data+%s*%s.numCols,%s.data,sizeof(%s)*%s.length);".format(quote(x),quote(pos),quote(x),quote(y),remap(x.Type.typeArguments(0)),quote(y)))
      stream.println("%s.numRows += 1;".format(quote(x)))
      stream.println("%s %s = %s;".format(remap(sym.Type),quote(sym),quote(x)))
    case _ => super.emitNode(sym, rhs)
  }
}
