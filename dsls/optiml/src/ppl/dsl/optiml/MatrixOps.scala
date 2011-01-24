package ppl.dsl.optiml

import datastruct.CudaGenDataStruct
import datastruct.scala.{MatrixImpl, VectorImpl, Vector, Matrix}
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.internal.{GenerationFailedException, CGenBase, CudaGenBase, ScalaGenBase}
import ppl.delite.framework.Config

trait MatrixOps extends DSLType with Variables {
  this: OptiML =>

  object Matrix {
    def apply[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_new(numRows, numCols)
    def apply[A:Manifest](xs: Rep[Vector[Vector[A]]]): Rep[Matrix[A]] = matrix_obj_fromvec(xs)
//    def apply[A:Manifest](xs: Rep[Vector[A]]*): Rep[Matrix[A]] = {
//      // TODO: how do we get the manifest?
//      implicit val mA = manifest[Rep[Vector[A]]]
//      val xs2 = unit(xs.toList)
//      matrix_obj_fromseq(xs2)
//    }
    def diag[A:Manifest](w: Rep[Int], vals: Rep[Vector[A]]) = matrix_obj_diag(w, vals)
    def identity(w: Rep[Int]) = matrix_obj_identity(w)
    def zeros(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_zeros(numRows, numCols)
    def zerosf(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_zerosf(numRows, numCols)
    def ones(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_ones(numRows, numCols)
    def onesf(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_onesf(numRows, numCols)
    def rand(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_rand(numRows, numCols)
    def randf(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_randf(numRows, numCols)
    def randn(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_randn(numRows, numCols)
    def randnf(numRows: Rep[Int], numCols: Rep[Int]) = matrix_obj_randnf(numRows, numCols)
  }

  implicit def repMatToMatOps[A:Manifest](x: Rep[Matrix[A]]) = new matRepCls(x)
  implicit def varToMatOps[A:Manifest](x: Var[Matrix[A]]): matRepCls[A]

  // could convert to infix, but apply doesn't work with it anyways yet
  class matRepCls[A:Manifest](x: Rep[Matrix[A]]) {
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
    def sliceRows(begin: Rep[Int], end: Rep[Int]) = matrix_slicerows(x,begin,end)
    def numRows = matrix_numrows(x)
    def numCols = matrix_numcols(x)

    // general
    def t = matrix_transpose(x)
    // TODO: implicit won't trigger
    //override def clone = matrix_clone(x)
    def cloneL() = matrix_clone(x)
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
    def +=(y: Rep[Matrix[A]])(implicit a: Arith[A]) = matrix_plusequals(x,y)
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
    def min(implicit o: Ordering[A]) = matrix_min(x)
    def minRow(implicit a: Arith[A], o: Ordering[A]) = matrix_minrow(x)
    def max(implicit o: Ordering[A]) = matrix_max(x)
    def maxRow(implicit a: Arith[A], o: Ordering[A]) = matrix_maxrow(x)
    def :>(y: Rep[Matrix[A]])(implicit o: Ordering[A]) = zip(y) { (a,b) => a > b }
    def :<(y: Rep[Matrix[A]])(implicit o: Ordering[A]) = zip(y) { (a,b) => a < b }

    // bulk operations
    def map[B:Manifest](f: Rep[A] => Rep[B]) = matrix_map(x,f)
    def mmap(f: Rep[A] => Rep[A]) = matrix_mmap(x,f)
    def mapRows[B:Manifest](f: Rep[Vector[A]] => Rep[Vector[B]]) = matrix_maprows(x,f)
    def mapRows[B:Manifest](f: Rep[Vector[A]] => Rep[B], isRow: Rep[Boolean] = true) = matrix_maprowstovec(x,f,isRow)
    def foreach(block: Rep[A] => Rep[Unit]) = matrix_foreach(x, block)
    def foreachRow(block: Rep[Vector[A]] => Rep[Unit]) = matrix_foreachrow(x, block)
    def zip[B:Manifest,R:Manifest](y: Rep[Matrix[B]])(f: (Rep[A],Rep[B]) => Rep[R]) = matrix_zipwith(x,y,f)
    def reduceRows(f: (Rep[Vector[A]],Rep[Vector[A]]) => Rep[Vector[A]]) = matrix_reducerows(x,f)
    def filterRows(pred: Rep[Vector[A]] => Rep[Boolean]) = matrix_filterrows(x,pred)
  }

  // special case overrides
  def infix_:>(x: Rep[Matrix[Float]], y: Rep[Matrix[Float]]) = x.zip(y) { (a,b) => if (a > b) 1f else 0f }
  def infix_:>(x: Rep[Matrix[Double]], y: Rep[Matrix[Double]])(implicit o: Overloaded1) = x.zip(y) { (a,b) => if (a > b) 1. else 0. }
  def infix_:>(x: Rep[Matrix[Int]], y: Rep[Matrix[Int]])(implicit o: Overloaded2) = x.zip(y) { (a,b) => if (a > b) 1 else 0 }
  def infix_:<(x: Rep[Matrix[Float]], y: Rep[Matrix[Float]]) = x.zip(y) { (a,b) => if (a > b) 1f else 0f }
  def infix_:<(x: Rep[Matrix[Double]], y: Rep[Matrix[Double]])(implicit o: Overloaded1) = x.zip(y) { (a,b) => if (a > b) 1. else 0. }
  def infix_:<(x: Rep[Matrix[Int]], y: Rep[Matrix[Int]])(implicit o: Overloaded2) = x.zip(y) { (a,b) => if (a > b) 1. else 0. }

  // object defs
  def matrix_obj_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[A]]
  def matrix_obj_fromseq[A:Manifest](xs: Rep[Seq[Rep[Vector[A]]]]): Rep[Matrix[A]]
  def matrix_obj_fromvec[A:Manifest](xs: Rep[Vector[Vector[A]]]): Rep[Matrix[A]]
  def matrix_obj_diag[A:Manifest](w: Rep[Int], vals: Rep[Vector[A]]): Rep[Matrix[A]]
  def matrix_obj_identity(w: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_zeros(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_zerosf(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_ones(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_onesf(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_rand(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_randf(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]
  def matrix_obj_randn(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Double]]
  def matrix_obj_randnf(numRows: Rep[Int], numCols: Rep[Int]): Rep[Matrix[Float]]

  // class defs
  def matrix_apply[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int], j: Rep[Int]): Rep[A]
  def matrix_vview[A:Manifest](x: Rep[Matrix[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean]): Rep[Vector[A]]
  def matrix_getrow[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int]): Rep[Vector[A]]
  def matrix_getcol[A:Manifest](x: Rep[Matrix[A]], i: Rep[Int]): Rep[Vector[A]]
  def matrix_slicerows[A:Manifest](x: Rep[Matrix[A]], begin: Rep[Int], end: Rep[Int]): Rep[Matrix[A]]
  def matrix_numrows[A:Manifest](x: Rep[Matrix[A]]): Rep[Int]
  def matrix_numcols[A:Manifest](x: Rep[Matrix[A]]): Rep[Int]

  def matrix_transpose[A:Manifest](x: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_clone[A:Manifest](x: Rep[Matrix[A]]): Rep[Matrix[A]]
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
  def matrix_plusequals[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Matrix[A]]): Rep[Matrix[A]]
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

  def matrix_min[A:Manifest:Ordering](x: Rep[Matrix[A]]): Rep[A]
  def matrix_minrow[A:Manifest:Arith:Ordering](x: Rep[Matrix[A]]): Rep[Vector[A]]
  def matrix_max[A:Manifest:Ordering](x: Rep[Matrix[A]]): Rep[A]
  def matrix_maxrow[A:Manifest:Arith:Ordering](x: Rep[Matrix[A]]): Rep[Vector[A]]

  def matrix_map[A:Manifest,B:Manifest](x: Rep[Matrix[A]], f: Rep[A] => Rep[B]): Rep[Matrix[B]]
  def matrix_mmap[A:Manifest](x: Rep[Matrix[A]], f: Rep[A] => Rep[A]): Rep[Matrix[A]]
  def matrix_maprows[A:Manifest,B:Manifest](x: Rep[Matrix[A]], f: Rep[Vector[A]] => Rep[Vector[B]]): Rep[Matrix[B]]
  def matrix_maprowstovec[A:Manifest,B:Manifest](x: Rep[Matrix[A]], f: Rep[Vector[A]] => Rep[B], isRow: Rep[Boolean]): Rep[Vector[B]]
  def matrix_foreach[A:Manifest](x: Rep[Matrix[A]], block: Rep[A] => Rep[Unit]): Rep[Unit]
  def matrix_foreachrow[A:Manifest](x: Rep[Matrix[A]], block: Rep[Vector[A]] => Rep[Unit]): Rep[Unit]
  def matrix_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Rep[Matrix[A]], y: Rep[Matrix[B]], f: (Rep[A],Rep[B]) => Rep[R]): Rep[Matrix[R]]
  def matrix_reducerows[A:Manifest](x: Rep[Matrix[A]], f: (Rep[Vector[A]],Rep[Vector[A]]) => Rep[Vector[A]]): Rep[Vector[A]]
  def matrix_filterrows[A:Manifest](x: Rep[Matrix[A]], pred: Rep[Vector[A]] => Rep[Boolean]): Rep[Matrix[A]]
}


trait MatrixOpsExp extends MatrixOps with VariablesExp {
  this: MatrixImplOps with OptiMLExp  =>

  implicit def varToMatOps[A:Manifest](x: Var[Matrix[A]]) = new matRepCls(readVar(x))


  //////////////////////////////////////////////////
  // implemented via method on real data structure

  case class MatrixObjectNew[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) extends Def[Matrix[A]] {
     val mM = manifest[MatrixImpl[A]]
  }
  case class MatrixApply[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int]) extends Def[A]
  case class MatrixVView[A:Manifest](x: Exp[Matrix[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean]) extends Def[Vector[A]]
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

  case class MatrixObjectFromSeq[A:Manifest](xs: Exp[Seq[Rep[Vector[A]]]])
    extends DeliteOpSingleTask(reifyEffects(matrix_obj_fromseq_impl(xs)))

  case class MatrixObjectFromVec[A:Manifest](xs: Exp[Vector[Vector[A]]])
    extends DeliteOpSingleTask(reifyEffects(matrix_obj_fromvec_impl(xs)))

  case class MatrixObjectDiag[A:Manifest](w: Exp[Int], vals: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffects(matrix_obj_diag_impl(w, vals)))

  case class MatrixObjectIdentity(w: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(matrix_obj_identity_impl(w)))

  case class MatrixObjectZeros(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(matrix_obj_zeros_impl(numRows, numCols)))

  case class MatrixObjectZerosF(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(matrix_obj_zerosf_impl(numRows, numCols)))

  case class MatrixObjectOnes(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(matrix_obj_ones_impl(numRows, numCols)))

  case class MatrixObjectOnesF(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(matrix_obj_onesf_impl(numRows, numCols)))

  case class MatrixObjectRand(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(matrix_obj_rand_impl(numRows, numCols)))

  case class MatrixObjectRandF(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(matrix_obj_randf_impl(numRows, numCols)))

  case class MatrixObjectRandn(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(matrix_obj_randn_impl(numRows, numCols)))

  case class MatrixObjectRandnF(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(matrix_obj_randnf_impl(numRows, numCols)))

  case class MatrixGetRow[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(matrix_getrow_impl(x,i)))

  case class MatrixGetCol[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(matrix_getcol_impl(x,i)))

  case class MatrixSliceRows[A:Manifest](x: Exp[Matrix[A]], begin: Exp[Int], end: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(matrix_slicerows_impl(x,begin,end)))

  case class MatrixUpdateRow[A:Manifest](x: Exp[Matrix[A]], row: Exp[Int], y: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffects(matrix_updaterow_impl(x,row,y)))

  case class MatrixPPrint[A:Manifest](x: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffects(matrix_pprint_impl[A](x)))

  case class MatrixRepmat[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(matrix_repmat_impl[A](x,i,j)))

  case class MatrixInverse[A](x: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double])
    extends DeliteOpSingleTask(reifyEffects(matrix_inverse_impl[A](x)))

  case class MatrixTranspose[A:Manifest](x: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffects(matrix_transpose_impl(x)))

  case class MatrixMinRow[A:Manifest:Arith:Ordering](x: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffects(matrix_minrow_impl(x)))

  case class MatrixMaxRow[A:Manifest:Arith:Ordering](x: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffects(matrix_maxrow_impl(x)))

  case class MatrixMapRows[A:Manifest,B:Manifest](x: Exp[Matrix[A]], f: Exp[Vector[A]] => Exp[Vector[B]])
    extends DeliteOpSingleTask(reifyEffects(matrix_maprows_impl(x,f)))

  case class MatrixForeachRow[A:Manifest](x: Exp[Matrix[A]], f: Exp[Vector[A]] => Exp[Unit])
    extends DeliteOpSingleTask(reifyEffects(matrix_foreachrow_impl(x,f)))

  case class MatrixFilterRows[A:Manifest](x: Exp[Matrix[A]], pred: Exp[Vector[A]] => Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffects(matrix_filterrows_impl(x,pred)))

  case class MatrixMultiply[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffects(matrix_multiply_impl(x,y))) {

    val mM = manifest[MatrixImpl[A]]
  }

  case class MatrixTimesVector[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffects(matrix_times_vector_impl(x,y))) {

    val mV = manifest[VectorImpl[A]]
  }
  case class MatrixSigmoid[A](in: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double])
    extends DeliteOpSingleTask(reifyEffects(matrix_sigmoid_impl(in))) {

    val v = fresh[A]
    val func = (1.0/(1.0+Math.exp(conv(v)*(-1))))
    val mM = manifest[MatrixImpl[Double]]
  }
  case class MatrixSigmoidF[A](in: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double])
    extends DeliteOpSingleTask(reifyEffects(matrix_sigmoidf_impl(in))) {

    val v = fresh[A]
    val func = (1.0/(1.0+Math.exp(conv(v)*(-1)))).asInstanceOfL[Float]
    val mM = manifest[MatrixImpl[Float]]
  }


  ////////////////////////////////
  // implemented via delite ops

  case class MatrixPlus[A:Manifest:Arith](inA: Exp[Matrix[A]], inB: Exp[Matrix[A]])
    extends DeliteOpZipWith[A,A,A,Matrix] {

    val alloc = reifyEffects(Matrix[A](inA.numRows, inA.numCols))
    val v = (fresh[A],fresh[A])
    val func = v._1 + v._2
  }

  case class MatrixPlusScalar[A:Manifest:Arith](in: Exp[Matrix[A]], y: Exp[A])
    extends DeliteOpMap[A,A,Matrix] {

    val alloc = reifyEffects(Matrix[A](in.numRows, in.numCols))
    val v = fresh[A]
    val func = v + y
  }

  case class MatrixPlusEquals[A:Manifest:Arith](inA: Exp[Matrix[A]], inB: Exp[Matrix[A]])
    extends DeliteOpZipWith[A,A,A,Matrix] {

    val alloc = inA
    val v = (fresh[A],fresh[A])
    val func = v._1 + v._2
  }

  case class MatrixMinus[A:Manifest:Arith](inA: Exp[Matrix[A]], inB: Exp[Matrix[A]])
    extends DeliteOpZipWith[A,A,A,Matrix] {

    val alloc = reifyEffects(Matrix[A](inA.numRows, inA.numCols))
    val v = (fresh[A],fresh[A])
    val func = v._1 - v._2
  }

  case class MatrixMinusScalar[A:Manifest:Arith](in: Exp[Matrix[A]], y: Exp[A])
    extends DeliteOpMap[A,A,Matrix] {

    val alloc = reifyEffects(Matrix[A](in.numRows, in.numCols))
    val v = fresh[A]
    val func = v - y
  }

  case class MatrixTimes[A:Manifest:Arith](inA: Exp[Matrix[A]], inB: Exp[Matrix[A]])
    extends DeliteOpZipWith[A,A,A,Matrix] {

    val alloc = reifyEffects(Matrix[A](inA.numRows, inA.numCols))
    val v = (fresh[A],fresh[A])
    val func = v._1 * v._2
  }

  case class MatrixTimesScalar[A:Manifest:Arith](in: Exp[Matrix[A]], y: Exp[A])
    extends DeliteOpMap[A,A,Matrix] {

    val alloc = reifyEffects(Matrix[A](in.numRows, in.numCols))
    val v = fresh[A]
    val func = v * y
  }
/*
  case class MatrixTimesVector[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Vector[A]])
    extends DeliteOpMap[Vector[A],A,Vector] {

    val in = reifyEffects {
      var tcoll = Vector[Vector[A]](x.numRows, false)
       for (i <- 0 until x.numRows){
         tcoll(i) = x.getRow(i)
       }
      tcoll
    }

    val alloc = reifyEffects(Vector[A](x.numRows, false))
    val v = fresh[Vector[A]]
    val func = v *:* y
    val mM = manifest[VectorImpl[A]]
  }
*/
  case class MatrixDivide[A:Manifest:Arith](inA: Exp[Matrix[A]], inB: Exp[Matrix[A]])
    extends DeliteOpZipWith[A,A,A,Matrix] {

    val alloc = reifyEffects(Matrix[A](inA.numRows, inA.numCols))
    val v = (fresh[A],fresh[A])
    val func = v._1 / v._2
  }

  case class MatrixDivideScalar[A:Manifest:Arith](in: Exp[Matrix[A]], y: Exp[A])
    extends DeliteOpMap[A,A,Matrix] {

    val alloc = reifyEffects(Matrix[A](in.numRows, in.numCols))
    val v = fresh[A]
    val func = v / y
  }
  
  case class MatrixSum[A:Manifest:Arith](in: Exp[Matrix[A]])
    extends DeliteOpReduce[A] {

    val v = (fresh[A],fresh[A])
    val func = v._1 + v._2
  }

  case class MatrixSumRow[A:Manifest:Arith](x: Exp[Matrix[A]])
    extends DeliteOpMap[Vector[A],A,Vector] {

    val alloc = reifyEffects(Vector[A](x.numRows, false))
    val in = reifyEffects {
      var tcoll = Vector[Vector[A]](x.numRows, false)
       for (i <- 0 until x.numRows){
         tcoll(i) = x.getRow(i)
       }
      tcoll
    }

    val v = fresh[Vector[A]]
    val func = v.sum
  }

  case class MatrixSumCol[A:Manifest:Arith](x: Exp[Matrix[A]])
    extends DeliteOpMap[Vector[A],A,Vector] {

    val alloc = reifyEffects(Vector[A](x.numCols, true))
    val in = reifyEffects {
      var tcoll = Vector[Vector[A]](x.numCols, true)
       for (i <- 0 until x.numCols){
         tcoll(i) = x.getCol(i)
       }
      tcoll
    }

    val v = fresh[Vector[A]]
    val func = v.sum
  }

//  case class MatrixUnaryMinus[A:Manifest:Arith](in: Exp[Matrix[A]])
//    extends DeliteOpMap[A,A,Matrix] {
//
//    val alloc = reifyEffects(Matrix[A](in.numRows, in.numCols))
//    val v = fresh[A]
//    val func = v.unary_-
//  }

  case class MatrixAbs[A:Manifest:Arith](in: Exp[Matrix[A]])
    extends DeliteOpMap[A,A,Matrix] {

    val alloc = reifyEffects(Matrix[A](in.numRows, in.numCols))
    val v = fresh[A]
    val func = v.abs
  }

  case class MatrixExp[A:Manifest:Arith](in: Exp[Matrix[A]])
    extends DeliteOpMap[A,A,Matrix] {

    val alloc = reifyEffects(Matrix[A](in.numRows, in.numCols))
    val v = fresh[A]
    val func = v.exp
  }
/*
  case class MatrixSigmoid[A](in: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double])
    extends DeliteOpMap[A,Double,Matrix] {

    val alloc = reifyEffects(Matrix[Double](in.numRows, in.numCols))
    val v = fresh[A]
    val func = (1.0/(1.0+Math.exp(conv(v)*(-1))))
    val mM = manifest[MatrixImpl[A]]
  }

  case class MatrixSigmoidF[A](in: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double])
    extends DeliteOpMap[A,Float,Matrix] {

    val alloc = reifyEffects(Matrix[Float](in.numRows, in.numCols))
    val v = fresh[A]
    val func = (1.0/(1.0+Math.exp(conv(v)*(-1)))).asInstanceOfL[Float]
    val mM = manifest[MatrixImpl[A]]
  }
*/
  case class MatrixMin[A:Manifest:Ordering](in: Exp[Matrix[A]])
    extends DeliteOpReduce[A] {

    val v = (fresh[A],fresh[A])
    val func = if (v._1 < v._2) v._1 else v._2
  }

  case class MatrixMax[A:Manifest:Ordering](in: Exp[Matrix[A]])
    extends DeliteOpReduce[A] {

    val v = (fresh[A],fresh[A])
    val func = if (v._1 > v._2) v._1 else v._2
  }

  case class MatrixMap[A:Manifest,B:Manifest](in: Exp[Matrix[A]], v: Exp[A], func: Exp[B])
    extends DeliteOpMap[A,B,Matrix] {

    val alloc = reifyEffects(Matrix[B](in.numRows, in.numCols))
  }

  case class MatrixMutableMap[A:Manifest](in: Exp[Matrix[A]], v: Exp[A], func: Exp[A])
    extends DeliteOpMap[A,A,Matrix] {

    val alloc = in
  }

//  case class MatrixMapRows[A:Manifest,B:Manifest](in: Exp[Vector[Vector[A]]], v: Exp[Vector[A]], func: Exp[Vector[B]])
//    extends DeliteOpMap[Vector[A],Vector[B],Vector] {
//
//    val alloc = in
//  }

  case class MatrixMapRowsToVec[A:Manifest,B:Manifest](x: Exp[Matrix[A]], v: Exp[Vector[A]], func: Exp[B], isRow: Exp[Boolean])
    extends DeliteOpMap[Vector[A],B,Vector] {

    val in = reifyEffects {
      var tcoll = Vector[Vector[A]](x.numRows, isRow)
       for (i <- 0 until x.numRows){
         tcoll(i) = x.getRow(i)
       }
      tcoll
    }

    val alloc = reifyEffects(Vector[B](x.numRows, isRow))
  }

  case class MatrixForeach[A:Manifest](in: Exp[Matrix[A]], v: Exp[A], func: Exp[Unit])
    extends DeliteOpForeach[A,Matrix] {

    val i = fresh[Int]
    val sync = reifyEffects(List())
  }

  case class MatrixZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[Matrix[A]], inB: Exp[Matrix[B]],
                                                             v: (Exp[A],Exp[B]), func: Exp[R])
    extends DeliteOpZipWith[A,B,R,Matrix] {

    val alloc = reifyEffects(Matrix[R](inA.numRows, inA.numCols))
  }

  case class MatrixReduceRows[A:Manifest](x: Exp[Matrix[A]], v: (Exp[Vector[A]],Exp[Vector[A]]), func: Exp[Vector[A]])
    extends DeliteOpReduce[Vector[A]] {

    val in = reifyEffects {
      var tcoll = Vector[Vector[A]](x.numRows, true)
       for (i <- 0 until x.numRows){
         tcoll(i) = x.getRow(i)
       }
      tcoll
    }
  }


  ////////////////////
  // object interface

  def matrix_obj_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(MatrixObjectNew[A](numRows, numCols))
  def matrix_obj_fromseq[A:Manifest](xs: Exp[Seq[Exp[Vector[A]]]]) = reflectEffect(MatrixObjectFromSeq(xs))
  def matrix_obj_fromvec[A:Manifest](xs: Exp[Vector[Vector[A]]]) = reflectEffect(MatrixObjectFromVec(reflectRead(xs)))
  def matrix_obj_diag[A:Manifest](w: Exp[Int], vals: Exp[Vector[A]]) = reflectEffect(MatrixObjectDiag(w, reflectRead(vals)))
  def matrix_obj_identity(w: Exp[Int]) = reflectEffect(MatrixObjectIdentity(w))
  def matrix_obj_zeros(numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(MatrixObjectZeros(numRows, numCols))
  def matrix_obj_zerosf(numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(MatrixObjectZerosF(numRows, numCols))
  def matrix_obj_ones(numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(MatrixObjectOnes(numRows, numCols))
  def matrix_obj_onesf(numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(MatrixObjectOnesF(numRows, numCols))
  def matrix_obj_rand(numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(MatrixObjectRand(numRows, numCols))
  def matrix_obj_randf(numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(MatrixObjectRandF(numRows, numCols))
  def matrix_obj_randn(numRows: Exp[Int], numCols: Exp[Int]) = reflectEffect(MatrixObjectRandn(numRows, numCols))
  def matrix_obj_randnf(numRows: Rep[Int], numCols: Rep[Int]) = reflectEffect(MatrixObjectRandnF(numRows, numCols))


  ///////////////////
  // class interface

  def matrix_apply[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int]) = MatrixApply[A](reflectRead(x),i,j)
  def matrix_vview[A:Manifest](x: Exp[Matrix[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean]) = MatrixVView(reflectRead(x), start, stride, length, isRow)
  def matrix_getrow[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int]) = MatrixGetRow[A](reflectRead(x),i)
  def matrix_getcol[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int]) = MatrixGetCol[A](reflectRead(x),i)
  def matrix_slicerows[A:Manifest](x: Exp[Matrix[A]], begin: Exp[Int], end: Exp[Int]) = MatrixSliceRows(reflectRead(x),begin,end)
  def matrix_numrows[A:Manifest](x: Exp[Matrix[A]]) = MatrixNumRows(reflectRead(x))
  def matrix_numcols[A:Manifest](x: Exp[Matrix[A]]) = MatrixNumCols(reflectRead(x))
    
  def matrix_update[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int], y: Exp[A]) = reflectMutation(MatrixUpdate[A](reflectWrite(x),i,j,reflectRead(y)))
  def matrix_updaterow[A:Manifest](x: Exp[Matrix[A]], row: Exp[Int], y: Exp[Vector[A]]) = reflectMutation(MatrixUpdateRow(reflectWrite(x),row,reflectRead(y)))
  def matrix_insertrow[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], y: Exp[Vector[A]]) = reflectMutation(MatrixInsertRow(reflectWrite(x),pos,reflectRead(y)))
  def matrix_insertallrows[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], y: Exp[Matrix[A]]) = reflectMutation(MatrixInsertAllRows(reflectWrite(x),pos,reflectRead(y)))
  def matrix_insertcol[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], y: Exp[Vector[A]]) = reflectMutation(MatrixInsertCol(reflectWrite(x),pos,reflectRead(y)))
  def matrix_insertallcols[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], y: Exp[Matrix[A]]) = reflectMutation(MatrixInsertAllCols(reflectWrite(x),pos,reflectRead(y)))
  def matrix_removerows[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], len: Exp[Int]) = reflectMutation(MatrixRemoveRows(reflectWrite(x),pos,len))
  def matrix_removecols[A:Manifest](x: Exp[Matrix[A]], pos: Exp[Int], len: Exp[Int]) = reflectMutation(MatrixRemoveCols(reflectWrite(x),pos,len))

  def matrix_transpose[A:Manifest](x: Exp[Matrix[A]]) = MatrixTranspose(reflectRead(x))
  def matrix_clone[A:Manifest](x: Exp[Matrix[A]]) = MatrixClone(reflectRead(x))
  def matrix_pprint[A:Manifest](x: Exp[Matrix[A]]) = reflectEffect(MatrixPPrint(reflectRead(x)))
  def matrix_repmat[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int]) = MatrixRepmat(reflectRead(x),i,j)

  def matrix_plus[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = MatrixPlus(reflectRead(x), reflectRead(y))
  def matrix_plus_scalar[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[A]) = MatrixPlusScalar(reflectRead(x), reflectRead(y))
  def matrix_plusequals[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = reflectMutation(MatrixPlusEquals(reflectReadWrite(x),reflectRead(y)))
  def matrix_minus[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = MatrixMinus(reflectRead(x), reflectRead(y))
  def matrix_minus_scalar[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[A]) = MatrixMinusScalar(reflectRead(x), reflectRead(y))
  def matrix_times[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = MatrixTimes(reflectRead(x), reflectRead(y))
  def matrix_multiply[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = MatrixMultiply(reflectRead(x), reflectRead(y))
  def matrix_times_vector[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Vector[A]]) = MatrixTimesVector(reflectRead(x), reflectRead(y))
  def matrix_times_scalar[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[A]) = MatrixTimesScalar(reflectRead(x), reflectRead(y))
  def matrix_divide[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = MatrixDivide(reflectRead(x), reflectRead(y))
  def matrix_divide_scalar[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[A]) = MatrixDivideScalar(reflectRead(x), reflectRead(y))
  //def matrix_unary_minus[A:Manifest:Arith](x: Exp[Matrix[A]]) = MatrixUnaryMinus(x)
  def matrix_abs[A:Manifest:Arith](x: Exp[Matrix[A]]) = MatrixAbs(reflectRead(x))
  def matrix_exp[A:Manifest:Arith](x: Exp[Matrix[A]]) = MatrixExp(reflectRead(x))
  def matrix_sum[A:Manifest:Arith](x: Exp[Matrix[A]]) = MatrixSum(reflectRead(x))
  def matrix_sumrow[A:Manifest:Arith](x: Exp[Matrix[A]]) = MatrixSumRow(reflectRead(x))
  def matrix_sumcol[A:Manifest:Arith](x: Exp[Matrix[A]]) = MatrixSumCol(reflectRead(x))
  def matrix_inverse[A](x: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double]) = MatrixInverse(reflectRead(x))
  def matrix_sigmoid[A](x: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double]) = MatrixSigmoid(reflectRead(x))
  def matrix_sigmoidf[A](x: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double]) = MatrixSigmoidF(reflectRead(x))
  
  def matrix_min[A:Manifest:Ordering](x: Exp[Matrix[A]]) = MatrixMin(reflectRead(x))
  def matrix_minrow[A:Manifest:Arith:Ordering](x: Exp[Matrix[A]]) = MatrixMinRow(reflectRead(x))
  def matrix_max[A:Manifest:Ordering](x: Exp[Matrix[A]]) = MatrixMax(reflectRead(x))
  def matrix_maxrow[A:Manifest:Arith:Ordering](x: Exp[Matrix[A]]) = MatrixMaxRow(reflectRead(x))

  def matrix_map[A:Manifest,B:Manifest](x: Exp[Matrix[A]], f: Exp[A] => Exp[B]) = {
    val v = fresh[A]
    val func = reifyEffects(f(v))
    MatrixMap(reflectRead(x), v, func)
  }
  def matrix_mmap[A:Manifest](x: Exp[Matrix[A]], f: Exp[A] => Exp[A]) = {
    val v = fresh[A]
    val func = reifyEffects(f(v))
    reflectMutation(MatrixMutableMap(reflectReadWrite(x), v, func))
  }
  def matrix_maprows[A:Manifest,B:Manifest](x: Exp[Matrix[A]], f: Exp[Vector[A]] => Exp[Vector[B]]) = {
    // TODO: how to represent this with a single parallel delite op?
    //val v = fresh[Vector[A]]
    //val func = reifyEffects(v mmap {f(v)})
    //MatrixMapRows(in, v, func)

    MatrixMapRows(reflectRead(x),f)
  }
  def matrix_maprowstovec[A:Manifest,B:Manifest](x: Exp[Matrix[A]], f: Exp[Vector[A]] => Exp[B], isRow: Exp[Boolean] = true) = {
    val v = fresh[Vector[A]]
    val func = reifyEffects(f(v))
    MatrixMapRowsToVec(reflectRead(x),v,func,isRow)
  }
  def matrix_foreach[A:Manifest](x: Exp[Matrix[A]], block: Exp[A] => Exp[Unit]) = {
    val v = fresh[A]
    val func = reifyEffects(block(v))
    reflectEffect(MatrixForeach(reflectRead(x), v, func))
  }
  def matrix_foreachrow[A:Manifest](x: Exp[Matrix[A]], block: Exp[Vector[A]] => Exp[Unit]) = {
    reflectEffect(MatrixForeachRow(reflectRead(x), block))
  }
  def matrix_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Exp[Matrix[A]], y: Exp[Matrix[B]], f: (Exp[A],Exp[B]) => Exp[R]) = {
    val v = (fresh[A], fresh[B])
    val func = reifyEffects(f(v._1,v._2))
    MatrixZipWith(reflectRead(x), reflectRead(y), v, func)
  }
  def matrix_reducerows[A:Manifest](x: Exp[Matrix[A]], f: (Exp[Vector[A]],Exp[Vector[A]]) => Exp[Vector[A]]) = {
    val v = (fresh[Vector[A]],fresh[Vector[A]])
    val func = reifyEffects(f(v._1, v._2))
    MatrixReduceRows(reflectRead(x), v, func)
  }
  def matrix_filterrows[A:Manifest](x: Exp[Matrix[A]], pred: Exp[Vector[A]] => Exp[Boolean]) = MatrixFilterRows(reflectRead(x), pred)
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

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case m@MatrixObjectNew(numRows, numCols) => emitValDef(sym, "new " + remap(m.mM) + "(" + quote(numRows) + "," + quote(numCols) + ")")
    case MatrixVView(x,start,stride,length,isRow) => emitValDef(sym, quote(x) + ".vview(" + quote(start) + "," + quote(stride) + "," + quote(length) + "," + quote(isRow) + ")")
    case MatrixApply(x,i,j) => emitValDef(sym, quote(x) + "(" + quote(i) + ", " + quote(j) + ")")
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

    // BLAS calls
    case m@MatrixMultiply(x,y) if (Config.useBlas) =>
      emitValDef(sym, "new " + remap(m.mM) + "(" + quote(x) + ".numRows," + quote(y) + ".numCols)")
      stream.println("scalaBLAS.matMult(%s.data,%s.data,%s.data,%s.numRows,%s.numCols,%s.numCols)".format(quote(x),quote(y),quote(sym),quote(x),quote(x),quote(y)))
    case m@MatrixTimesVector(x,y) if (Config.useBlas) =>
      emitValDef(sym, "new " + remap(m.mV) + "(" + quote(x) + ".numRows, false)")
      stream.println("scalaBLAS.matVMult(%s.data,%s.data,%s.data,%s.numRows,%s.numCols,0,1)".format(quote(x),quote(y),quote(sym),quote(x),quote(x)))
    case m@MatrixSigmoid(x) if (Config.useBlas) =>
      emitValDef(sym, "new " + remap(m.mM) + "(" + quote(x) + ".numRows," + quote(x) + ".numCols)")
      stream.println("scalaBLAS.sigmoid(%s.data,%s.data,0,%s.numRows*%s.numCols)".format(quote(x),quote(sym),quote(x),quote(x)))
    case m@MatrixSigmoidF(x) if (Config.useBlas) =>
      emitValDef(sym, "new " + remap(m.mM) + "(" + quote(x) + ".numRows," + quote(x) + ".numCols)")
      stream.println("scalaBLAS.sigmoid(%s.data,%s.data,0,%s.numRows*%s.numCols)".format(quote(x),quote(sym),quote(x),quote(x)))

    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenMatrixOps extends CudaGenBase with CudaGenDataStruct {
  val IR: MatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {

	  /* CUBLAS calls */
    case MatrixMultiply(x,y) =>
      val callStream = "cublasSetKernelStream(stream);"
      var callKernel = ""
      if(remap(x.Type.typeArguments(0)) == "double")
        callKernel = "cublasDgemm('n','n',%s.numCols,%s.numRows,%s.numRows,1.0,%s.data,%s.numCols,%s.data,%s.numCols,0.0,%s.data,%s.numCols);".format(quote(y),quote(x),quote(y),quote(y),quote(y),quote(x),quote(x),quote(sym),quote(sym))
      else if(remap(x.Type.typeArguments(0)) == "float")
        callKernel = "cublasSgemm('n','n',%s.numCols,%s.numRows,%s.numRows,1.0,%s.data,%s.numCols,%s.data,%s.numCols,0.0,%s.data,%s.numCols);".format(quote(y),quote(x),quote(y),quote(y),quote(y),quote(x),quote(x),quote(sym),quote(sym))
      else
        throw new RuntimeException("CudaGen: Not GPUable (Type %s is not supported for MatrixMulitply CUBLAS library)".format(remap(x.Type.typeArguments(0))))
      emitLibCall(sym,List(callStream,callKernel))
      emitMatrixAlloc(sym,"%s.numRows".format(quote(x)),"%s.numCols".format(quote(y)))
    
    case MatrixTimesVector(x,y) =>
      val callStream = "cublasSetKernelStream(stream);"
      var callKernel = ""
      if(remap(x.Type.typeArguments(0)) == "double")
        callKernel = "cublasDgemv('t', %s.numCols, %s.numRows, 1.0, %s.data, %s.numCols, %s.data, 1, 0.0, %s.data, 1);".format(quote(x),quote(x),quote(x),quote(x),quote(y),quote(sym))
      else if(remap(x.Type.typeArguments(0)) == "float")
        callKernel = "cublasSgemv('t', %s.numCols, %s.numRows, 1.0, %s.data, %s.numCols, %s.data, 1, 0.0, %s.data, 1);".format(quote(x),quote(x),quote(x),quote(x),quote(y),quote(sym))
      else
        throw new RuntimeException("CudaGen: Not GPUable (Type %s is not supported for Matrix*Vector CUBLAS library)".format(remap(x.Type.typeArguments(0))))
      emitLibCall(sym,List(callStream,callKernel))
      emitVectorAlloc(sym,"%s.numRows".format(quote(x)),"false")

	  // The ops that call through to the underlying real data structure
    case MatrixApply(x,i,j) =>
      emitValDef(sym, "%s.apply(%s,%s)".format(quote(x),quote(i),quote(j)))
    case MatrixUpdate(x,i,j,y)  =>
      stream.println(addTab() + "%s.update(%s,%s,%s);".format(quote(x),quote(i),quote(j),quote(y)))
    case MatrixNumRows(x)  =>
      emitValDef(sym, quote(x) + ".numRows")
    case MatrixNumCols(x)  =>
      emitValDef(sym, quote(x) + ".numCols")

    /* Specialized CUDA code generations */
    case MatrixUpdateRow(x, row, y) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s.length".format(quote(y)))
      stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(y)))
      tabWidth += 1
      stream.println(addTab()+"%s.update(%s,%s,%s.apply(%s));".format(quote(x),quote(row),currDimStr,quote(y),currDimStr))
      tabWidth -= 1
      stream.println(addTab()+"}")
      currDim -= 1

    case MatrixGetRow(x,i) =>
      if(kernelSymbol != sym) {
        stream.println(addTab()+"%s %s;".format(remap(sym.Type),quote(sym)))
        stream.println(addTab()+"%s.length = %s.numCols;".format(quote(sym),quote(x)))
        stream.println(addTab()+"%s.isRow = true;".format(quote(sym)))
        stream.println(addTab()+"%s.data = %s.data+%s*%s.numCols;".format(quote(sym),quote(x),quote(i),quote(x)))
		    emitVectorAlloc(sym,"%s.numCols".format(quote(x)),"true","%s.data".format(quote(x),quote(x)))
      }

    case MatrixObjectDiag(w, vals) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s * %s".format(quote(w),quote(w)))
      stream.println(addTab()+"if( %s < %s*%s ) {".format(currDimStr,quote(w),quote(w)))
      tabWidth += 1
      stream.println(addTab()+"%s.data[%s] = 0;".format(quote(sym),currDimStr))
      stream.println(addTab()+"int i = %s / %s;".format(quote(sym),currDimStr))
      stream.println(addTab()+"int j = " + quote(sym) + " % " + currDimStr + ";")
      stream.println(addTab()+"if(i == j) {")
      tabWidth += 1
      stream.println(addTab()+"%s.update(i, j, %s.apply(i));".format(quote(sym),quote(vals)))
      tabWidth -= 1
      stream.println(addTab()+"}")
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s".format(quote(w)),"%s".format(quote(w)))
      currDim -= 1

    case MatrixTranspose(x) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s.size()".format(quote(x)))
      stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(x)))
      tabWidth += 1
      stream.println(addTab()+"int i = %s / %s;".format(quote(sym),currDimStr))
      stream.println(addTab()+"int j = " + quote(sym) + " % " + currDimStr + ";")
      stream.println(addTab()+"%s.update(j, i, %s.apply(i,j));".format(quote(sym),quote(x)))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s.numCols".format(quote(x)),"%s.numRows".format(quote(x)))
      currDim -= 1

    case MatrixSumCol(x) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s.numCols".format(quote(x)))
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
      emitVectorAlloc(sym,"%s.numCols".format(quote(x)),"true")
      currDim -= 1

    case m@MatrixSigmoidF(x) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s.size()".format(quote(x)))
      stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(x)))
      tabWidth += 1
	    val sigmoidFunc = emitDevFunc(m.func,List(m.v))
      stream.println(addTab()+"int i = %s / %s;".format(quote(sym),currDimStr))
      stream.println(addTab()+"int j = " + quote(sym) + " % " + currDimStr + ";")
      stream.println(addTab()+"%s.update(i,j,%s(%s.apply(i,j)));".format(quote(sym),sigmoidFunc,quote(x)))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s.numRows".format(quote(x)),"%s.numCols".format(quote(x)))
      currDim -= 1


    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMatrixOps extends CGenBase {
  val IR: MatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {

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
    case MatrixApply(x,i,j) =>
      emitValDef(sym, "%s.apply(%s,%s)".format(quote(x),quote(i),quote(j)))
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
