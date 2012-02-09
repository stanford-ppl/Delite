package ppl.dsl.optila.matrix

import java.io.{PrintWriter}

import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import scala.virtualization.lms.common.{CudaGenBase, ScalaGenBase, OpenCLGenBase, CGenBase}
import scala.virtualization.lms.internal.{GenerationFailedException}
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.Config
import ppl.delite.framework.extern.lib._

import ppl.dsl.optila._

trait MatrixOps extends Variables {
  this: OptiLA =>


  // object SymmetricMatrix {
  //     def apply[A:Manifest](n: Rep[Int]) = symmatrix_obj_new(n)
  //   }
  
  abstract class MatrixBuilder[Elem, To] {
    def alloc(numRows: Rep[Int], numCols: Rep[Int]): Rep[To]
    def toIntf(x: Rep[To]): Interface[Matrix[Elem]]
  }  
  // def sparseVectorBuilder[A:Manifest] = new VectorBuilder[A,SparseVector[A]] {
  //     def alloc(length: Rep[Int], isRow: Rep[Boolean]) = Vector.sparse[A](length, isRow)
  //   }
  
  object Matrix {
    def apply[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]) = densematrix_obj_new(numRows, numCols)
    def apply[A:Manifest](xs: Rep[DenseVector[DenseVector[A]]]): Rep[DenseMatrix[A]] = densematrix_obj_fromvec(xs)
    def apply[A](xs: Rep[DenseVector[VectorView[A]]])(implicit mA: Manifest[A], o: Overloaded1): Rep[Matrix[A]] = densematrix_obj_fromvec(xs.asInstanceOf[Rep[DenseVector[DenseVector[A]]]])
    def apply[A:Manifest](xs: Rep[DenseVector[A]]*): Rep[DenseMatrix[A]] = DenseMatrix(DenseVector(xs: _*))

    def dense[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]) = densematrix_obj_new(numRows, numCols)
    //def sparse[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]) = sparsematrix_obj_new(numRows, numCols)   
    
    def diag[A:Manifest](w: Rep[Int], vals: Interface[Vector[A]]) = DenseMatrix.diag[A](w,vals)
    def identity(w: Rep[Int]) = DenseMatrix.identity(w)
    def zeros(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix.zeros(numRows,numCols)
    def zerosf(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix.zerosf(numRows,numCols)
    def mzerosf(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix.mzerosf(numRows,numCols)
    def ones(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix.ones(numRows,numCols)
    def onesf(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix.onesf(numRows,numCols)
    def rand(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix.rand(numRows,numCols)
    def randf(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix.randf(numRows,numCols)
    def randn(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix.randn(numRows,numCols)
    def randnf(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix.randnf(numRows,numCols)
    def mrandnf(numRows: Rep[Int], numCols: Rep[Int]) = DenseMatrix.mrandnf(numRows,numCols)
  }

  trait MatOpsCls[A] extends DCInterfaceOps[Matrix[A],A] {    
    type M[X] // generic return type, unless overloaded for the op as below (TODO: use type classes to clean this up!)
    type V[X]
    type MA = M[A] 
    type VA = V[A]
    
    implicit def mA: Manifest[A]     
    implicit def mM[B:Manifest]: Manifest[M[B]] 
    implicit def toOps[B:Manifest](x: Rep[M[B]]): MatOpsCls[B]
    implicit def toIntf[B:Manifest](x: Rep[M[B]]): Interface[Matrix[B]]        
    implicit def builder[B:Manifest]: MatrixBuilder[B,M[B]]        
    implicit def mV[B:Manifest]: Manifest[V[B]]           
    implicit def vecToIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]]            
    implicit def vecBuilder[B:Manifest]: VectorBuilder[B,V[B]]    
        
    type Self <: Matrix[A]
    implicit def wrap(x: Rep[Self]): Interface[Matrix[A]]
    val elem: Rep[Self] 
    val x = elem

    //////////////////////
    // abstract interface 
    // must be implemented by each type of Matrix
    
    // TODO aks: do we need this here? can we use the dynamically dispatched dc_* methods instead?
    def dcSize(implicit ctx: SourceContext): Rep[Int] // = matrix_dcsize(x)
    def dcApply(n: Rep[Int])(implicit ctx: SourceContext): Rep[A]// = matrix_dcapply(x,n)
    def dcUpdate(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]// = matrix_dcupdate(x,n,y)    
    def numRows(implicit ctx: SourceContext): Rep[Int]
    def numCols(implicit ctx: SourceContext): Rep[Int]
    def apply(i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext): Rep[A]     
    def update(i: Rep[Int], j: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
    def insertRow(pos: Rep[Int], y: Rep[VA])(implicit ctx: SourceContext): Rep[Unit]
    def insertAllRows(pos: Rep[Int], y: Rep[MA])(implicit ctx: SourceContext): Rep[Unit]
    def insertCol(pos: Rep[Int], y: Rep[VA])(implicit ctx: SourceContext): Rep[Unit]
    def insertAllCols(pos: Rep[Int], y: Rep[MA])(implicit ctx: SourceContext): Rep[Unit]    
    def removeRows(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
    def removeCols(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
    def *(y: Rep[MA])(implicit a: Arith[A], ctx: SourceContext): Rep[MA] // = matrix_multiply(x,y)
    def inv(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext): Rep[M[Double]] // = matrix_inverse(x)
    def mapRows[B:Manifest](f: Rep[VectorView[A]] => Rep[V[B]])(implicit ctx: SourceContext): Rep[M[B]] //= matrix_maprows[A,B,V[B],M[B]](x,f)
    def reduceRows(f: (Rep[VectorView[A]],Rep[VectorView[A]]) => Rep[VA])(implicit ctx: SourceContext): Rep[VA] //= matrix_reducerows[A,VA](x,f)
    def vview(start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext): Rep[VectorView[A]] //= matrix_vview(x,start,stride,length,isRow)
    
    // conversions
    def toBoolean(implicit conv: Rep[A] => Rep[Boolean]) = map(e => conv(e))
    def toDouble(implicit conv: Rep[A] => Rep[Double]) = map(e => conv(e))
    def toFloat(implicit conv: Rep[A] => Rep[Float]) = map(e => conv(e))
    def toInt(implicit conv: Rep[A] => Rep[Int]) = map(e => conv(e))
    //def toLong(implicit conv: Rep[A] => Rep[Long]) = map(e => conv(e))

    // accessors
    def apply(i: Rep[Int])(implicit ctx: SourceContext): Rep[VectorView[A]] = getRow(i)
    def size(implicit ctx: SourceContext): Rep[Int] = numRows*numCols
    def getRow(row: Rep[Int])(implicit ctx: SourceContext): Rep[VectorView[A]] = matrix_getrow(x,row)
    def getCol(col: Rep[Int])(implicit ctx: SourceContext): Rep[VectorView[A]] = matrix_getcol(x,col)
    def slice(startRow: Rep[Int], endRow: Rep[Int], startCol: Rep[Int], endCol: Rep[Int])(implicit ctx: SourceContext): Rep[MA] = matrix_slice[A,MA](x,startRow,endRow,startCol,endCol)
    def sliceRows(start: Rep[Int], end: Rep[Int])(implicit ctx: SourceContext): Rep[MA] = matrix_slicerows[A,MA](x,start,end)

    // general
    def t(implicit ctx: SourceContext): Rep[MA] = matrix_transpose[A,MA](x)
    // TODO: implicit won't trigger
    //override def clone = matrix_clone(x)
    def Clone()(implicit ctx: SourceContext): Rep[MA] = matrix_clone[A,MA](x) 
    def mutable()(implicit ctx: SourceContext): Rep[MA] = matrix_mutable_clone[A,MA](x)
    def pprint()(implicit ctx: SourceContext): Rep[Unit] = matrix_pprint(x)
    def replicate(i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext): Rep[MA] = matrix_repmat[A,MA](x,i,j)

    // data operations
    def update(i: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit] = updateRow(i, y)
    def updateRow(row: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit] = matrix_updaterow(x,row,y)
    def +=(y: Rep[VA])(implicit ctx: SourceContext): Rep[Unit] = insertRow(numRows,y)
    def ++=(y: Rep[MA])(implicit ctx: SourceContext): Rep[Unit] = insertAllRows(numRows,y)
    def removeRow(pos: Rep[Int])(implicit ctx: SourceContext): Rep[Unit] = removeRows(pos, unit(1))
    def removeCol(pos: Rep[Int])(implicit ctx: SourceContext): Rep[Unit] = removeCols(pos, unit(1))

    // arithmetic operations
    def +(y: Interface[Matrix[A]])(implicit a: Arith[A], ctx: SourceContext): Rep[MA] = matrix_plus[A,MA](x,y)
    def +(y: Rep[A])(implicit a: Arith[A], o: Overloaded1, ctx: SourceContext): Rep[MA] = matrix_plus_scalar[A,MA](x,y)
    def +[B:Manifest](y: Interface[Matrix[B]])(implicit a: Arith[A], c: Rep[B] => Rep[A], ctx: SourceContext) = matrix_plus_withconvert[B,A,MA](y,x)
    def +=(y: Interface[Matrix[A]])(implicit a: Arith[A], ctx: SourceContext) = { matrix_plusequals[A](x,y); x }
    def -(y: Interface[Matrix[A]])(implicit a: Arith[A], ctx: SourceContext): Rep[MA] = matrix_minus[A,MA](x,y)
    def -(y: Rep[A])(implicit a: Arith[A], o: Overloaded1, ctx: SourceContext): Rep[MA] = matrix_minus_scalar[A,MA](x,y)
    def -[B:Manifest](y: Interface[Matrix[B]])(implicit a: Arith[A], c: Rep[B] => Rep[A], ctx: SourceContext) = matrix_minus_withconvert[B,A,MA](y,x)
    def *:*(y: Interface[Matrix[A]])(implicit a: Arith[A], ctx: SourceContext): Rep[MA] = matrix_times[A,MA](x,y)
    def *(y: Interface[Vector[A]])(implicit a: Arith[A], o: Overloaded1, ctx: SourceContext): Rep[VA] = matrix_times_vector[A,VA](x,y)
    def *(y: Rep[A])(implicit a: Arith[A], o: Overloaded2, ctx: SourceContext): Rep[MA] = matrix_times_scalar[A,MA](x,y)
    def *:*[B:Manifest](y: Interface[Matrix[B]])(implicit a: Arith[A], c: Rep[B] => Rep[A], ctx: SourceContext) = matrix_times_withconvert[B,A,MA](y,x)
    def /(y: Interface[Matrix[A]])(implicit a: Arith[A], ctx: SourceContext): Rep[MA] = matrix_divide[A,MA](x,y)
    def /(y: Rep[A])(implicit a: Arith[A], o: Overloaded1, ctx: SourceContext): Rep[MA] = matrix_divide_scalar[A,MA](x,y)
    def /[B:Manifest](y: Interface[Matrix[B]])(implicit a: Arith[A], c: Rep[B] => Rep[A], ctx: SourceContext) = matrix_divide_withconvert[B,A,MA](y,x)
    //def unary_-(implicit a: Arith[A]) = matrix_unary_minus(x)
    def abs(implicit a: Arith[A], ctx: SourceContext): Rep[MA] = matrix_abs[A,MA](x)
    def exp(implicit a: Arith[A], ctx: SourceContext): Rep[MA] = matrix_exp[A,MA](x)
    def sum(implicit a: Arith[A], ctx: SourceContext) = matrix_sum(x)
    def sumRow(implicit a: Arith[A], ctx: SourceContext): Rep[VA] = matrix_sumrow[A,VA](x)
    def sumCol(implicit a: Arith[A], ctx: SourceContext): Rep[VA] = matrix_sumcol[A,VA](x)
    def sigmoid(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext): Rep[M[Double]] = matrix_sigmoid[A,M[Double]](x)
    def sigmoidf(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext): Rep[M[Float]] = matrix_sigmoidf[A,M[Float]](x)

    // ordering operations
    def min(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext) = matrix_min(x)
    def minRow(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext): Rep[VA] = matrix_minrow[A,VA](x)
    def max(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext) = matrix_max(x)
    def maxRow(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext): Rep[VA] = matrix_maxrow[A,VA](x)
    def :>(y: Interface[Matrix[A]])(implicit o: Ordering[A], ctx: SourceContext) = zip(y) { (a,b) => a > b }
    def :<(y: Interface[Matrix[A]])(implicit o: Ordering[A], ctx: SourceContext) = zip(y) { (a,b) => a < b }

    // bulk operations
    def map[B:Manifest](f: Rep[A] => Rep[B])(implicit ctx: SourceContext): Rep[M[B]] = matrix_map[A,B,M[B]](x,f)
    /// TODO: rename to transform?
    def mmap(f: Rep[A] => Rep[A])(implicit ctx: SourceContext): Rep[Self] = { matrix_mmap(x,f); x }    
    def mapRowsToVector[B:Manifest](f: Rep[VectorView[A]] => Rep[B], isRow: Rep[Boolean] = unit(false))(implicit ctx: SourceContext): Rep[V[B]] = matrix_maprowstovec[A,B,V[B]](x,f,isRow)
    def foreach(block: Rep[A] => Rep[Unit])(implicit ctx: SourceContext) = matrix_foreach(x, block)
    def foreachRow(block: Rep[VectorView[A]] => Rep[Unit])(implicit ctx: SourceContext) = matrix_foreachrow(x, block)
    def zip[B:Manifest,R:Manifest](y: Interface[Matrix[B]])(f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[M[R]] = matrix_zipwith[A,B,R,M[R]](x,y,f)    
    def filterRows(pred: Rep[VectorView[A]] => Rep[Boolean])(implicit ctx: SourceContext): Rep[MA] = matrix_filterrows[A,MA](x,pred)
    def groupRowsBy[K:Manifest](pred: Rep[VectorView[A]] => Rep[K])(implicit ctx: SourceContext): Rep[DenseVector[MA]] = matrix_grouprowsby(x, pred)
    def count(pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext) = matrix_count(x, pred)
    // def countRows    
  }

  def __equal[A:Manifest,M[X] <: Matrix[X]](a: Rep[M[A]], b: Rep[M[A]])(implicit toIntf: Rep[M[A]] => Interface[Matrix[A]], mA: Manifest[M[A]], ctx: SourceContext, o: Overloaded5): Rep[Boolean] = matrix_equals(a,b)
  def __equal[A:Manifest,M[X] <: Matrix[X]](a: Rep[M[A]], b: Var[M[A]])(implicit toIntf: Rep[M[A]] => Interface[Matrix[A]], mA: Manifest[M[A]], ctx: SourceContext, o: Overloaded6): Rep[Boolean] = matrix_equals(a,readVar(b))
  def __equal[A:Manifest,M[X] <: Matrix[X]](a: Var[M[A]], b: Rep[M[A]])(implicit toIntf: Rep[M[A]] => Interface[Matrix[A]], mA: Manifest[M[A]], ctx: SourceContext, o: Overloaded7): Rep[Boolean] = matrix_equals(readVar(a),b)
  def __equal[A:Manifest,M[X] <: Matrix[X]](a: Var[M[A]], b: Var[M[A]])(implicit toIntf: Rep[M[A]] => Interface[Matrix[A]], mA: Manifest[M[A]], ctx: SourceContext, o: Overloaded8): Rep[Boolean] = matrix_equals(readVar(a),readVar(b))

  // special case overrides
  def infix_:>[M[X] <: Matrix[X]](x: Rep[M[Float]], y: Rep[M[Float]])(implicit toIntf: Rep[M[Float]] => Interface[Matrix[Float]], ctx: SourceContext): Rep[M[Float]] = (toIntf(x).zip(y) { (a,b) => if (a > b) unit(1f) else unit(0f) }).ops.elem.asInstanceOf[Rep[M[Float]]]
  def infix_:>[M[X] <: Matrix[X]](x: Rep[M[Double]], y: Rep[M[Double]])(implicit toIntf: Rep[M[Double]] => Interface[Matrix[Double]], ctx: SourceContext, o: Overloaded1): Rep[M[Double]] = (toIntf(x).zip(y) { (a,b) => if (a > b) unit(1.) else unit(0.) }).ops.elem.asInstanceOf[Rep[M[Double]]]
  def infix_:>[M[X] <: Matrix[X]](x: Rep[M[Int]], y: Rep[M[Int]])(implicit toIntf: Rep[M[Int]] => Interface[Matrix[Int]], ctx: SourceContext, o: Overloaded2): Rep[M[Int]] = (toIntf(x).zip(y) { (a,b) => if (a > b) unit(1) else unit(0) }).ops.elem.asInstanceOf[Rep[M[Int]]]
  def infix_:<[M[X] <: Matrix[X]](x: Rep[M[Float]], y: Rep[M[Float]])(implicit toIntf: Rep[M[Float]] => Interface[Matrix[Float]], ctx: SourceContext, o: Overloaded3): Rep[M[Float]] = (toIntf(x).zip(y) { (a,b) => if (a > b) unit(1f) else unit(0f) }).ops.elem.asInstanceOf[Rep[M[Float]]]
  def infix_:<[M[X] <: Matrix[X]](x: Rep[M[Double]], y: Rep[M[Double]])(implicit toIntf: Rep[M[Double]] => Interface[Matrix[Double]], ctx: SourceContext, o: Overloaded4): Rep[M[Double]] = (toIntf(x).zip(y) { (a,b) => if (a > b) unit(1.) else unit(0.) }).ops.elem.asInstanceOf[Rep[M[Double]]]
  def infix_:<[M[X] <: Matrix[X]](x: Rep[M[Int]], y: Rep[M[Int]])(implicit toIntf: Rep[M[Int]] => Interface[Matrix[Int]], ctx: SourceContext, o: Overloaded5): Rep[M[Int]] = (toIntf(x).zip(y) { (a,b) => if (a > b) unit(1) else unit(0) }).ops.elem.asInstanceOf[Rep[M[Int]]]
  
  /**
   * Binary math operations on Vectors with unit conversions (precision widening). 
   */  
  
  // generic 
  def infix_+[L,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: L, rhs: Rep[M[R]])(implicit c: L => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded13): Rep[M[R]] = matrix_plus_scalar[R,M[R]](toIntf(rhs),c(lhs))
  def infix_+[L:Arith:Manifest,R:Manifest,M[X] <: Matrix[X]](lhs: Rep[L], rhs: Rep[M[R]])(implicit c: Rep[R] => Rep[L], mb: MatrixBuilder[L,M[L]], toIntf: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[L]], ctx: SourceContext, o: Overloaded14): Rep[M[L]] = matrix_plus_scalar_withconvert[R,L,M[L]](toIntf(rhs),lhs)
  def infix_+[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: Rep[R])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[L]] => Interface[Matrix[L]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded15): Rep[M[R]] = matrix_plus_scalar_withconvert[L,R,M[R]](toIntf(lhs),rhs)
  def infix_+[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: R)(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[L]] => Interface[Matrix[L]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded16): Rep[M[R]] = matrix_plus_scalar_withconvert[L,R,M[R]](toIntf(lhs),unit(rhs))
  def infix_+[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Interface[Matrix[L]], rhs: Rep[M[R]])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded17): Rep[M[R]] = matrix_plus_withconvert[L,R,M[R]](lhs,toIntf(rhs))
  def infix_+[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: Rep[M[R]])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntfL: Rep[M[L]] => Interface[Matrix[L]], toIntfR: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded18): Rep[M[R]] = matrix_plus_withconvert[L,R,M[R]](toIntfL(lhs),toIntfR(rhs))
  
  // special cases to fill holes
  def infix_+[M[X] <: Matrix[X]](lhs: Rep[Int], rhs: Rep[M[Double]])(implicit mb: MatrixBuilder[Double,M[Double]], toIntf: Rep[M[Double]] => Interface[Matrix[Double]], m: Manifest[M[Double]], ctx: SourceContext, o: Overloaded19): Rep[M[Double]] = matrix_plus_scalar[Double,M[Double]](toIntf(rhs),repIntToRepDouble(lhs))
  def infix_+[M[X] <: Matrix[X]](lhs: Rep[Int], rhs: Rep[M[Float]])(implicit mb: MatrixBuilder[Float,M[Float]], toIntf: Rep[M[Float]] => Interface[Matrix[Float]], m: Manifest[M[Float]], ctx: SourceContext, o: Overloaded20): Rep[M[Float]] = matrix_plus_scalar[Float,M[Float]](toIntf(rhs),repIntToRepFloat(lhs))
  def infix_+[M[X] <: Matrix[X]](lhs: Rep[Float], rhs: Rep[M[Double]])(implicit mb: MatrixBuilder[Double,M[Double]], toIntf: Rep[M[Double]] => Interface[Matrix[Double]], m: Manifest[M[Double]], ctx: SourceContext, o: Overloaded21): Rep[M[Double]] = matrix_plus_scalar[Double,M[Double]](toIntf(rhs),repFloatToRepDouble(lhs))
  def infix_+[M[X] <: Matrix[X]](lhs: Float, rhs: Rep[M[Int]])(implicit mb: MatrixBuilder[Float,M[Float]], toIntf: Rep[M[Int]] => Interface[Matrix[Int]], m: Manifest[M[Float]], ctx: SourceContext, o: Overloaded22): Rep[M[Float]] = matrix_plus_scalar_withconvert[Int,Float,M[Float]](toIntf(rhs),unit(lhs))
  def infix_+[M[X] <: Matrix[X]](lhs: Double, rhs: Rep[M[Int]])(implicit mb: MatrixBuilder[Double,M[Double]], toIntf: Rep[M[Int]] => Interface[Matrix[Int]], m: Manifest[M[Double]], ctx: SourceContext, o: Overloaded23): Rep[M[Double]] = matrix_plus_scalar_withconvert[Int,Double,M[Double]](toIntf(rhs),unit(lhs))
  def infix_+[M[X] <: Matrix[X]](lhs: Double, rhs: Rep[M[Float]])(implicit mb: MatrixBuilder[Double,M[Double]], toIntf: Rep[M[Float]] => Interface[Matrix[Float]], m: Manifest[M[Double]], ctx: SourceContext, o: Overloaded24): Rep[M[Double]] = matrix_plus_scalar_withconvert[Float,Double,M[Double]](toIntf(rhs),unit(lhs))

  def infix_-[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: Rep[R])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[L]] => Interface[Matrix[L]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded5): Rep[M[R]] = matrix_minus_scalar_withconvert[L,R,M[R]](toIntf(lhs),rhs)
  def infix_-[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: R)(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[L]] => Interface[Matrix[L]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded6): Rep[M[R]] = matrix_minus_scalar_withconvert[L,R,M[R]](toIntf(lhs),unit(rhs))
  def infix_-[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Interface[Matrix[L]], rhs: Rep[M[R]])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded7): Rep[M[R]] = matrix_minus_withconvert[L,R,M[R]](lhs,toIntf(rhs))
  def infix_-[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: Rep[M[R]])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntfL: Rep[M[L]] => Interface[Matrix[L]], toIntfR: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded8): Rep[M[R]] = matrix_minus_withconvert[L,R,M[R]](toIntfL(lhs),toIntfR(rhs))

  def infix_*[L,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: L, rhs: Rep[M[R]])(implicit c: L => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded13): Rep[M[R]] = matrix_times_scalar[R,M[R]](toIntf(rhs),c(lhs))
  def infix_*[L:Arith:Manifest,R:Manifest,M[X] <: Matrix[X]](lhs: Rep[L], rhs: Rep[M[R]])(implicit c: Rep[R] => Rep[L], mb: MatrixBuilder[L,M[L]], toIntf: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[L]], ctx: SourceContext, o: Overloaded14): Rep[M[L]] = matrix_times_scalar_withconvert[R,L,M[L]](toIntf(rhs),lhs)
  def infix_*[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: Rep[R])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[L]] => Interface[Matrix[L]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded15): Rep[M[R]] = matrix_times_scalar_withconvert[L,R,M[R]](toIntf(lhs),rhs)
  def infix_*[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: R)(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[L]] => Interface[Matrix[L]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded16): Rep[M[R]] = matrix_times_scalar_withconvert[L,R,M[R]](toIntf(lhs),unit(rhs))
  def infix_*:*[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Interface[Matrix[L]], rhs: Rep[M[R]])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded17): Rep[M[R]] = matrix_times_withconvert[L,R,M[R]](lhs,toIntf(rhs))
  def infix_*:*[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: Rep[M[R]])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntfL: Rep[M[L]] => Interface[Matrix[L]], toIntfR: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded18): Rep[M[R]] = matrix_times_withconvert[L,R,M[R]](toIntfL(lhs),toIntfR(rhs))
  def infix_*[M[X] <: Matrix[X]](lhs: Rep[Int], rhs: Rep[M[Double]])(implicit mb: MatrixBuilder[Double,M[Double]], toIntf: Rep[M[Double]] => Interface[Matrix[Double]], m: Manifest[M[Double]], ctx: SourceContext, o: Overloaded19): Rep[M[Double]] = matrix_times_scalar[Double,M[Double]](toIntf(rhs),repIntToRepDouble(lhs))
  def infix_*[M[X] <: Matrix[X]](lhs: Rep[Int], rhs: Rep[M[Float]])(implicit mb: MatrixBuilder[Float,M[Float]], toIntf: Rep[M[Float]] => Interface[Matrix[Float]], m: Manifest[M[Float]], ctx: SourceContext, o: Overloaded20): Rep[M[Float]] = matrix_times_scalar[Float,M[Float]](toIntf(rhs),repIntToRepFloat(lhs))
  def infix_*[M[X] <: Matrix[X]](lhs: Rep[Float], rhs: Rep[M[Double]])(implicit mb: MatrixBuilder[Double,M[Double]], toIntf: Rep[M[Double]] => Interface[Matrix[Double]], m: Manifest[M[Double]], ctx: SourceContext, o: Overloaded21): Rep[M[Double]] = matrix_times_scalar[Double,M[Double]](toIntf(rhs),repFloatToRepDouble(lhs))
  def infix_*[M[X] <: Matrix[X]](lhs: Float, rhs: Rep[M[Int]])(implicit mb: MatrixBuilder[Float,M[Float]], toIntf: Rep[M[Int]] => Interface[Matrix[Int]], m: Manifest[M[Float]], ctx: SourceContext, o: Overloaded22): Rep[M[Float]] = matrix_times_scalar_withconvert[Int,Float,M[Float]](toIntf(rhs),unit(lhs))
  def infix_*[M[X] <: Matrix[X]](lhs: Double, rhs: Rep[M[Int]])(implicit mb: MatrixBuilder[Double,M[Double]], toIntf: Rep[M[Int]] => Interface[Matrix[Int]], m: Manifest[M[Double]], ctx: SourceContext, o: Overloaded23): Rep[M[Double]] = matrix_times_scalar_withconvert[Int,Double,M[Double]](toIntf(rhs),unit(lhs))
  def infix_*[M[X] <: Matrix[X]](lhs: Double, rhs: Rep[M[Float]])(implicit mb: MatrixBuilder[Double,M[Double]], toIntf: Rep[M[Float]] => Interface[Matrix[Float]], m: Manifest[M[Double]], ctx: SourceContext, o: Overloaded24): Rep[M[Double]] = matrix_times_scalar_withconvert[Float,Double,M[Double]](toIntf(rhs),unit(lhs))
  
  def infix_/[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: Rep[R])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[L]] => Interface[Matrix[L]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded5): Rep[M[R]] = matrix_divide_scalar_withconvert[L,R,M[R]](toIntf(lhs),rhs)
  def infix_/[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: R)(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[L]] => Interface[Matrix[L]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded6): Rep[M[R]] = matrix_divide_scalar_withconvert[L,R,M[R]](toIntf(lhs),unit(rhs))
  def infix_/[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Interface[Matrix[L]], rhs: Rep[M[R]])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded7): Rep[M[R]] = matrix_divide_withconvert[L,R,M[R]](lhs,toIntf(rhs))
  def infix_/[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: Rep[M[R]])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntfL: Rep[M[L]] => Interface[Matrix[L]], toIntfR: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded8): Rep[M[R]] = matrix_divide_withconvert[L,R,M[R]](toIntfL(lhs),toIntfR(rhs))
  
  
  // clients that can handle multiple kinds of matrix must accept an Interface[Matrix[T]],  not a Rep[Matrix[T]]
  class MInterface[A:Manifest](val ops: MatOpsCls[A]) extends DCInterface[Matrix[A],A] {// clients use Interface[Matrix]
    override def toString = "MInterface(" + ops.elem.toString + "  [manifest: " + ops.mA.toString + "])"
  }

  // then we convert from a Interface[Matrix[T]] to an interfaceMatToOpsCls, providing all of the original matrix methods  
  implicit def interfaceToMatOps[A:Manifest](intf: Interface[Matrix[A]]): InterfaceMatOpsCls[A] = new InterfaceMatOpsCls(intf.asInstanceOf[MInterface[A]]) // all Interface[Matrix] should be instances of MInterface, but can we enforce this?
  
  class InterfaceMatOpsCls[A:Manifest](val intf: MInterface[A]) {
    def toBoolean(implicit conv: Rep[A] => Rep[Boolean]) = intf.ops.toIntf(intf.ops.toBoolean)
    def toDouble(implicit conv: Rep[A] => Rep[Double]) = intf.ops.toIntf(intf.ops.toDouble)
    def toFloat(implicit conv: Rep[A] => Rep[Float]) = intf.ops.toIntf(intf.ops.toFloat)
    def toInt(implicit conv: Rep[A] => Rep[Int]) = intf.ops.toIntf(intf.ops.toInt)
    //def toLong(implicit conv: Rep[A] => Rep[Long]) = intf.ops.toIntf(intf.ops.toLong)
  
    def apply(i: Rep[Int])(implicit ctx: SourceContext) = intf.ops.getRow(i)
    def apply(i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext) = intf.ops.apply(i,j)
    def size(implicit ctx: SourceContext): Rep[Int] = intf.ops.size
    def vview(start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext) = intf.ops.vview(start,stride,length,isRow)
    def getRow(row: Rep[Int])(implicit ctx: SourceContext) = intf.ops.getRow(row)
    def getCol(col: Rep[Int])(implicit ctx: SourceContext) = intf.ops.getCol(col)
    def slice(startRow: Rep[Int], endRow: Rep[Int], startCol: Rep[Int], endCol: Rep[Int])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.slice(startRow,endRow,startCol,endCol))
    def sliceRows(start: Rep[Int], end: Rep[Int])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.sliceRows(start,end))
    def numRows(implicit ctx: SourceContext) = intf.ops.numRows
    def numCols(implicit ctx: SourceContext) = intf.ops.numCols

    def t(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.t)
    def Clone()(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.Clone())
    def mutable()(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.mutable())
    def pprint()(implicit ctx: SourceContext) = intf.ops.pprint()
    def replicate(i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.replicate(i,j))

    def update(i: Rep[Int], j: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = intf.ops.update(i,j,y)
    def update(i: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = intf.ops.update(i,y)
    def updateRow(row: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = intf.ops.updateRow(row,y)
    // TODO: we should be able to do these operations with arbitrary interfaces
    def +=(y: Interface[Vector[A]])(implicit ctx: SourceContext) = {
      if (y.asInstanceOf[VInterface[Any]].ops.mV[A] != intf.ops.mV[A]) error(unit("matrix interface += called with illegal argument"))
      else intf.ops.+=(y.ops.elem.asInstanceOf[Rep[intf.ops.V[A]]])
    }
    // def ++=(y: Interface[Matrix[A]])(implicit ctx: SourceContext) = intf.ops.++=(y)
    // def insertRow(pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = intf.ops.insertRow(pos,y)
    // def insertAllRows(pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = intf.ops.insertAllRows(pos,y)
    // def insertCol(pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = intf.ops.insertCol(pos,y)
    // def insertAllCols(pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = intf.ops.insertAllCols(pos,y)
    def removeRow(pos: Rep[Int])(implicit ctx: SourceContext) = intf.ops.removeRow(pos)
    def removeRows(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = intf.ops.removeRows(pos,len)
    def removeCol(pos: Rep[Int])(implicit ctx: SourceContext) = intf.ops.removeCol(pos)
    def removeCols(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = intf.ops.removeCols(pos,len)

    def +(y: Interface[Matrix[A]])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops.+(y))    
    def +(y: Rep[A])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops.+(y))    
    def -(y: Interface[Matrix[A]])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops.-(y))    
    def -(y: Rep[A])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops.-(y))    
    def *:*(y: Interface[Matrix[A]])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops.*:*(y))    
    //def *(y: Interface[Matrix[A]])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops.*(y))
    def *(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.vecToIntf(intf.ops.*(y))
    def *(y: Rep[A])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops.*(y))    
    def /(y: Interface[Matrix[A]])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops./(y))
    def /(y: Rep[A])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops./(y))
    //def unary_-(implicit a: Arith[A]) = matrix_unary_minus(x)
    def abs(implicit a: Arith[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops.abs)
    def exp(implicit a: Arith[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops.exp)
    def sum(implicit a: Arith[A], ctx: SourceContext) = intf.ops.sum
    def sumRow(implicit a: Arith[A], ctx: SourceContext) = intf.ops.vecToIntf(intf.ops.sumRow)
    def sumCol(implicit a: Arith[A], ctx: SourceContext) = intf.ops.vecToIntf(intf.ops.sumCol)
    //def inv(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext) = intf.ops.toIntf(intf.ops.inv)
    def sigmoid(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext) = intf.ops.toIntf(intf.ops.sigmoid)
    def sigmoidf(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext) = intf.ops.toIntf(intf.ops.sigmoidf)

    def min(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext) = intf.ops.min
    def minRow(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext) = intf.ops.vecToIntf(intf.ops.minRow)
    def max(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext) = intf.ops.max
    def maxRow(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext) = intf.ops.vecToIntf(intf.ops.maxRow)
    def :>(y: Interface[Matrix[A]])(implicit o: Ordering[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops.:>(y))
    def :<(y: Interface[Matrix[A]])(implicit o: Ordering[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops.:<(y))

    def map[B:Manifest](f: Rep[A] => Rep[B])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.map(f))
    /// TODO: rename to transform?
    def mmap(f: Rep[A] => Rep[A])(implicit ctx: SourceContext) = intf.ops.wrap(intf.ops.mmap(f))
    //def mapRows[B:Manifest](f: Rep[VectorView[A]] => Rep[V[B]])(implicit ctx: SourceContext) = matrix_maprows[DenseVector[B]](x,f) // AKS TODO
    def mapRowsToVector[B:Manifest](f: Rep[VectorView[A]] => Rep[B], isRow: Rep[Boolean] = unit(false))(implicit ctx: SourceContext) = intf.ops.vecToIntf(intf.ops.mapRowsToVector(f,isRow))
    def foreach(block: Rep[A] => Rep[Unit])(implicit ctx: SourceContext) = intf.ops.foreach(block)
    def foreachRow(block: Rep[VectorView[A]] => Rep[Unit])(implicit ctx: SourceContext) = intf.ops.foreachRow(block)
    def zip[B:Manifest,R:Manifest](y: Interface[Matrix[B]])(f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.zip(y)(f))
    //def reduceRows(f: (Rep[VectorView[A]],Rep[VectorView[A]]) => Rep[VA])(implicit ctx: SourceContext) = matrix_reducerows[DenseVector[A]](x,f) // AKS TODO
    def filterRows(pred: Rep[VectorView[A]] => Rep[Boolean])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.filterRows(pred))
    def groupRowsBy[K:Manifest](pred: Rep[VectorView[A]] => Rep[K])(implicit ctx: SourceContext) = intf.ops.groupRowsBy(pred)
    def count(pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext) = intf.ops.count(pred)
    // def countRows        
  }
    
  // class defs
  //def matrix_vview[A:Manifest](x: Interface[Matrix[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext): Rep[VectorView[A]]
  def matrix_getrow[A:Manifest](x: Interface[Matrix[A]], i: Rep[Int])(implicit ctx: SourceContext): Rep[VectorView[A]]
  def matrix_getcol[A:Manifest](x: Interface[Matrix[A]], j: Rep[Int])(implicit ctx: SourceContext): Rep[VectorView[A]]
  def matrix_slice[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], startRow: Rep[Int], endRow: Rep[Int], startCol: Rep[Int], endCol: Rep[Int])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def matrix_slicerows[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], start: Rep[Int], end: Rep[Int])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]

  def matrix_equals[A:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[Boolean]
  def matrix_transpose[A:Manifest,MA:Manifest](x: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def matrix_clone[A:Manifest,MA:Manifest](x: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def matrix_mutable_clone[A:Manifest,MA:Manifest](x: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def matrix_pprint[A:Manifest](x: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[Unit]
  def matrix_repmat[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], i: Rep[Int], j: Rep[Int])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]

  def matrix_updaterow[A:Manifest](x: Interface[Matrix[A]], row: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit]

  def matrix_plus[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def matrix_plus_scalar[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Rep[A])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def matrix_plus_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[B]])(implicit conv: Rep[A] => Rep[B], b: MatrixBuilder[B,MB], ctx: SourceContext): Rep[MB]
  def matrix_plus_scalar_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Rep[B])(implicit conv: Rep[A] => Rep[B], b: MatrixBuilder[B,MB], ctx: SourceContext): Rep[MB]  
  def matrix_plusequals[A:Manifest:Arith](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[Unit]
  def matrix_minus[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def matrix_minus_scalar[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Rep[A])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def matrix_minus_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[B]])(implicit conv: Rep[A] => Rep[B], b: MatrixBuilder[B,MB], ctx: SourceContext): Rep[MB]
  def matrix_minus_scalar_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Rep[B])(implicit conv: Rep[A] => Rep[B], b: MatrixBuilder[B,MB], ctx: SourceContext): Rep[MB]    
  def matrix_times[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  //def matrix_multiply[A:Manifest:Arith](x: Rep[Matrix[A]], y: Rep[Matrix[A]])(implicit ctx: SourceContext): Rep[Matrix[A]]
  def matrix_times_vector[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  def matrix_times_scalar[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Rep[A])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def matrix_times_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[B]])(implicit conv: Rep[A] => Rep[B], b: MatrixBuilder[B,MB], ctx: SourceContext): Rep[MB]
  def matrix_times_scalar_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Rep[B])(implicit conv: Rep[A] => Rep[B], b: MatrixBuilder[B,MB], ctx: SourceContext): Rep[MB]    
  def matrix_divide[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def matrix_divide_scalar[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Rep[A])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def matrix_divide_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[B]])(implicit conv: Rep[A] => Rep[B], b: MatrixBuilder[B,MB], ctx: SourceContext): Rep[MB]
  def matrix_divide_scalar_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Rep[B])(implicit conv: Rep[A] => Rep[B], b: MatrixBuilder[B,MB], ctx: SourceContext): Rep[MB]    
  //def matrix_unary_minus[A:Manifest:Arith](x: Rep[Matrix[A]]): Rep[Matrix[A]]
  def matrix_abs[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def matrix_exp[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def matrix_sum[A:Manifest:Arith](x: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[A]
  def matrix_sumrow[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  def matrix_sumcol[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  //def matrix_inverse[A](x: Rep[Matrix[A]])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double], ctx: SourceContext): Rep[Matrix[Double]]
  def matrix_sigmoid[A:Manifest,MD:Manifest](x: Interface[Matrix[A]])(implicit conv: Rep[A] => Rep[Double], b: MatrixBuilder[Double,MD], ctx: SourceContext): Rep[MD]
  def matrix_sigmoidf[A:Manifest,MF:Manifest](x: Interface[Matrix[A]])(implicit conv: Rep[A] => Rep[Double], b: MatrixBuilder[Float,MF], ctx: SourceContext): Rep[MF]

  def matrix_min[A:Manifest:Ordering:HasMinMax](x: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[A]
  def matrix_minrow[A:Manifest:Ordering:HasMinMax,VA:Manifest](x: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  def matrix_max[A:Manifest:Ordering:HasMinMax](x: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[A]
  def matrix_maxrow[A:Manifest:Ordering:HasMinMax,VA:Manifest](x: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]

  def matrix_map[A:Manifest,B:Manifest,MB:Manifest](x: Interface[Matrix[A]], f: Rep[A] => Rep[B])(implicit b: MatrixBuilder[B,MB], ctx: SourceContext): Rep[MB]
  def matrix_mmap[A:Manifest](x: Interface[Matrix[A]], f: Rep[A] => Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  //def matrix_maprows[A:Manifest,VA:Manifest,B:Manifest,VB:Manifest,MB:Manifest](x: Interface[Matrix[A]], f: Interface[Vector[A]] => Interface[VB])(implicit b: MatrixBuilder[B,MB], ctx: SourceContext): Rep[MB]
  def matrix_maprowstovec[A:Manifest,B:Manifest,VB:Manifest](x: Interface[Matrix[A]], f: Rep[VectorView[A]] => Rep[B], isRow: Rep[Boolean])(implicit b: VectorBuilder[B,VB], ctx: SourceContext): Rep[VB]
  def matrix_foreach[A:Manifest](x: Interface[Matrix[A]], block: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def matrix_foreachrow[A:Manifest](x: Interface[Matrix[A]], block: Rep[VectorView[A]] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def matrix_zipwith[A:Manifest,B:Manifest,R:Manifest,MR:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit b: MatrixBuilder[R,MR], ctx: SourceContext): Rep[MR]
  //def matrix_reducerows[A:Manifest](x: Rep[Matrix[A]], f: (Rep[DenseVector[A]],Rep[DenseVector[A]]) => Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[DenseVector[A]]
  def matrix_filterrows[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], pred: Rep[VectorView[A]] => Rep[Boolean])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def matrix_grouprowsby[A:Manifest,K:Manifest,MA:Manifest](x: Interface[Matrix[A]], pred: Rep[VectorView[A]] => Rep[K])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[DenseVector[MA]] 
  def matrix_count[A:Manifest](x: Interface[Matrix[A]], pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): Rep[Int]
}


trait MatrixOpsExp extends MatrixOps with DeliteCollectionOpsExp with VariablesExp {
  this: MatrixImplOps with OptiLAExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure

  // case class SymmetricMatrixObjectNew[A:Manifest](n:Exp[Int]) extends Def[SymmetricMatrix[A]] {
  //      val m = manifest[A]
  //   }
  
  //case class MatrixApply[A:Manifest](x: Exp[Matrix[A]], i: Exp[Int], j: Exp[Int]) extends Def[A]
  //case class MatrixVView[A:Manifest](x: Interface[Matrix[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean]) extends Def[VectorView[A]]
  // case class MatrixApply[A:Manifest](x: Interface[Matrix[A]], i: Exp[Int], j: Exp[Int])
  //   extends DeliteOpSingleTask(reifyEffectsHere(matrix_apply_impl(x, i, j)))

  case class MatrixGetRow[A:Manifest](x: Interface[Matrix[A]], i: Exp[Int]) 
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_getrow_impl(x,i))) {
      
    val m = manifest[A]
  }
  
  case class MatrixGetCol[A:Manifest](x: Interface[Matrix[A]], i: Exp[Int]) 
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_getcol_impl(x,i))) {
      
    val m = manifest[A]
  }

  case class MatrixSlice[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], startRow: Exp[Int], endRow: Exp[Int], startCol: Exp[Int], endCol: Exp[Int])(implicit b: MatrixBuilder[A,MA])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_slice_impl[A,MA](x,startRow,endRow,startCol,endCol)))

  case class MatrixSliceRows[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], start: Exp[Int], end: Exp[Int])(implicit b: MatrixBuilder[A,MA])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_slicerows_impl[A,MA](x,start,end)))
    
  case class MatrixClone[A:Manifest,MA:Manifest](x: Interface[Matrix[A]])(implicit val b: MatrixBuilder[A,MA])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_clone_impl[A,MA](x))) {
    
    val m = manifest[A]
    val mMA = manifest[MA]
  }

//  case class MatrixUpdateRow[A:Manifest](x: Exp[Matrix[A]], row: Exp[Int], y: Exp[Vector[A]])
//    extends DeliteOpSingleTask(reifyEffectsHere(matrix_updaterow_impl(x,row,y)))

  // this is a single task right now because of the likely early exit. should we have a delite op for this?
  case class MatrixEquals[A:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_equals_impl(x,y)))

  case class MatrixTranspose[A:Manifest,MA:Manifest](x: Interface[Matrix[A]])(implicit val b: MatrixBuilder[A,MA])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_transpose_impl[A,MA](x))) {
    val m = manifest[A]
    val mMA = manifest[MA]
  }

  case class MatrixPPrint[A:Manifest](x: Interface[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_pprint_impl[A](x)))

  case class MatrixRepmat[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], i: Exp[Int], j: Exp[Int])(implicit b: MatrixBuilder[A,MA])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_repmat_impl[A,MA](x,i,j)))

  // case class MatrixInverse[A:Manifest,MA:Manifest](x: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], val conv: Exp[A] => Exp[Double])
  //   extends DeliteOpSingleTask(reifyEffectsHere(matrix_inverse_impl[A,MA](x)))

  case class MatrixMinRow[A:Manifest:Ordering:HasMinMax,VA:Manifest](x: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_minrow_impl[A,VA](x)))

  case class MatrixMaxRow[A:Manifest:Ordering:HasMinMax,VA:Manifest](x: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_maxrow_impl[A,VA](x)))

//  case class MatrixMapRows[A:Manifest,B:Manifest](x: Exp[Matrix[A]], f: Exp[VectorView[A]] => Exp[Vector[B]])
//    extends DeliteOpSingleTask(reifyEffectsHere(matrix_maprows_impl(x,f)))

//  case class MatrixForeachRow[A:Manifest](x: Exp[Matrix[A]], f: Exp[VectorView[A]] => Exp[Unit])
//    extends DeliteOpSingleTask(reifyEffectsHere(matrix_foreachrow_impl(x,f)))

  case class MatrixFilterRows[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], pred: Exp[VectorView[A]] => Exp[Boolean])(implicit b: MatrixBuilder[A,MA])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_filterrows_impl[A,MA](x,pred)))  

  case class MatrixSumCol[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA]) 
    extends DeliteOpSingleTask(reifyEffects(matrix_sumcol_impl[A,VA](x)))

  case class MatrixGroupRowsBy[A:Manifest,K:Manifest,MA:Manifest](x: Interface[Matrix[A]], pred: Exp[VectorView[A]] => Exp[K])(implicit b: MatrixBuilder[A,MA])
    extends DeliteOpSingleTask(reifyEffects(matrix_grouprowsby_impl[A,K,MA](x,pred)))

  ///////////////////////////////////////////////////////////////////
  // BLAS enabled routines 

  // TODO: generalize this so that we can generate fused, delite parallel op, or BLAS variants
  // having separate IR nodes breaks pattern matching optimizations... 

  case class MatrixTimesVector[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]], y: Interface[Vector[A]])(implicit val b: VectorBuilder[A,VA])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_times_vector_impl[A,VA](x,y))) {

    def m = manifest[A]
    def a = implicitly[Arith[A]]
    def mVA = manifest[VA]
  }

  
  // case class MatrixMultiply[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]])
  //   extends DeliteOpSingleTask(reifyEffectsHere(matrix_multiply_impl(x,y))) {
  // 
  //   def m = manifest[A]
  //   def a = implicitly[Arith[A]]
  // }

  case class MatrixSigmoid[A:Manifest,MD:Manifest](in: Interface[Matrix[A]])(implicit conv: Exp[A] => Exp[Double], b: MatrixBuilder[Double,MD])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_sigmoid_impl[A,MD](in))) {
    // extends DeliteOpMap[A,Double,MD] {
    // 
    //     def alloc = b.alloc(in.numRows, in.numCols)
    //     val size = in.numRows*in.numCols
    //     def func = e => (1.0/(1.0+exp(conv(e)*(-1))))
  }  
  
  case class MatrixSigmoidF[A:Manifest,MF:Manifest](in: Interface[Matrix[A]])(implicit conv: Exp[A] => Exp[Double], b: MatrixBuilder[Float,MF])
    extends DeliteOpSingleTask(reifyEffectsHere(matrix_sigmoidf_impl[A,MF](in))) {
    // extends DeliteOpMap[A,Float,MF] {
    // 
    // def alloc = b.alloc(in.numRows, in.numCols)
    // val size = in.numRows*in.numCols
    // def func = e => (1.0/(1.0+exp(conv(e)*(-1)))).AsInstanceOf[Float]
  }  
  
  
  ////////////////////////////////
  // implemented via delite ops
  
  abstract class MatrixArithmeticMap[A:Manifest:Arith,MA:Manifest](implicit val b: MatrixBuilder[A,MA]) extends DeliteOpMap[A,A,MA] {
    val intf: Interface[Matrix[A]]    
    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]    
    def alloc = b.alloc(intf.numRows, intf.numCols)
    val size = copyTransformedOrElse(_.size)(intf.dcSize)
    
    def m = manifest[A]
    def mMA = manifest[MA]
    def a = implicitly[Arith[A]]
  }
  
  abstract class MatrixArithmeticZipWith[A:Manifest:Arith,MA:Manifest](implicit val b: MatrixBuilder[A,MA]) extends DeliteOpZipWith[A,A,A,MA] {
    val intfA: Interface[Matrix[A]]
    val intfB: Interface[Matrix[A]]
    val inA = intfA.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Matrix[A]]]    
    def alloc = b.alloc(intfA.numRows, intfA.numCols)
    val size = copyTransformedOrElse(_.size)(intfA.dcSize)
    
    def m = manifest[A]
    def mMA = manifest[MA]
    def a = implicitly[Arith[A]]
  }
  
  abstract class MatrixArithmeticIndexedLoop[A:Manifest:Arith] extends DeliteOpIndexedLoop {
    val intf: Interface[Matrix[A]]    
    val size = copyTransformedOrElse(_.size)(intf.dcSize)
    
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }
  
  abstract class MatrixArithmeticReduce[A:Manifest:Arith] extends DeliteOpReduce[A] {
    val intf: Interface[Matrix[A]]    
    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]    
    val size = copyTransformedOrElse(_.size)(intf.dcSize)
    
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }
  
  case class MatrixPlus[A:Manifest:Arith,MA:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticZipWith[A,MA] {

    def func = (a,b) => a + b
  }

  case class MatrixPlusScalar[A:Manifest:Arith,MA:Manifest](intf: Interface[Matrix[A]], y: Exp[A])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticMap[A,MA] {

    def func = e => e + y
  }
  
  case class MatrixPlusWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[B]])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB])
    extends DeliteOpZipWith[A,B,B,MB] {
    
    val inA = intfA.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Matrix[B]]]
    
    def alloc = b.alloc(intfA.numRows, intfA.numCols)
    val size = copyTransformedOrElse(_.size)(intfA.size)
  
    def func = (a,b) => conv(a) + b
    
    def m = manifest[B]
    def mMB = manifest[MB]
    def a = implicitly[Arith[B]]    
  }
  
  case class MatrixPlusScalarWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intf: Interface[Matrix[A]], y: Exp[B])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB])
    extends DeliteOpMap[A,B,MB] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    def alloc = b.alloc(intf.numRows, intf.numCols)
    val size = copyTransformedOrElse(_.size)(intf.size)
  
    def func = e => conv(e) + y
    
    def m = manifest[B]
    def mMB = manifest[MB]
    def a = implicitly[Arith[B]]    
  }
  
  case class MatrixPlusEquals[A:Manifest:Arith](intf: Interface[Matrix[A]], intfB: Interface[Matrix[A]])
    extends MatrixArithmeticIndexedLoop {

    def func = i => intf.dcUpdate(i, intf.dcApply(i) + intfB.dcApply(i))    
  }

  case class MatrixMinus[A:Manifest:Arith,MA:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticZipWith[A,MA] {

    def func = (a,b) => a - b
  }

  case class MatrixMinusScalar[A:Manifest:Arith,MA:Manifest](intf: Interface[Matrix[A]], y: Exp[A])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticMap[A,MA] {

    def func = e => e - y
  }
  
  case class MatrixMinusWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[B]])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB])
    extends DeliteOpZipWith[A,B,B,MB] {
    
    val inA = intfA.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Matrix[B]]]
    
    def alloc = b.alloc(intfA.numRows, intfA.numCols)
    val size = copyTransformedOrElse(_.size)(intfA.size)

    def m = manifest[B]
    def mMB = manifest[MB]
    def a = implicitly[Arith[B]]
  
    def func = (a,b) => conv(a) - b
  }
  
  case class MatrixMinusScalarWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intf: Interface[Matrix[A]], y: Exp[B])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB])
    extends DeliteOpMap[A,B,MB] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    def alloc = b.alloc(intf.numRows, intf.numCols)
    val size = copyTransformedOrElse(_.size)(intf.size)

    def m = manifest[B]
    def mMB = manifest[MB]
    def a = implicitly[Arith[B]]
  
    def func = e => conv(e) - y
  }
  

  case class MatrixTimes[A:Manifest:Arith,MA:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticZipWith[A,MA] {

    def func = (a,b) => a * b
  }

  case class MatrixTimesScalar[A:Manifest:Arith,MA:Manifest](intf: Interface[Matrix[A]], y: Exp[A])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticMap[A,MA] {

    def func = e => e * y
  }

  case class MatrixTimesWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[B]])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB])
    extends DeliteOpZipWith[A,B,B,MB] {
    
    val inA = intfA.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Matrix[B]]]
    
    def alloc = b.alloc(intfA.numRows, intfA.numCols)
    val size = copyTransformedOrElse(_.size)(intfA.size)
  
    def func = (a,b) => conv(a) * b
    
    def m = manifest[B]
    def mMB = manifest[MB]
    def a = implicitly[Arith[B]]    
  }
  
  case class MatrixTimesScalarWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intf: Interface[Matrix[A]], y: Exp[B])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB])
    extends DeliteOpMap[A,B,MB] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    def alloc = b.alloc(intf.numRows, intf.numCols)
    val size = copyTransformedOrElse(_.size)(intf.size)
  
    def func = e => conv(e) * y
    
    def m = manifest[B]
    def mMB = manifest[MB]
    def a = implicitly[Arith[B]]    
  }
  
  case class MatrixDivide[A:Manifest:Arith,MA:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticZipWith[A,MA] {

    def func = (a,b) => a / b
  }

  case class MatrixDivideScalar[A:Manifest:Arith,MA:Manifest](intf: Interface[Matrix[A]], y: Exp[A])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticMap[A,MA] {

    def func = e => e / y
  }
  
  case class MatrixDivideWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[B]])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB])
    extends DeliteOpZipWith[A,B,B,MB] {
    
    val inA = intfA.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Matrix[B]]]
    
    def alloc = b.alloc(intfA.numRows, intfA.numCols)
    val size = copyTransformedOrElse(_.size)(intfA.size)

    def func = (a,b) => conv(a) / b
    
    def m = manifest[B]
    def mMB = manifest[MB]
    def a = implicitly[Arith[B]]      
  }
  
  case class MatrixDivideScalarWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intf: Interface[Matrix[A]], y: Exp[B])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB])
    extends DeliteOpMap[A,B,MB] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    def alloc = b.alloc(intf.numRows, intf.numCols)
    val size = copyTransformedOrElse(_.size)(intf.size)
  
    def func = e => conv(e) / y
    
    def m = manifest[B]
    def mMB = manifest[MB]
    def a = implicitly[Arith[B]]    
  }
  
  
  case class MatrixSum[A:Manifest:Arith](intf: Interface[Matrix[A]]) 
    extends MatrixArithmeticReduce[A] {
      
    val zero = implicitly[Arith[A]].empty
    def func = (a,b) => a + b
  }
  
  /* this would be nice, but case class inheritance is deprecated */
  //case class MatrixSumRow[A:Manifest:Arith](x: Exp[Matrix[A]]) extends MatrixMapRowsToVec[A,A](x, row => row.sum, unit(false))  
  case class MatrixSumRow[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA])
    extends DeliteOpMap[Int,A,VA] {

    def alloc = b.alloc(x.numRows, unit(false))
    val in = (unit(0)::x.numRows)
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

  case class MatrixAbs[A:Manifest:Arith,MA:Manifest](intf: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticMap[A,MA] {

    def func = e => e.abs
  }

  case class MatrixExp[A:Manifest:Arith,MA:Manifest](intf: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticMap[A,MA] {

    def func = e => e.exp
  }
  
  case class MatrixMin[A:Manifest:Ordering:HasMinMax](intf: Interface[Matrix[A]])
    extends DeliteOpReduce[A] {

    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val size = copyTransformedOrElse(_.size)(intf.dcSize)
    val zero = implicitly[HasMinMax[A]].maxValue
    def func = (a,b) => if (a < b) a else b
  }

  case class MatrixMax[A:Manifest:Ordering:HasMinMax](intf: Interface[Matrix[A]])
    extends DeliteOpReduce[A] {

    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val size = intf.dcSize
    val zero = implicitly[HasMinMax[A]].minValue
    def func = (a,b) => if (a > b) a else b
  }

  case class MatrixMap[A:Manifest,B:Manifest,MB:Manifest](intf: Interface[Matrix[A]], func: Exp[A] => Exp[B])(implicit val b: MatrixBuilder[B,MB])
    extends DeliteOpMap[A,B,MB] {

    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val size = intf.dcSize
    def alloc = b.alloc(intf.numRows, intf.numCols)    
    
    def mA = manifest[A]
    def mB = manifest[B]
    def mMB = manifest[MB]
  }

  case class MatrixMutableMap[A:Manifest](intf: Interface[Matrix[A]], block: Exp[A] => Exp[A])
    extends DeliteOpIndexedLoop {

    val size = copyTransformedOrElse(_.size)(intf.dcSize)
    def func = i => intf.dcUpdate(i, block(intf.dcApply(i)))
  }

  // case class MatrixMapRows[A:Manifest,B:Manifest](x: Exp[Matrix[A]], block: Exp[VectorView[A]] => Exp[DenseVector[B]], out: Exp[Matrix[B]])
  //   extends DeliteOpIndexedLoop {
  // 
  //   val size = x.numRows
  //   def func = i => { out(i) = block(x(i)) } // updateRow should be fused with function application
  // }

  case class MatrixForeachRow[A:Manifest](x: Interface[Matrix[A]], block: Exp[VectorView[A]] => Exp[Unit])
    extends DeliteOpIndexedLoop {

    val size = x.numRows
    def func = i => block(x(i))
  }

  case class MatrixMapRowsToVec[A:Manifest,B: Manifest,VB:Manifest](x: Interface[Matrix[A]], rowFunc: Exp[VectorView[A]] => Exp[B], isRow: Exp[Boolean])(implicit b: VectorBuilder[B,VB])
    extends DeliteOpMap[Int,B,VB] {

    def alloc = b.alloc(x.numRows, isRow)
    val in = (unit(0)::x.numRows)
    val size = x.numRows
    def func = i => rowFunc(x(i))   
  }

  case class MatrixForeach[A:Manifest](intf: Interface[Matrix[A]], func: Exp[A] => Exp[Unit])
    extends DeliteOpForeach[A] {

    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val size = intf.dcSize
    def sync = n => List()
  }

  case class MatrixUpdateRow[A:Manifest](x: Interface[Matrix[A]], row: Exp[Int], y: Interface[Vector[A]])
    extends DeliteOpIndexedLoop {
    
    val size = copyTransformedOrElse(_.size)(y.length) // TODO: assert y.length == x.numCols
    def func = j => { x(row,j) = y(j) } 
  }
  
  case class MatrixZipWith[A:Manifest,B:Manifest,R:Manifest,MR:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[B]],
                                                                         func: (Exp[A], Exp[B]) => Exp[R])(implicit val b: MatrixBuilder[R,MR])
    extends DeliteOpZipWith[A,B,R,MR] {

    val inA = intfA.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val inB = intfA.ops.elem.asInstanceOf[Exp[Matrix[B]]]
    def alloc = b.alloc(intfA.numRows, intfA.numCols)
    val size = copyTransformedOrElse(_.size)(intfA.dcSize)
    
    val mA = manifest[A]
    val mB = manifest[B]
    val mR = manifest[R]
    val mMR = manifest[MR]    
  }

  // More efficient (though slightly uglier) to express this as a loop directly. 
  // TODO: nicer DeliteOpLoop templates? e.g. DeliteOpReductionLoop, ...
  // case class MatrixReduceRows[A:Manifest](x: Exp[Matrix[A]], func: (Exp[VectorView[A]], Exp[DenseVector[A]]) => Exp[DenseVector[A]])
  //   extends DeliteOpReduceLike[VectorView[A],DenseVector[A]] {
  // 
  //   val size = x.numRows
  //   val zero = EmptyVector[A]
  //   
  //   lazy val body: Def[DenseVector[A]] = copyBodyOrElse(DeliteReduceElem[DenseVector[A]](
  //     func = reifyEffects(x(v)),
  //     Nil,
  //     zero = this.zero,
  //     rV = this.rV,
  //     rFunc = reifyEffects(this.func(rV._1, rV._2)),
  //     true
  //   ))
  // }

  case class MatrixCount[A:Manifest](intf: Interface[Matrix[A]], cond: Exp[A] => Exp[Boolean]) 
    extends DeliteOpFilterReduce[A,Int] {

    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val size = copyTransformedOrElse(_.size)(intf.dcSize)
    val zero = unit(0)
    def func = e => unit(1)
    def reduce = (a,b) => a + b   
    
    def m = manifest[A]
  } 


  /////////////////////
  // delite collection
  
  def isDenseMat[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.Type.erasure,classOf[DenseMatrix[A]])  
  def asDenseMat[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[DenseMatrix[A]]]
  
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isDenseMat(x)) densematrix_size(asDenseMat(x))
    else super.dc_size(x)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isDenseMat(x)) densematrix_rawapply(asDenseMat(x),n)
    else super.dc_apply(x,n)    
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isDenseMat(x)) densematrix_rawupdate(asDenseMat(x),n,y)
    else super.dc_update(x,n,y)        
  }
  
  
  ///////////////////
  // class interface

  //def matrix_vview[A:Manifest](x: Interface[Matrix[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(MatrixVView(x, start, stride, length, isRow))
  def matrix_getrow[A:Manifest](x: Interface[Matrix[A]], i: Exp[Int])(implicit ctx: SourceContext) = reflectPure(MatrixGetRow[A](x,i))
  def matrix_getcol[A:Manifest](x: Interface[Matrix[A]], i: Exp[Int])(implicit ctx: SourceContext) = reflectPure(MatrixGetCol[A](x,i))
  def matrix_slice[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], startRow: Exp[Int], endRow: Exp[Int], startCol: Exp[Int], endCol: Exp[Int])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixSlice[A,MA](x,startRow,endRow,startCol,endCol))
  def matrix_slicerows[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], start: Exp[Int], end: Exp[Int])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixSliceRows[A,MA](x,start,end))

  def matrix_updaterow[A:Manifest](x: Interface[Matrix[A]], row: Exp[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectWrite(x.ops.elem)(MatrixUpdateRow(x,row,y))

  def matrix_equals[A:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = reflectPure(MatrixEquals(x,y))
  def matrix_transpose[A:Manifest,MA:Manifest](x: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixTranspose[A,MA](x))
  def matrix_clone[A:Manifest,MA:Manifest](x: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixClone[A,MA](x))
  def matrix_mutable_clone[A:Manifest,MA:Manifest](x: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectMutable(MatrixClone[A,MA](x))
  def matrix_pprint[A:Manifest](x: Interface[Matrix[A]])(implicit ctx: SourceContext) = reflectEffect(MatrixPPrint(x)) // TODO: simple
  def matrix_repmat[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], i: Exp[Int], j: Exp[Int])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixRepmat[A,MA](x,i,j))

  def matrix_plus[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixPlus[A,MA](x, y))
  def matrix_plus_scalar[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Exp[A])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixPlusScalar[A,MA](x, y))
  def matrix_plus_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[B]])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB], ctx: SourceContext) = reflectPure(MatrixPlusWithConvert[A,B,MB](x,y))
  def matrix_plus_scalar_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Exp[B])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB], ctx: SourceContext) = reflectPure(MatrixPlusScalarWithConvert[A,B,MB](x,y))  
  def matrix_minus[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixMinus[A,MA](x,y))
  def matrix_minus_scalar[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Exp[A])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixMinusScalar[A,MA](x,y))
  def matrix_minus_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[B]])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB], ctx: SourceContext) = reflectPure(MatrixMinusWithConvert[A,B,MB](x,y))
  def matrix_minus_scalar_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Exp[B])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB], ctx: SourceContext) = reflectPure(MatrixMinusScalarWithConvert[A,B,MB](x,y))    
  def matrix_times[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixTimes[A,MA](x,y))
  // def matrix_multiply[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]])(implicit ctx: SourceContext) = {
  //     if (Config.useBlas && (manifest[A] == manifest[Double] || manifest[A] == manifest[Float])) reflectPure(MatrixMultiplyBLAS(x,y))
  //     else reflectPure(MatrixMultiply(x,y))
  //   }
  def matrix_times_vector[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = {
    //if (Config.useBlas && (manifest[A] == manifest[Double] || manifest[A] == manifest[Float])) reflectPure(MatrixTimesVectorBLAS(x,y))
    reflectPure(MatrixTimesVector[A,VA](x,y))
  }
  def matrix_times_scalar[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Exp[A])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixTimesScalar[A,MA](x,y))
  def matrix_times_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[B]])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB], ctx: SourceContext) = reflectPure(MatrixTimesWithConvert[A,B,MB](x,y))
  def matrix_times_scalar_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Exp[B])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB], ctx: SourceContext) = reflectPure(MatrixTimesScalarWithConvert[A,B,MB](x,y))    
  def matrix_divide[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixDivide[A,MA](x,y))
  def matrix_divide_scalar[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Exp[A])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixDivideScalar[A,MA](x,y))
  def matrix_divide_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[B]])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB], ctx: SourceContext) = reflectPure(MatrixDivideWithConvert[A,B,MB](x,y))
  def matrix_divide_scalar_withconvert[A:Manifest,B:Manifest:Arith,MB:Manifest](x: Interface[Matrix[A]], y: Exp[B])(implicit conv: Exp[A] => Exp[B], b: MatrixBuilder[B,MB], ctx: SourceContext) = reflectPure(MatrixDivideScalarWithConvert[A,B,MB](x,y))      
  //def matrix_unary_minus[A:Manifest:Arith](x: Exp[Matrix[A]]) = MatrixUnaryMinus(x)
  def matrix_abs[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixAbs[A,MA](x))
  def matrix_exp[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixExp[A,MA](x))
  def matrix_sum[A:Manifest:Arith](x: Interface[Matrix[A]])(implicit ctx: SourceContext) = reflectPure(MatrixSum(x))
  def matrix_sumrow[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(MatrixSumRow[A,VA](x))
  def matrix_sumcol[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(MatrixSumCol[A,VA](x))
  //def matrix_inverse[A](x: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double], ctx: SourceContext) = reflectPure(MatrixInverse(x))
  def matrix_sigmoid[A:Manifest,MD:Manifest](x: Interface[Matrix[A]])(implicit conv: Exp[A] => Exp[Double], b: MatrixBuilder[Double,MD], ctx: SourceContext) = {
    //if (Config.useBlas && manifest[A] == manifest[Double]) reflectPure(MatrixSigmoidVectorized(x))    
    reflectPure(MatrixSigmoid[A,MD](x))
  }
  def matrix_sigmoidf[A:Manifest,MF:Manifest](x: Interface[Matrix[A]])(implicit conv: Exp[A] => Exp[Double], b: MatrixBuilder[Float,MF], ctx: SourceContext) = {
    //if (Config.useBlas && manifest[A] == manifest[Float]) reflectPure(MatrixSigmoidVectorized(x))    
    reflectPure(MatrixSigmoidF[A,MF](x))
  }

  def matrix_plusequals[A:Manifest:Arith](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = reflectWrite(x.ops.elem)(MatrixPlusEquals(x,y))
  
  def matrix_min[A:Manifest:Ordering:HasMinMax](x: Interface[Matrix[A]])(implicit ctx: SourceContext) = reflectPure(MatrixMin(x))
  def matrix_minrow[A:Manifest:Ordering:HasMinMax,VA:Manifest](x: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(MatrixMinRow[A,VA](x))
  def matrix_max[A:Manifest:Ordering:HasMinMax](x: Interface[Matrix[A]])(implicit ctx: SourceContext) = reflectPure(MatrixMax(x))
  def matrix_maxrow[A:Manifest:Ordering:HasMinMax,VA:Manifest](x: Interface[Matrix[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(MatrixMaxRow[A,VA](x))

  def matrix_map[A:Manifest,B:Manifest,MB:Manifest](x: Interface[Matrix[A]], f: Exp[A] => Exp[B])(implicit b: MatrixBuilder[B,MB], ctx: SourceContext) = reflectPure(MatrixMap[A,B,MB](x, f))
  def matrix_mmap[A:Manifest](x: Interface[Matrix[A]], f: Exp[A] => Exp[A])(implicit ctx: SourceContext) = reflectWrite(x.ops.elem)(MatrixMutableMap(x, f)) // effect??
  // def matrix_maprows[A:Manifest,B:Manifest](x: Exp[Matrix[A]], f: Exp[VectorView[A]] => Exp[DenseVector[B]])(implicit ctx: SourceContext) = {
  //   val out = matrix_obj_new[B](x.numRows, x.numCols)
  //   reflectWrite(out)(MatrixMapRows(x,f,out))
  //   out.unsafeImmutable // will this work?
  // }
  def matrix_maprowstovec[A:Manifest,B:Manifest,VB:Manifest](x: Interface[Matrix[A]], f: Exp[VectorView[A]] => Exp[B], isRow: Exp[Boolean] = unit(true))(implicit b: VectorBuilder[B,VB], ctx: SourceContext) = {
    reflectPure(MatrixMapRowsToVec[A,B,VB](x, f, isRow))
  }
  def matrix_foreach[A:Manifest](x: Interface[Matrix[A]], block: Exp[A] => Exp[Unit])(implicit ctx: SourceContext) = {
    reflectEffect(MatrixForeach(x, block)) // read??
  }
  def matrix_foreachrow[A:Manifest](x: Interface[Matrix[A]], block: Exp[VectorView[A]] => Exp[Unit])(implicit ctx: SourceContext) = {
    reflectEffect(MatrixForeachRow(x, block)) // read??
  }
  def matrix_zipwith[A:Manifest,B:Manifest,R:Manifest,MR:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[B]], f: (Exp[A],Exp[B]) => Exp[R])(implicit b: MatrixBuilder[R,MR], ctx: SourceContext) = {
    reflectPure(MatrixZipWith[A,B,R,MR](x, y, f))
  }
  // def matrix_reducerows[A:Manifest](x: Exp[Matrix[A]], f: (Exp[DenseVector[A]],Exp[DenseVector[A]]) => Exp[DenseVector[A]])(implicit ctx: SourceContext) = {
  //     //reflectPure(MatrixReduceRows(x, f))
  //     throw new UnsupportedOperationException("temporarily removed until new DeliteOpReduce[A,R] is supported")
  //   }
  def matrix_filterrows[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], pred: Exp[VectorView[A]] => Exp[Boolean])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixFilterRows[A,MA](x, pred))
  def matrix_grouprowsby[A:Manifest,K:Manifest,MA:Manifest](x: Interface[Matrix[A]], pred: Exp[VectorView[A]] => Exp[K])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixGroupRowsBy[A,K,MA](x,pred))
  def matrix_count[A:Manifest](x: Interface[Matrix[A]], pred: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(MatrixCount(x, pred))

  //////////////////
  // internal

  //def matrix_dcsize[A:Manifest](x: Interface[Matrix[A]])(implicit ctx: SourceContext) = x.numRows * x.numCols
  //def matrix_dcapply[A:Manifest](x: Interface[Matrix[A]], n: Exp[Int])(implicit ctx: SourceContext) = reflectPure(MatrixRawApply(x,n))//reflectPure(DeliteCollectionApply(x,n))//matrix_raw_data(x).apply(n)  // AKS TODO
  //def matrix_dcupdate[A:Manifest](x: Interface[Matrix[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = reflectWrite(x)(MatrixRawUpdate(x,n,y))///*reflectWrite(x)*/reflectPure(DeliteCollectionUpdate(x,n,y))//matrix_raw_data(x).update(n,y)  // AKS TODO
  //def matrix_raw_data[A:Manifest](x: Exp[Matrix[A]]) = reflectMutable(MatrixRawData(x))  

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = {
    (e match {
      //case e@MatrixRawData(x) => matrix_raw_data(f(x))(e.m)
      case e@MatrixGetRow(x,i) => matrix_getrow(f(x),f(i))(e.m, implicitly[SourceContext])
      case e@MatrixGetCol(x,i) => matrix_getcol(f(x),f(i))(e.m, implicitly[SourceContext])
      //case MatrixVView(x, start, stride, length, isRow) => matrix_vview(f(x),f(start),f(stride),f(length),f(isRow)) // should set original, too?
      // delite ops
      //case e@MatrixApply(x,i,j) => reflectPure(new { override val original = Some(f,e) } with MatrixApply(f(x),f(i),f(j)))(mtype(manifest[A]),implicitly[SourceContext])
      case e@MatrixAbs(x) => reflectPure(new { override val original = Some(f,e) } with MatrixAbs(f(x))(e.m,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])
      case e@MatrixSum(x) => reflectPure(new { override val original = Some(f,e) } with MatrixSum(f(x))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
      case e@MatrixMinus(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixMinus(f(x),f(y))(e.m,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])
      case e@MatrixPlus(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixPlus(f(x),f(y))(e.m,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])
      case e@MatrixTimes(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixTimes(f(x),f(y))(e.m,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])
      case e@MatrixMap(x,g) => reflectPure(new { override val original = Some(f,e) } with MatrixMap(f(x),f(g))(e.mA,e.mB,e.mMB,e.b))(mtype(manifest[A]),implicitly[SourceContext])
      case e@MatrixTimesVector(x,y) => reflectPure(new {override val original = Some(f,e) } with MatrixTimesVector(f(x),f(y))(e.m,e.a,e.mVA,e.b))(mtype(manifest[A]),implicitly[SourceContext])
      //case e@MatrixTimesVectorBLAS(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixTimesVectorBLAS(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
      //case e@MatrixMultiply(x,y) => reflectPure(new {override val original = Some(f,e) } with MatrixMultiply(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
      //case e@MatrixMultiplyBLAS(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixMultiplyBLAS(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
      //case e@MatrixInverse(x) => reflectPure(new {override val original = Some(f,e) } with MatrixInverse(f(x))(e.mA,f(e.conv)))(mtype(manifest[A]),implicitly[SourceContext])
      case e@MatrixTranspose(x) => reflectPure(new {override val original = Some(f,e) } with MatrixTranspose(f(x))(e.m,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])
      // reflected
      case Reflect(e@MatrixGetRow(x,i), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixGetRow(f(x),f(i))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatrixGetCol(x,i), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixGetCol(f(x),f(i))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatrixClone(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixClone(f(x))(e.m,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatrixPlus(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixPlus(f(x),f(y))(e.m,e.a,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatrixPlusEquals(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixPlusEquals(f(x),f(y))(e.m, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatrixUpdateRow(x,r,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixUpdateRow(f(x),f(r),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@MatrixZipWith(x,y,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixZipWith(f(x),f(y),f(g))(e.mA,e.mB,e.mR,e.mMR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case _ => super.mirror(e, f)
    }).asInstanceOf[Exp[A]] // why??
  }
  
  /////////////////////
  // aliases and sharing

  // TODO: precise sharing info for other IR types (default is conservative)

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    //case MatrixMultiply(a,b) => Nil
    case MatrixTimes(a,b) => Nil
    case MatrixTimesVector(a,v) => Nil
    case MatrixTimesScalar(a,x) => Nil
    case MatrixRepmat(a,i,j) => Nil
    case MatrixClone(a) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    //case MatrixMultiply(a,b) => Nil
    case MatrixTimes(a,b) => Nil
    case MatrixTimesVector(a,v) => Nil
    case MatrixTimesScalar(a,x) => Nil
    case MatrixRepmat(a,i,j) => Nil
    case MatrixClone(a) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    //case MatrixMultiply(a,b) => Nil
    case MatrixTimes(a,b) => Nil
    case MatrixTimesVector(a,v) => Nil
    case MatrixTimesScalar(a,x) => Nil
    case MatrixRepmat(a,i,j) => Nil
    case MatrixClone(a) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    //case MatrixMultiply(a,b) => Nil
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
  this: MatrixImplOps with OptiLAExp =>

  // TODO aks: debug pattern matching with interfaces
  // need an extractor for Interface
  /*
  override def matrix_plus[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit bldr: MatrixBuilder[A,MA], ctx: SourceContext) = (x,y) match {
    // (AB + AD) == A(B + D)
    case (Interface(Def(MatrixTimes(a, b))), Interface(Def(MatrixTimes(c, d)))) if (a == c) => MatrixTimes(a.asInstanceOf[Interface[Matrix[A]]], bldr.toIntf(MatrixPlus(b.asInstanceOf[Interface[Matrix[A]]],d.asInstanceOf[Interface[Matrix[A]]])))
    // ...
    case _ => super.matrix_plus[A,MA](x, y)
  }

  override def matrix_equals[A:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = (x,y) match {
    case (a,b) if (a == b) => unit(true) // same symbol
    case _ => super.matrix_equals(x,y)
  }

  override def matrix_times[A:Manifest:Arith,MA:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = (x,y) match {
    // X^-1*X = X*X^-1 = I (if X is non-singular)
    //case (Def(MatrixInverse(a)), b) if (a == b) => MatrixObjectIdentity(a.numRows).asInstanceOf[Exp[Matrix[A]]]
    //case (b, Def(MatrixInverse(a))) if (a == b) => MatrixObjectIdentity(a.numRows).asInstanceOf[Exp[Matrix[A]]]

    // X*I = I*X = X
    case (Def(DenseMatrixObjectIdentity(a)), b) if (a == b) => b.asInstanceOf[Exp[MA]]
    case (a, Def(DenseMatrixObjectIdentity(b))) if (a == b) => a.asInstanceOf[Exp[MA]]

    // else
    case _ => super.matrix_times[A,MA](x, y)
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

  */
}


trait ScalaGenMatrixOps extends ScalaGenBase {
  val IR: MatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    //case m@SymmetricMatrixObjectNew(n) => emitValDef(sym, "new generated.scala.SymmetricMatrixImpl[" + remap(m.m) + "](" + quote(n) + ")")
    //case MatrixVView(x,start,stride,length,isRow) => emitValDef(sym, quote(x) + ".vview(" + quote(start) + "," + quote(stride) + "," + quote(length) + "," + quote(isRow) + ")")
    //case MatrixApply(x,i,j) => emitValDef(sym, quote(x) + "(" + quote(i) + ", " + quote(j) + ")")
    //case MatrixGetRow(x,i) => emitValDef(sym, quote(x) + ".getRow(" + quote(i) + ")")
    //case MatrixGetCol(x,j) => emitValDef(sym, quote(x) + ".getCol(" + quote(j) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenMatrixOps extends CudaGenBase with CudaGenDataStruct {
  val IR: MatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

	  /* The ops that call through to the underlying data structure */
    //case MatrixDCApply(x,i) =>
    //  emitValDef(sym, "%s.dcApply(%s)".format(quote(x),quote(i)))
    //case MatrixApply(x,i,j) =>
    //  emitValDef(sym, "%s.apply(%s,%s)".format(quote(x),quote(i),quote(j)))
    // case MatrixUpdate(x,i,j,y)  =>
    //   stream.println(addTab() + "%s.update(%s,%s,%s);".format(quote(x),quote(i),quote(j),quote(y)))
    // case MatrixNumRows(x)  =>
    //   emitValDef(sym, quote(x) + ".numRows")
    // case MatrixNumCols(x)  =>
    //   emitValDef(sym, quote(x) + ".numCols")

    /* Specialized CUDA code generations for DeliteOpSingleTasks */
    case MatrixUpdateRow(x, row, y) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->length".format(quote(y.ops.elem)))
      stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(y.ops.elem)))
      tabWidth += 1
      stream.println(addTab()+"%s.update(%s,%s,%s.apply(%s));".format(quote(x.ops.elem),quote(row),currDimStr,quote(y.ops.elem),currDimStr))
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
      setCurrDimLength("%s->size()".format(quote(x.ops.elem)))
      stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(x.ops.elem)))
      tabWidth += 1
      stream.println(addTab()+"int i = %s / %s.numCols;".format(currDimStr,quote(x.ops.elem)))
      stream.println(addTab()+"int j = " + currDimStr + " % " + "%s.numCols;".format(quote(x.ops.elem)))
      stream.println(addTab()+"%s.update(j, i, %s.apply(i,j));".format(quote(sym),quote(x.ops.elem)))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s.numCols".format(quote(x.ops.elem)),"%s.numRows".format(quote(x.ops.elem)),false)
      currDim -= 1

    case MatrixSumCol(x) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->numCols".format(quote(x.ops.elem)))
      stream.println(addTab()+"if( %s < %s.numCols ) {".format(currDimStr,quote(x.ops.elem)))
      tabWidth += 1
      stream.println(addTab()+"%s reducVal = 0;".format(remap(x.ops.elem.Type.typeArguments(0))))
      stream.println(addTab()+"for(int i=0; i<%s.numRows; i++) {".format(quote(x.ops.elem)))
      tabWidth += 1
      stream.println(addTab()+"reducVal += %s.apply(i,%s);".format(quote(x.ops.elem),currDimStr))
      tabWidth -= 1
      stream.println(addTab()+"}")
      stream.println(addTab()+"%s.update(%s,reducVal);".format(quote(sym),currDimStr))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitVectorAlloc(sym,"%s.numCols".format(quote(x.ops.elem)),"true",false)
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
   
    /* The ops that call through to the underlying data structure */
    //case MatrixDCApply(x,i) =>
    //  emitValDef(sym, "%s.dcApply(%s)".format(quote(x),quote(i)))
    //case MatrixApply(x,i,j) =>
    //  emitValDef(sym, "%s.apply(%s,%s)".format(quote(x),quote(i),quote(j)))
    // case MatrixUpdate(x,i,j,y)  =>
    //       stream.println(addTab() + "%s.update(%s,%s,%s);".format(quote(x),quote(i),quote(j),quote(y)))
    //     case MatrixNumRows(x)  =>
    //       emitValDef(sym, quote(x) + ".numRows")
    //     case MatrixNumCols(x)  =>
    //       emitValDef(sym, quote(x) + ".numCols")
    
    /* Specialized CUDA code generations for DeliteOpSingleTasks */
    case MatrixUpdateRow(x, row, y) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->length".format(quote(y.ops.elem)))
      stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(y.ops.elem)))
      tabWidth += 1
      stream.println(addTab()+"%s.update(%s,%s,%s.apply(%s));".format(quote(x.ops.elem),quote(row),currDimStr,quote(y.ops.elem),currDimStr))
      tabWidth -= 1
      stream.println(addTab()+"}")
      currDim -= 1

    case MatrixTranspose(x) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->size()".format(quote(x.ops.elem)))
      stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(x.ops.elem)))
      tabWidth += 1
      stream.println(addTab()+"int i = %s / %s.numCols;".format(currDimStr,quote(x.ops.elem)))
      stream.println(addTab()+"int j = " + currDimStr + " % " + "%s.numCols;".format(quote(x.ops.elem)))
      stream.println(addTab()+"%s.update(j, i, %s.apply(i,j));".format(quote(sym),quote(x.ops.elem)))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s.numCols".format(quote(x.ops.elem)),"%s.numRows".format(quote(x.ops.elem)),false)
      currDim -= 1

    case MatrixSumCol(x) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->numCols".format(quote(x.ops.elem)))
      stream.println(addTab()+"if( %s < %s.numCols ) {".format(currDimStr,quote(x.ops.elem)))
      tabWidth += 1
      stream.println(addTab()+"%s reducVal = 0;".format(remap(x.ops.elem.Type.typeArguments(0))))
      stream.println(addTab()+"for(int i=0; i<%s.numRows; i++) {".format(quote(x.ops.elem)))
      tabWidth += 1
      stream.println(addTab()+"reducVal += %s.apply(i,%s);".format(quote(x.ops.elem),currDimStr))
      tabWidth -= 1
      stream.println(addTab()+"}")
      stream.println(addTab()+"%s.update(%s,reducVal);".format(quote(sym),currDimStr))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitVectorAlloc(sym,"%s.numCols".format(quote(x.ops.elem)),"true",false)
      currDim -= 1
  
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMatrixOps extends CGenBase {
  val IR: MatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

    case MatrixGetRow(x,i) =>
      stream.println("Vector<%s> %s;".format(remap(sym.Type.typeArguments(0)),quote(sym)))
      stream.println("%s.len = %s.numCols;".format(quote(sym),quote(x.ops.elem)))
      stream.println("%s.isRow = true;".format(quote(sym)))
      stream.println("%s.data = %s.data+%s.numCols*%s;".format(quote(sym),quote(x.ops.elem),quote(x.ops.elem),quote(i)))
    //case MatrixDCApply(x,i) =>
    //  emitValDef(sym, "%s.apply(%s)".format(quote(x),quote(i)))
    //case MatrixApply(x,i,j) =>
    //  emitValDef(sym, "%s.apply(%s,%s)".format(quote(x),quote(i),quote(j)))
    // case MatrixUpdate(x,i,j,y)  =>
    //   stream.println("%s.update(%s,%s,%s);".format(quote(x),quote(i),quote(j),quote(y)))
    // case MatrixNumRows(x)  =>
    //   emitValDef(sym, quote(x) + ".numRows")
    // case MatrixNumCols(x)  =>
    //   emitValDef(sym, quote(x) + ".numCols")
    // case MatrixInsertRow(x, pos, y)  =>
    //   stream.println("%s.data = (%s *)realloc(%s.data,sizeof(%s)*(%s.numRows+1)*%s.numCols);".format(quote(x),remap(x.Type.typeArguments(0)),quote(x),remap(x.Type.typeArguments(0)),quote(x),quote(x)))
    //   stream.println("memcpy(%s.data+%s*%s.numCols,%s.data,sizeof(%s)*%s.length);".format(quote(x),quote(pos),quote(x),quote(y),remap(x.Type.typeArguments(0)),quote(y)))
    //   stream.println("%s.numRows += 1;".format(quote(x)))
    //   stream.println("%s %s = %s;".format(remap(sym.Type),quote(sym),quote(x)))
    case _ => super.emitNode(sym, rhs)
  }
}
