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
    def apply[A:Manifest](numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_new(numRows, numCols)
    def apply[A:Manifest](xs: Rep[DenseVector[DenseVector[A]]])(implicit ctx: SourceContext): Rep[DenseMatrix[A]] = densematrix_obj_fromvec(xs)
    def apply[A](xs: Rep[DenseVector[VectorView[A]]])(implicit mA: Manifest[A], o: Overloaded1, ctx: SourceContext): Rep[Matrix[A]] = densematrix_obj_fromvec(xs.asInstanceOf[Rep[DenseVector[DenseVector[A]]]])
    def apply[A:Manifest](xs: Rep[DenseVector[A]]*)(implicit ctx: SourceContext): Rep[DenseMatrix[A]] = DenseMatrix(DenseVector(xs: _*))

    def dense[A:Manifest](numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_new(numRows, numCols)
    //def sparse[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]) = sparsematrix_obj_new(numRows, numCols)   
    
    def diag[A:Manifest](w: Rep[Int], vals: Interface[Vector[A]])(implicit ctx: SourceContext) = DenseMatrix.diag[A](w,vals)
    def identity(w: Rep[Int])(implicit ctx: SourceContext) = DenseMatrix.identity(w)
    def zeros(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = DenseMatrix.zeros(numRows,numCols)
    def zerosf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = DenseMatrix.zerosf(numRows,numCols)
    def mzerosf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = DenseMatrix.mzerosf(numRows,numCols)
    def ones(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = DenseMatrix.ones(numRows,numCols)
    def onesf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = DenseMatrix.onesf(numRows,numCols)
    def rand(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = DenseMatrix.rand(numRows,numCols)
    def randf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = DenseMatrix.randf(numRows,numCols)
    def randn(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = DenseMatrix.randn(numRows,numCols)
    def randnf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = DenseMatrix.randnf(numRows,numCols)
    def mrandnf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = DenseMatrix.mrandnf(numRows,numCols)
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
    implicit def builder[B:Manifest](implicit ctx: SourceContext): MatrixBuilder[B,M[B]]        
    implicit def mV[B:Manifest]: Manifest[V[B]]           
    implicit def vecToIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]]            
    implicit def vecBuilder[B:Manifest](implicit ctx: SourceContext): VectorBuilder[B,V[B]]    
        
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
    def reduceRows(f: (Rep[VA],Rep[VectorView[A]]) => Rep[VA])(implicit ctx: SourceContext): Rep[VA] //= matrix_reducerows[A,VA](x,f)
    def vview(start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext): Rep[VectorView[A]] //= matrix_vview(x,start,stride,length,isRow)
    
    // conversions
    def toBoolean(implicit conv: Rep[A] => Rep[Boolean]) = map(e => conv(e))
    def toDouble(implicit conv: Rep[A] => Rep[Double]) = map(e => conv(e))
    def toFloat(implicit conv: Rep[A] => Rep[Float]) = map(e => conv(e))
    def toInt(implicit conv: Rep[A] => Rep[Int]) = map(e => conv(e))
    //def toLong(implicit conv: Rep[A] => Rep[Long]) = map(e => conv(e))

    // accessors
    def apply(i: Rep[Int])(implicit ctx: SourceContext): Rep[VectorView[A]] = getRow(i)
    def size(implicit ctx: SourceContext): Rep[Int] = matrix_size(x)
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
    def :+(y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[MA] = matrix_addrow[A,MA](x,y)    
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
    def dcSize(implicit ctx: SourceContext): Rep[Int] = intf.ops.dcSize
    def dcApply(n: Rep[Int])(implicit ctx: SourceContext): Rep[A] = intf.ops.dcApply(n)
    def dcUpdate(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit] = intf.ops.dcUpdate(n,y)
    
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
    def :+(y: Interface[Vector[A]])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.:+(y))
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
  def matrix_size[A:Manifest](x: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[Int]
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
  def matrix_addrow[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], y: Interface[Vector[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
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
    extends DeliteOpSingleWithManifest[A,VectorView[A]](reifyEffectsHere(matrix_getrow_impl(x,i)))
  
  case class MatrixGetCol[A:Manifest](x: Interface[Matrix[A]], i: Exp[Int]) 
    extends DeliteOpSingleWithManifest[A,VectorView[A]](reifyEffectsHere(matrix_getcol_impl(x,i)))

  case class MatrixSlice[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], startRow: Exp[Int], endRow: Exp[Int], startCol: Exp[Int], endCol: Exp[Int])(implicit val b: MatrixBuilder[A,MA])
    extends DeliteOpSingleWithManifest[A,MA](reifyEffectsHere(matrix_slice_impl[A,MA](x,startRow,endRow,startCol,endCol)))

  case class MatrixSliceRows[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], start: Exp[Int], end: Exp[Int])(implicit val b: MatrixBuilder[A,MA])
    extends DeliteOpSingleWithManifest[A,MA](reifyEffectsHere(matrix_slicerows_impl[A,MA](x,start,end)))
    
  case class MatrixClone[A:Manifest,MA:Manifest](x: Interface[Matrix[A]])(implicit val b: MatrixBuilder[A,MA])
    extends DeliteOpSingleWithManifest[A,MA](reifyEffectsHere(matrix_clone_impl[A,MA](x)))    

  case class MatrixAddRow[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], y: Interface[Vector[A]])(implicit val b: MatrixBuilder[A,MA])
    extends DeliteOpSingleWithManifest[A,MA](reifyEffectsHere(matrix_addrow_impl[A,MA](x,y)))

//  case class MatrixUpdateRow[A:Manifest](x: Exp[Matrix[A]], row: Exp[Int], y: Exp[Vector[A]])
//    extends DeliteOpSingleTask(reifyEffectsHere(matrix_updaterow_impl(x,row,y)))

  // this is a single task right now because of the likely early exit. should we have a delite op for this?
  case class MatrixEquals[A:Manifest](x: Interface[Matrix[A]], y: Interface[Matrix[A]])
    extends DeliteOpSingleWithManifest[A,Boolean](reifyEffectsHere(matrix_equals_impl(x,y)))

  case class MatrixPPrint[A:Manifest](x: Interface[Matrix[A]])
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(matrix_pprint_impl[A](x)))

  case class MatrixRepmat[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], i: Exp[Int], j: Exp[Int])(implicit val b: MatrixBuilder[A,MA])
    extends DeliteOpSingleWithManifest[A,MA](reifyEffectsHere(matrix_repmat_impl[A,MA](x,i,j)))

  // case class MatrixInverse[A:Manifest,MA:Manifest](x: Interface[Matrix[A]])(implicit val b: MatrixBuilder[A,MA], val conv: Exp[A] => Exp[Double])
  //   extends DeliteOpSingleWithManifest[A,MA](reifyEffectsHere(matrix_inverse_impl[A,MA](x)))

  case class MatrixMinRow[A:Manifest:Ordering:HasMinMax,VA:Manifest](x: Interface[Matrix[A]])(implicit val b: VectorBuilder[A,VA])
    extends DeliteOpSingleWithManifest[A,VA](reifyEffectsHere(matrix_minrow_impl[A,VA](x))) {
      
    val o = implicitly[Ordering[A]]
    val p = implicitly[HasMinMax[A]]
  }

  case class MatrixMaxRow[A:Manifest:Ordering:HasMinMax,VA:Manifest](x: Interface[Matrix[A]])(implicit val b: VectorBuilder[A,VA])
    extends DeliteOpSingleWithManifest[A,VA](reifyEffectsHere(matrix_maxrow_impl[A,VA](x))) {
    
    val o = implicitly[Ordering[A]]
    val p = implicitly[HasMinMax[A]]    
  }

//  case class MatrixMapRows[A:Manifest,B:Manifest](x: Exp[Matrix[A]], f: Exp[VectorView[A]] => Exp[Vector[B]])
//    extends DeliteOpSingleTask(reifyEffectsHere(matrix_maprows_impl(x,f)))

//  case class MatrixForeachRow[A:Manifest](x: Exp[Matrix[A]], f: Exp[VectorView[A]] => Exp[Unit])
//    extends DeliteOpSingleTask(reifyEffectsHere(matrix_foreachrow_impl(x,f)))

  case class MatrixFilterRows[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], pred: Exp[VectorView[A]] => Exp[Boolean])(implicit val b: MatrixBuilder[A,MA])
    extends DeliteOpSingleWithManifest[A,MA](reifyEffectsHere(matrix_filterrows_impl[A,MA](x,pred)))  

  case class MatrixGroupRowsBy[A:Manifest,K:Manifest,MA:Manifest](x: Interface[Matrix[A]], pred: Exp[VectorView[A]] => Exp[K])(implicit val b: MatrixBuilder[A,MA])
    extends DeliteOpSingleWithManifest[A,DenseVector[MA]](reifyEffects(matrix_grouprowsby_impl[A,K,MA](x,pred))) {
      
    val mK = manifest[K]
    val mMA = manifest[MA]
  }

  ///////////////////////////////////////////////////////////////////
  // BLAS enabled routines 

  // TODO: generalize this so that we can generate fused, delite parallel op, or BLAS variants
  // having separate IR nodes breaks pattern matching optimizations... 

  case class MatrixTimesVector[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]], y: Interface[Vector[A]])(implicit val b: VectorBuilder[A,VA])
    extends DeliteOpSingleWithManifest[A,VA](reifyEffectsHere(matrix_times_vector_impl[A,VA](x,y))) {

    val a = implicitly[Arith[A]]
  }

  
  // case class MatrixMultiply[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]])
  //   extends DeliteOpSingleTask(reifyEffectsHere(matrix_multiply_impl(x,y))) {
  // 
  //   def m = manifest[A]
  //   def a = implicitly[Arith[A]]
  // }

  case class MatrixSigmoid[A:Manifest,MD:Manifest](in: Interface[Matrix[A]])(implicit val conv: Exp[A] => Exp[Double], val b: MatrixBuilder[Double,MD])
    extends DeliteOpSingleWithManifest[A,MD](reifyEffectsHere(matrix_sigmoid_impl[A,MD](in))) 
    // extends DeliteOpMap[A,Double,MD] {
    // 
    //     def alloc = b.alloc(in.numRows, in.numCols)
    //     val size = in.numRows*in.numCols
    //     def func = e => (1.0/(1.0+exp(conv(e)*(-1))))
  // }  
  
  case class MatrixSigmoidF[A:Manifest,MF:Manifest](in: Interface[Matrix[A]])(implicit val conv: Exp[A] => Exp[Double], val b: MatrixBuilder[Float,MF])
    extends DeliteOpSingleWithManifest[A,MF](reifyEffectsHere(matrix_sigmoidf_impl[A,MF](in))) 
    // extends DeliteOpMap[A,Float,MF] {
    // 
    // def alloc = b.alloc(in.numRows, in.numCols)
    // val size = in.numRows*in.numCols
    // def func = e => (1.0/(1.0+exp(conv(e)*(-1)))).AsInstanceOf[Float]
  // }  
  
  // case class MatrixTranspose[A:Manifest,MA:Manifest](x: Interface[Matrix[A]])(implicit val b: MatrixBuilder[A,MA])
  //   extends DeliteOpSingleWithManifest[A,MA](reifyEffectsHere(matrix_transpose_impl[A,MA](x))) {
  //     
  //   val mMA = manifest[MA]
  // }
  
  // case class MatrixSumCol[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]])(implicit val b: VectorBuilder[A,VA])
  //   extends DeliteOpSingleTaskWithManifest[A,VA](reifyEffectsHere(matrix_sumcol_impl[A,VA](x))) {
  //     
  //   val a = implicitly[Arith[A]]
  //   val mVA = manifest[VA]
  // }
        
  
  ////////////////////////////////
  // implemented via delite ops
  
  abstract class MatrixArithmeticMap[A:Manifest:Arith,MA:Manifest](implicit val b: MatrixBuilder[A,MA]) extends DeliteOpMap[A,A,MA] {
    val intf: Interface[Matrix[A]]    
    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]    
    def alloc = b.alloc(intf.numRows, intf.numCols)
    val size = copyTransformedOrElse(_.size)(intf.size)
    
    val mA = manifest[A]
    val a = implicitly[Arith[A]]
    val mMA = manifest[MA]
  }
  
  abstract class MatrixArithmeticZipWith[A:Manifest:Arith,MA:Manifest](implicit val b: MatrixBuilder[A,MA]) extends DeliteOpZipWith[A,A,A,MA] {
    val intfA: Interface[Matrix[A]]
    val intfB: Interface[Matrix[A]]
    val inA = intfA.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Matrix[A]]]    
    def alloc = b.alloc(intfA.numRows, intfA.numCols)
    val size = copyTransformedOrElse(_.size)(intfA.size)
    
    val mA = manifest[A]
    val a = implicitly[Arith[A]]
    val mMA = manifest[MA]
  }
  
  abstract class MatrixArithmeticIndexedLoop[A:Manifest:Arith] extends DeliteOpIndexedLoop {
    val intf: Interface[Matrix[A]]    
    val size = copyTransformedOrElse(_.size)(intf.size)
    
    val mA = manifest[A]
    val a = implicitly[Arith[A]]
  }
  
  abstract class MatrixArithmeticReduce[A:Manifest:Arith] extends DeliteOpReduce[A] {
    val intf: Interface[Matrix[A]]    
    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]    
    val size = copyTransformedOrElse(_.size)(intf.size)
    
    val mA = manifest[A]
    val a = implicitly[Arith[A]]
  }
  
  case class MatrixPlus[A:Manifest:Arith,MA:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticZipWith[A,MA] {

    def func = (a,b) => a + b
  }

  case class MatrixPlusScalar[A:Manifest:Arith,MA:Manifest](intf: Interface[Matrix[A]], y: Exp[A])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticMap[A,MA] {

    def func = e => e + y
  }
  
  case class MatrixPlusWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[B]])(implicit val conv: Exp[A] => Exp[B], val b: MatrixBuilder[B,MB])
    extends DeliteOpZipWith[A,B,B,MB] {
    
    val inA = intfA.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Matrix[B]]]  
    def alloc = b.alloc(intfA.numRows, intfA.numCols)
    val size = copyTransformedOrElse(_.size)(intfA.size)  
    def func = (a,b) => conv(a) + b
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]    
    val mMB = manifest[MB]
  }
  
  case class MatrixPlusScalarWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intf: Interface[Matrix[A]], y: Exp[B])(implicit val conv: Exp[A] => Exp[B], val b: MatrixBuilder[B,MB])
    extends DeliteOpMap[A,B,MB] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    def alloc = b.alloc(intf.numRows, intf.numCols)
    val size = copyTransformedOrElse(_.size)(intf.size)  
    def func = e => conv(e) + y
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]    
    val mMB = manifest[MB]
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
  
  case class MatrixMinusWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[B]])(implicit val conv: Exp[A] => Exp[B], val b: MatrixBuilder[B,MB])
    extends DeliteOpZipWith[A,B,B,MB] {
    
    val inA = intfA.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Matrix[B]]]    
    def alloc = b.alloc(intfA.numRows, intfA.numCols)
    val size = copyTransformedOrElse(_.size)(intfA.size)
    def func = (a,b) => conv(a) - b
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]    
    val mMB = manifest[MB]  
  }
  
  case class MatrixMinusScalarWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intf: Interface[Matrix[A]], y: Exp[B])(implicit val conv: Exp[A] => Exp[B], val b: MatrixBuilder[B,MB])
    extends DeliteOpMap[A,B,MB] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    def alloc = b.alloc(intf.numRows, intf.numCols)
    val size = copyTransformedOrElse(_.size)(intf.size)
    def func = e => conv(e) - y
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]    
    val mMB = manifest[MB]    
  }
  

  case class MatrixTimes[A:Manifest:Arith,MA:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticZipWith[A,MA] {

    def func = (a,b) => a * b
  }

  case class MatrixTimesScalar[A:Manifest:Arith,MA:Manifest](intf: Interface[Matrix[A]], y: Exp[A])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticMap[A,MA] {

    def func = e => e * y
  }

  case class MatrixTimesWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[B]])(implicit val conv: Exp[A] => Exp[B], val b: MatrixBuilder[B,MB])
    extends DeliteOpZipWith[A,B,B,MB] {
    
    val inA = intfA.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Matrix[B]]]    
    def alloc = b.alloc(intfA.numRows, intfA.numCols)
    val size = copyTransformedOrElse(_.size)(intfA.size)  
    def func = (a,b) => conv(a) * b
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]    
    val mMB = manifest[MB]    
  }
  
  case class MatrixTimesScalarWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intf: Interface[Matrix[A]], y: Exp[B])(implicit val conv: Exp[A] => Exp[B], val b: MatrixBuilder[B,MB])
    extends DeliteOpMap[A,B,MB] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    def alloc = b.alloc(intf.numRows, intf.numCols)
    val size = copyTransformedOrElse(_.size)(intf.size)
    def func = e => conv(e) * y
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]    
    val mMB = manifest[MB]    
  }
  
  case class MatrixDivide[A:Manifest:Arith,MA:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[A]])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticZipWith[A,MA] {

    def func = (a,b) => a / b
  }

  case class MatrixDivideScalar[A:Manifest:Arith,MA:Manifest](intf: Interface[Matrix[A]], y: Exp[A])(implicit b: MatrixBuilder[A,MA])
    extends MatrixArithmeticMap[A,MA] {

    def func = e => e / y
  }
  
  case class MatrixDivideWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[B]])(implicit val conv: Exp[A] => Exp[B], val b: MatrixBuilder[B,MB])
    extends DeliteOpZipWith[A,B,B,MB] {
    
    val inA = intfA.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Matrix[B]]]  
    def alloc = b.alloc(intfA.numRows, intfA.numCols)
    val size = copyTransformedOrElse(_.size)(intfA.size)
    def func = (a,b) => conv(a) / b
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]    
    val mMB = manifest[MB]
  }
  
  case class MatrixDivideScalarWithConvert[A:Manifest,B:Manifest:Arith,MB:Manifest](intf: Interface[Matrix[A]], y: Exp[B])(implicit val conv: Exp[A] => Exp[B], val b: MatrixBuilder[B,MB])
    extends DeliteOpMap[A,B,MB] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    def alloc = b.alloc(intf.numRows, intf.numCols)
    val size = copyTransformedOrElse(_.size)(intf.size)  
    def func = e => conv(e) / y
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]    
    val mMB = manifest[MB]
  }
  
  
  case class MatrixSum[A:Manifest:Arith](intf: Interface[Matrix[A]]) 
    extends MatrixArithmeticReduce[A] {
      
    val zero = implicitly[Arith[A]].empty
    def func = (a,b) => a + b
  }
  
  /* this would be nice, but case class inheritance is deprecated */
  //case class MatrixSumRow[A:Manifest:Arith](x: Exp[Matrix[A]]) extends MatrixMapRowsToVec[A,A](x, row => row.sum, unit(false))  
  case class MatrixSumRow[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]])(implicit val b: VectorBuilder[A,VA])
    extends DeliteOpMap[Int,A,VA] {

    def alloc = b.alloc(x.numRows, unit(false))
    val in = copyTransformedOrElse(_.in)(unit(0)::x.numRows)
    val size = copyTransformedOrElse(_.size)(x.numRows)
    def func = i => x(i).sum
    
    val mA = manifest[A]
    val a = implicitly[Arith[A]]
    val mVA = manifest[VA]
  } 

  case class MatrixSumCol[A:Manifest:Arith,VA:Manifest](x: Interface[Matrix[A]])(implicit val b: VectorBuilder[A,VA])
    extends DeliteOpMap[Int,A,VA] {
  
    def alloc = b.alloc(x.numCols, unit(true))
    val in = copyTransformedOrElse(_.in)(unit(0)::x.numCols)
    val size = copyTransformedOrElse(_.size)(x.numCols)
    def func = i => x.getCol(i).sum
    
    val mA = manifest[A]
    val a = implicitly[Arith[A]]
    val mVA = manifest[VA]
  } 

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
    val size = copyTransformedOrElse(_.size)(intf.size)
    val zero = implicitly[HasMinMax[A]].maxValue
    def func = (a,b) => if (a < b) a else b
    
    val mA = manifest[A]
    val o = implicitly[Ordering[A]]
    val p = implicitly[HasMinMax[A]]
  }

  case class MatrixMax[A:Manifest:Ordering:HasMinMax](intf: Interface[Matrix[A]])
    extends DeliteOpReduce[A] {

    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val size = copyTransformedOrElse(_.size)(intf.size)
    val zero = implicitly[HasMinMax[A]].minValue
    def func = (a,b) => if (a > b) a else b
    
    val mA = manifest[A]
    val o = implicitly[Ordering[A]]
    val p = implicitly[HasMinMax[A]]    
  }

  case class MatrixMap[A:Manifest,B:Manifest,MB:Manifest](intf: Interface[Matrix[A]], func: Exp[A] => Exp[B])(implicit val b: MatrixBuilder[B,MB])
    extends DeliteOpMap[A,B,MB] {

    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val size = copyTransformedOrElse(_.size)(intf.size)
    def alloc = b.alloc(intf.numRows, intf.numCols)    
    
    val mA = manifest[A]
    val mB = manifest[B]
    val mMB = manifest[MB]
  }

  case class MatrixMutableMap[A:Manifest](intf: Interface[Matrix[A]], block: Exp[A] => Exp[A])
    extends DeliteOpIndexedLoop {

    val size = copyTransformedOrElse(_.size)(intf.size)
    def func = i => intf.dcUpdate(i, block(intf.dcApply(i)))
    
    val mA = manifest[A]
  }

  // case class MatrixMapRows[A:Manifest,B:Manifest](x: Exp[Matrix[A]], block: Exp[VectorView[A]] => Exp[DenseVector[B]], out: Exp[Matrix[B]])
  //   extends DeliteOpIndexedLoop {
  // 
  //   val size = x.numRows
  //   def func = i => { out(i) = block(x(i)) } // updateRow should be fused with function application
  // }

  case class MatrixForeachRow[A:Manifest](x: Interface[Matrix[A]], block: Exp[VectorView[A]] => Exp[Unit])
    extends DeliteOpIndexedLoop {

    val size = copyTransformedOrElse(_.size)(x.numRows)
    def func = i => block(x(i))
    
    val mA = manifest[A]
  }

  case class MatrixMapRowsToVec[A:Manifest,B:Manifest,VB:Manifest](x: Interface[Matrix[A]], rowFunc: Exp[VectorView[A]] => Exp[B], isRow: Exp[Boolean])(implicit val b: VectorBuilder[B,VB])
    extends DeliteOpMap[Int,B,VB] {

    def alloc = b.alloc(x.numRows, isRow)
    val in = copyTransformedOrElse(_.in)(unit(0)::x.numRows)
    val size = copyTransformedOrElse(_.size)(x.numRows)
    def func = i => rowFunc(x(i))   
    
    val mA = manifest[A]
    val mB = manifest[B]
    val mVB = manifest[VB]
  }

  case class MatrixForeach[A:Manifest](intf: Interface[Matrix[A]], func: Exp[A] => Exp[Unit])
    extends DeliteOpForeach[A] {

    val in = intf.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val size = copyTransformedOrElse(_.size)(intf.size)
    def sync = n => List()
    
    val mA = manifest[A]
  }

  case class MatrixUpdateRow[A:Manifest](x: Interface[Matrix[A]], row: Exp[Int], y: Interface[Vector[A]])
    extends DeliteOpIndexedLoop {
    
    val size = copyTransformedOrElse(_.size)(y.length) // TODO: assert y.length == x.numCols
    def func = j => { x(row,j) = y(j) } 
    
    val mA = manifest[A]
  }
  
  case class MatrixZipWith[A:Manifest,B:Manifest,R:Manifest,MR:Manifest](intfA: Interface[Matrix[A]], intfB: Interface[Matrix[B]],
                                                                         func: (Exp[A], Exp[B]) => Exp[R])(implicit val b: MatrixBuilder[R,MR])
    extends DeliteOpZipWith[A,B,R,MR] {

    val inA = intfA.ops.elem.asInstanceOf[Exp[Matrix[A]]]
    val inB = intfA.ops.elem.asInstanceOf[Exp[Matrix[B]]]
    def alloc = b.alloc(intfA.numRows, intfA.numCols)
    val size = copyTransformedOrElse(_.size)(intfA.size)
    
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
    val size = copyTransformedOrElse(_.size)(intf.size)
    val zero = unit(0)
    def func = e => unit(1)
    def reduce = (a,b) => a + b   
    
    val mA = manifest[A]
  } 

  case class MatrixTranspose[A:Manifest,MA:Manifest](x: Interface[Matrix[A]])(implicit val b: MatrixBuilder[A,MA])
    extends DeliteOpMap[Int,A,MA] {
      
    val in = copyTransformedOrElse(_.in)(unit(0)::size)
    def alloc = b.alloc(x.numCols, x.numRows)
    val size = copyTransformedOrElse(_.size)(x.size)
    def func = i => x(i%x.numRows,i/x.numRows)
  
    val mA = manifest[A]
    val mMA = manifest[MA]
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

  def matrix_size[A:Manifest](x: Interface[Matrix[A]])(implicit ctx: SourceContext) = x.numRows*x.numCols
  //def matrix_vview[A:Manifest](x: Interface[Matrix[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(MatrixVView(x, start, stride, length, isRow))
  def matrix_getrow[A:Manifest](x: Interface[Matrix[A]], i: Exp[Int])(implicit ctx: SourceContext) = reflectPure(MatrixGetRow[A](x,i))
  def matrix_getcol[A:Manifest](x: Interface[Matrix[A]], i: Exp[Int])(implicit ctx: SourceContext) = reflectPure(MatrixGetCol[A](x,i))
  def matrix_slice[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], startRow: Exp[Int], endRow: Exp[Int], startCol: Exp[Int], endCol: Exp[Int])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixSlice[A,MA](x,startRow,endRow,startCol,endCol))
  def matrix_slicerows[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], start: Exp[Int], end: Exp[Int])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixSliceRows[A,MA](x,start,end))
  def matrix_addrow[A:Manifest,MA:Manifest](x: Interface[Matrix[A]], y: Interface[Vector[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(MatrixAddRow[A,MA](x,y))
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
    //reflectEffect(MatrixForeach(x, block)) // read??
    val mf = MatrixForeach(x, block) //reflectEffect(VectorForeach(x, block)) 
    reflectEffect(mf, summarizeEffects(mf.body.asInstanceOf[DeliteForeachElem[A]].func).star andAlso Simple())  
  }
  def matrix_foreachrow[A:Manifest](x: Interface[Matrix[A]], block: Exp[VectorView[A]] => Exp[Unit])(implicit ctx: SourceContext) = {
    //reflectEffect(MatrixForeachRow(x, block)) // read??
    val mf = MatrixForeachRow(x, block) //reflectEffect(VectorForeach(x, block)) 
    reflectEffect(mf, summarizeEffects(mf.body.asInstanceOf[DeliteForeachElem[Int]].func).star andAlso Simple())      
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


  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    // delite ops
    case e@MatrixGetRow(x,i) => reflectPure(new { override val original = Some(f,e) } with MatrixGetRow(f(x),f(i))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixGetCol(x,i) => reflectPure(new { override val original = Some(f,e) } with MatrixGetCol(f(x),f(i))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixSlice(x,sr,er,sc,ec) => reflectPure(new {override val original = Some(f,e) } with MatrixSlice(f(x),f(sr),f(er),f(sc),f(ec))(e.mA,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixSliceRows(x,s,end) => reflectPure(new {override val original = Some(f,e) } with MatrixSliceRows(f(x),f(s),f(end))(e.mA,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixAddRow(x,y) => reflectPure(new {override val original = Some(f,e) } with MatrixAddRow(f(x),f(y))(e.mA,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixClone(x) => reflectPure(new {override val original = Some(f,e) } with MatrixClone(f(x))(e.mA,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixEquals(x,y) => reflectPure(new {override val original = Some(f,e) } with MatrixEquals(f(x),f(y))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixTranspose(x) => reflectPure(new {override val original = Some(f,e) } with MatrixTranspose(f(x))(e.mA,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixRepmat(x,i,j) => reflectPure(new {override val original = Some(f,e) } with MatrixRepmat(f(x),f(i),f(j))(e.mA,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixMinRow(x) => reflectPure(new {override val original = Some(f,e) } with MatrixMinRow(f(x))(e.mA,e.o,e.p,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixMaxRow(x) => reflectPure(new {override val original = Some(f,e) } with MatrixMaxRow(f(x))(e.mA,e.o,e.p,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixFilterRows(x,pred) => reflectPure(new {override val original = Some(f,e) } with MatrixFilterRows(f(x),f(pred))(e.mA,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixSumCol(x) => reflectPure(new {override val original = Some(f,e) } with MatrixSumCol(f(x))(e.mA,e.a,e.mVA,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixGroupRowsBy(x,pred) => reflectPure(new {override val original = Some(f,e) } with MatrixGroupRowsBy(f(x),f(pred))(e.mA,e.mK,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixTimesVector(x,y) => reflectPure(new {override val original = Some(f,e) } with MatrixTimesVector(f(x),f(y))(e.mA,e.a,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixSigmoid(x) => reflectPure(new {override val original = Some(f,e) } with MatrixSigmoid(f(x))(e.mA,e.mR,e.conv,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixSigmoidF(x) => reflectPure(new {override val original = Some(f,e) } with MatrixSigmoidF(f(x))(e.mA,e.mR,e.conv,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixPlus(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixPlus(f(x),f(y))(e.mA,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixPlusScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixPlusScalar(f(x),f(y))(e.mA,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixPlusWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixPlusWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixPlusScalarWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixPlusScalarWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixMinus(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixMinus(f(x),f(y))(e.mA,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixMinusScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixMinusScalar(f(x),f(y))(e.mA,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixMinusWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixMinusWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixMinusScalarWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixMinusScalarWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixTimes(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixTimes(f(x),f(y))(e.mA,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixTimesScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixTimesScalar(f(x),f(y))(e.mA,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixTimesWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixTimesWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixTimesScalarWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixTimesScalarWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixDivide(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixDivide(f(x),f(y))(e.mA,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixDivideScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixDivideScalar(f(x),f(y))(e.mA,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixDivideWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixDivideWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixDivideScalarWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixDivideScalarWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@MatrixSum(x) => reflectPure(new { override val original = Some(f,e) } with MatrixSum(f(x))(e.mA,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixSumRow(x) => reflectPure(new { override val original = Some(f,e) } with MatrixSumRow(f(x))(e.mA,e.a,e.mVA,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixAbs(x) => reflectPure(new { override val original = Some(f,e) } with MatrixAbs(f(x))(e.mA,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixExp(x) => reflectPure(new { override val original = Some(f,e) } with MatrixExp(f(x))(e.mA,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixMin(x) => reflectPure(new {override val original = Some(f,e) } with MatrixMin(f(x))(e.mA,e.o,e.p))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixMax(x) => reflectPure(new {override val original = Some(f,e) } with MatrixMax(f(x))(e.mA,e.o,e.p))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixMap(x,g) => reflectPure(new { override val original = Some(f,e) } with MatrixMap(f(x),f(g))(e.mA,e.mB,e.mMB,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixMapRowsToVec(x,g,r) => reflectPure(new { override val original = Some(f,e) } with MatrixMapRowsToVec(f(x),f(g),f(r))(e.mA,e.mB,e.mVB,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixZipWith(x,y,g) => reflectPure(new { override val original = Some(f,e) } with MatrixZipWith(f(x),f(y),f(g))(e.mA,e.mB,e.mR,e.mMR,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixCount(x,g) => reflectPure(new { override val original = Some(f,e) } with MatrixCount(f(x),f(g))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
      
    // reflected
    case Reflect(e@MatrixGetRow(x,i), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixGetRow(f(x),f(i))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixGetCol(x,i), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixGetCol(f(x),f(i))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixSlice(x,sr,er,sc,ec), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixSlice(f(x),f(sr),f(er),f(sc),f(ec))(e.mA,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixSliceRows(x,s,end), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixSliceRows(f(x),f(s),f(end))(e.mA,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixAddRow(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixAddRow(f(x),f(y))(e.mA,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixClone(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixClone(f(x))(e.mA,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixEquals(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixEquals(f(x),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixTranspose(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixTranspose(f(x))(e.mA,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixRepmat(x,i,j), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixRepmat(f(x),f(i),f(j))(e.mA,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixMinRow(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixMinRow(f(x))(e.mA,e.o,e.p,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixMaxRow(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixMaxRow(f(x))(e.mA,e.o,e.p,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixFilterRows(x,pred), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixFilterRows(f(x),f(pred))(e.mA,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixSumCol(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixSumCol(f(x))(e.mA,e.a,e.mVA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixGroupRowsBy(x,pred), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixGroupRowsBy(f(x),f(pred))(e.mA,e.mK,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixTimesVector(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixTimesVector(f(x),f(y))(e.mA,e.a,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixSigmoid(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixSigmoid(f(x))(e.mA,e.mR,e.conv,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixSigmoidF(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixSigmoidF(f(x))(e.mA,e.mR,e.conv,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixPlusEquals(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixPlusEquals(f(x),f(y))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixPlus(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixPlus(f(x),f(y))(e.mA,e.a,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixPlusScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixPlusScalar(f(x),f(y))(e.mA,e.a,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@MatrixPlusWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixPlusWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixPlusScalarWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixPlusScalarWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixMinus(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixMinus(f(x),f(y))(e.mA,e.a,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixMinusScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixMinusScalar(f(x),f(y))(e.mA,e.a,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@MatrixMinusWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixMinusWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixMinusScalarWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixMinusScalarWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixTimes(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixTimes(f(x),f(y))(e.mA,e.a,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixTimesScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixTimesScalar(f(x),f(y))(e.mA,e.a,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@MatrixTimesWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixTimesWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixTimesScalarWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixTimesScalarWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixDivide(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixDivide(f(x),f(y))(e.mA,e.a,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixDivideScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixDivideScalar(f(x),f(y))(e.mA,e.a,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@MatrixDivideWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixDivideWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixDivideScalarWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixDivideScalarWithConvert(f(x),f(y))(e.mA,e.mB,e.a,e.mMB,e.conv,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixSum(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixSum(f(x))(e.mA,e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixSumRow(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixSumRow(f(x))(e.mA,e.a,e.mVA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixAbs(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixAbs(f(x))(e.mA,e.a,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixExp(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixExp(f(x))(e.mA,e.a,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixMin(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixMin(f(x))(e.mA,e.o,e.p), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixMax(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixMax(f(x))(e.mA,e.o,e.p), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixMap(x,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixMap(f(x),f(g))(e.mA,e.mB,e.mMB,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixMapRowsToVec(x,g,r), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixMapRowsToVec(f(x),f(g),f(r))(e.mA,e.mB,e.mVB,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixZipWith(x,y,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixZipWith(f(x),f(y),f(g))(e.mA,e.mB,e.mR,e.mMR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@MatrixForeach(x,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixForeach(f(x),f(g))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@MatrixForeachRow(x,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixForeachRow(f(x),f(g))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@MatrixCount(x,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixCount(f(x),f(g))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixUpdateRow(x,r,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixUpdateRow(f(x),f(r),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixMutableMap(x,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixMutableMap(f(x),f(g))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixPPrint(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixPPrint(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
  
  
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
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenMatrixOps extends OpenCLGenBase with OpenCLGenDataStruct {
  val IR: MatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMatrixOps extends CGenBase {
  val IR: MatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
