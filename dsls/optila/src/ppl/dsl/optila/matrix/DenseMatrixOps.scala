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

trait DenseMatrixOps extends Variables {
  this: OptiLA =>

  implicit def repToDenseMatOps[A:Manifest](x: Rep[DenseMatrix[A]]) = new DenseMatOpsCls(x)
  implicit def varToDenseMatOps[A:Manifest](x: Var[DenseMatrix[A]]) = new DenseMatOpsCls(readVar(x))  
  implicit def denseMatToInterface[A:Manifest](lhs: Rep[DenseMatrix[A]]) = new MInterface[A](new DenseMatOpsCls[A](lhs))
  implicit def denseMatVarToInterface[A:Manifest](lhs: Var[DenseMatrix[A]]) = new MInterface[A](new DenseMatOpsCls[A](readVar(lhs)))
  
  implicit def denseMatrixBuilder[A:Manifest] = new MatrixBuilder[A,DenseMatrix[A]] {
    def alloc(numRows: Rep[Int], numCols: Rep[Int]) = {
      Matrix.dense[A](numRows, numCols)
    }
    def toIntf(x: Rep[DenseMatrix[A]]): Interface[Matrix[A]] = denseMatToInterface(x)
  }  

  object DenseMatrix {
    def apply[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]) = densematrix_obj_new(numRows, numCols)
    def apply[A:Manifest](xs: Rep[DenseVector[DenseVector[A]]]): Rep[DenseMatrix[A]] = densematrix_obj_fromvec(xs)
    //def apply[A:Manifest](xs: Rep[DenseVector[VectorView[A]]])(implicit o: Overloaded1): Rep[DenseMatrix[A]] = densematrix_obj_fromvec(xs.asInstanceOf[Rep[DenseVector[DenseVector[A]]]])  // AKS TODO
    def apply[A:Manifest](xs: Rep[DenseVector[A]]*): Rep[DenseMatrix[A]] = DenseMatrix(DenseVector(xs: _*))

    def diag[A:Manifest](w: Rep[Int], vals: Interface[Vector[A]]) = densematrix_obj_diag(w, vals)
    def identity(w: Rep[Int]) = densematrix_obj_identity(w)
    def zeros(numRows: Rep[Int], numCols: Rep[Int]) = densematrix_obj_zeros(numRows, numCols)
    def zerosf(numRows: Rep[Int], numCols: Rep[Int]) = densematrix_obj_zerosf(numRows, numCols)
    def mzerosf(numRows: Rep[Int], numCols: Rep[Int]) = densematrix_obj_mzerosf(numRows, numCols)
    def ones(numRows: Rep[Int], numCols: Rep[Int]) = densematrix_obj_ones(numRows, numCols)
    def onesf(numRows: Rep[Int], numCols: Rep[Int]) = densematrix_obj_onesf(numRows, numCols)
    def rand(numRows: Rep[Int], numCols: Rep[Int]) = densematrix_obj_rand(numRows, numCols)
    def randf(numRows: Rep[Int], numCols: Rep[Int]) = densematrix_obj_randf(numRows, numCols)
    def randn(numRows: Rep[Int], numCols: Rep[Int]) = densematrix_obj_randn(numRows, numCols)
    def randnf(numRows: Rep[Int], numCols: Rep[Int]) = densematrix_obj_randnf(numRows, numCols)
    def mrandnf(numRows: Rep[Int], numCols: Rep[Int]) = densematrix_obj_mrandnf(numRows, numCols)
  }

  class DenseMatOpsCls[A:Manifest](val elem: Rep[DenseMatrix[A]]) extends MatOpsCls[A] {
    type M[X] = DenseMatrix[X]
    type V[X] = DenseVector[X]
    type Self = DenseMatrix[A]

    def mA: Manifest[A] = manifest[A]
    def mM[B:Manifest]: Manifest[M[B]] = manifest[DenseMatrix[B]]    
    def wrap(x: Rep[DenseMatrix[A]]): Interface[Matrix[A]] = denseMatToInterface(x)
    def toOps[B:Manifest](x: Rep[M[B]]): MatOpsCls[B] = repToDenseMatOps[B](x)
    def toIntf[B:Manifest](x: Rep[M[B]]): Interface[Matrix[B]] = denseMatToInterface[B](x)        
    def builder[B:Manifest]: MatrixBuilder[B,M[B]] = denseMatrixBuilder[B]            
    def mV[B:Manifest]: Manifest[V[B]] = manifest[DenseVector[B]]
    def vecToIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]] = denseVecToInterface[B](x)        
    def vecBuilder[B:Manifest]: VectorBuilder[B,V[B]] = denseVectorBuilder[B]
    
    // delite collection
    def dcSize(implicit ctx: SourceContext): Rep[Int] = x.size
    def dcApply(n: Rep[Int])(implicit ctx: SourceContext): Rep[A] = densematrix_rawapply(x,n)
    def dcUpdate(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit] = densematrix_rawupdate(x,n,y)
    
    // accessors
    def apply(i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext) = densematrix_apply(x,i,j)
    def numRows(implicit ctx: SourceContext) = densematrix_numrows(x)
    def numCols(implicit ctx: SourceContext) = densematrix_numcols(x)
    def vview(start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext) = densematrix_vview(x,start,stride,length,isRow)
    
    // data operations
    def update(i: Rep[Int], j: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = densematrix_update(x,i,j,y)
    def insertRow(pos: Rep[Int], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = densematrix_insertrow(x,pos,y)
    def insertAllRows(pos: Rep[Int], y: Rep[DenseMatrix[A]])(implicit ctx: SourceContext) = densematrix_insertallrows(x,pos,y)
    def insertCol(pos: Rep[Int], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = densematrix_insertcol(x,pos,y)
    def insertAllCols(pos: Rep[Int], y: Rep[DenseMatrix[A]])(implicit ctx: SourceContext) = densematrix_insertallcols(x,pos,y)
    def removeRows(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = densematrix_removerows(x,pos,len)
    def removeCols(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = densematrix_removecols(x,pos,len)
    
    // not supported by interface right now
    def *(y: Rep[MA])(implicit a: Arith[A], ctx: SourceContext): Rep[MA] = densematrix_multiply(x,y)
    def inv(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext) = densematrix_inverse(x)    
    def mapRows[B:Manifest](f: Rep[VectorView[A]] => Rep[DenseVector[B]])(implicit ctx: SourceContext) = densematrix_maprows(x,f)
    def reduceRows(f: (Rep[VectorView[A]],Rep[VectorView[A]]) => Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[DenseVector[A]] = densematrix_reducerows(x,f)
    
    // overrides
    def *(y: Rep[DenseVector[A]])(implicit a: Arith[A], o: Overloaded1, ctx: SourceContext): Rep[DenseVector[A]] = densematrix_times_vector(x,y)
    override def sigmoid(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext): Rep[DenseMatrix[Double]] = densematrix_sigmoid(x)
    override def sigmoidf(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext): Rep[DenseMatrix[Float]] = densematrix_sigmoidf(x)

  }
  
  // object defs
  //def symmatrix_obj_new[A:Manifest](n: Rep[Int]): Rep[SymmetricMatrix[A]]
  def densematrix_obj_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[A]]
  def densematrix_obj_fromseq[A:Manifest](xs: Seq[Interface[Vector[A]]]): Rep[DenseMatrix[A]]
  def densematrix_obj_fromvec[A:Manifest](xs: Rep[DenseVector[DenseVector[A]]]): Rep[DenseMatrix[A]]
  def densematrix_obj_diag[A:Manifest](w: Rep[Int], vals: Interface[Vector[A]]): Rep[DenseMatrix[A]]
  def densematrix_obj_identity(w: Rep[Int]): Rep[DenseMatrix[Double]]
  def densematrix_obj_zeros(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Double]]
  def densematrix_obj_zerosf(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Float]]
  def densematrix_obj_mzerosf(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Float]]
  def densematrix_obj_ones(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Double]]
  def densematrix_obj_onesf(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Float]]
  def densematrix_obj_rand(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Double]]
  def densematrix_obj_randf(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Float]]
  def densematrix_obj_randn(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Double]]
  def densematrix_obj_randnf(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Float]]
  def densematrix_obj_mrandnf(numRows: Rep[Int], numCols: Rep[Int]): Rep[DenseMatrix[Float]]
  
  
  // class defs
  def densematrix_apply[A:Manifest](x: Rep[DenseMatrix[A]], i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext): Rep[A]
  def densematrix_numrows[A:Manifest](x: Rep[DenseMatrix[A]])(implicit ctx: SourceContext): Rep[Int]
  def densematrix_numcols[A:Manifest](x: Rep[DenseMatrix[A]])(implicit ctx: SourceContext): Rep[Int]
  def densematrix_vview[A:Manifest](x: Rep[DenseMatrix[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext): Rep[VectorView[A]] 

  def densematrix_update[A:Manifest](x: Rep[DenseMatrix[A]], i: Rep[Int], j: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_insertrow[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], y: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_insertallrows[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], y: Rep[DenseMatrix[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_insertcol[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], y: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_insertallcols[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], y: Rep[DenseMatrix[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_removerows[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_removecols[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  
  def densematrix_multiply[A:Manifest:Arith](x: Rep[DenseMatrix[A]], y: Rep[DenseMatrix[A]])(implicit ctx: SourceContext): Rep[DenseMatrix[A]]
  def densematrix_times_vector[A:Manifest:Arith](x: Rep[DenseMatrix[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[DenseVector[A]]
  def densematrix_inverse[A:Manifest](x: Rep[DenseMatrix[A]])(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext): Rep[DenseMatrix[Double]]  
  def densematrix_sigmoid[A:Manifest](x: Rep[DenseMatrix[A]])(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext): Rep[DenseMatrix[Double]]
  def densematrix_sigmoidf[A:Manifest](x: Rep[DenseMatrix[A]])(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext): Rep[DenseMatrix[Float]]
  def densematrix_maprows[A:Manifest,B:Manifest](x: Rep[DenseMatrix[A]], f: Rep[VectorView[A]] => Rep[DenseVector[B]])(implicit ctx: SourceContext): Rep[DenseMatrix[B]] 
  def densematrix_reducerows[A:Manifest](x: Rep[DenseMatrix[A]], f: (Rep[VectorView[A]],Rep[VectorView[A]]) => Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[DenseVector[A]]   
  
  def densematrix_size[A:Manifest](x: Rep[DenseMatrix[A]])(implicit ctx: SourceContext): Rep[Int]
  def densematrix_rawapply[A:Manifest](x: Rep[DenseMatrix[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[A]
  def densematrix_rawupdate[A:Manifest](x: Rep[DenseMatrix[A]], n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
}

trait DenseMatrixCompilerOps extends DenseMatrixOps {
  this: OptiLA =>
  
  def densematrix_raw_data[A:Manifest](x: Rep[DenseMatrix[A]])(implicit ctx: SourceContext): Rep[Array[A]]
  def densematrix_set_raw_data[A:Manifest](x: Rep[DenseMatrix[A]], data: Rep[Array[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_set_numrows[A:Manifest](x: Rep[DenseMatrix[A]], newVal: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_set_numcols[A:Manifest](x: Rep[DenseMatrix[A]], newVal: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
}

trait DenseMatrixOpsExp extends DenseMatrixCompilerOps with DeliteCollectionOpsExp with VariablesExp {
  this: DenseMatrixImplOps with OptiLAExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure
  
  case class DenseMatrixObjectNew[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) extends DefWithManifest[A,DenseMatrix[A]] 
  case class DenseMatrixRawData[A:Manifest](x: Exp[DenseMatrix[A]]) extends DefWithManifest[A,Array[A]]
  case class DenseMatrixNumRows[A:Manifest](x: Exp[DenseMatrix[A]]) extends DefWithManifest[A,Int] 
  case class DenseMatrixNumCols[A:Manifest](x: Exp[DenseMatrix[A]]) extends DefWithManifest[A,Int]
  case class DenseMatrixSetNumRows[A:Manifest](x: Exp[DenseMatrix[A]], newVal: Exp[Int]) extends DefWithManifest[A,Unit]
  case class DenseMatrixSetNumCols[A:Manifest](x: Exp[DenseMatrix[A]], newVal: Exp[Int]) extends DefWithManifest[A,Unit]
  case class DenseMatrixSetRawData[A:Manifest](x: Exp[DenseMatrix[A]], data: Exp[Array[A]]) extends DefWithManifest[A,Unit]
    
  /////////////////////////////////////
  // implemented via kernel embedding

  case class DenseMatrixObjectFromSeq[A:Manifest](xs: Seq[Interface[Vector[A]]])
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_obj_fromseq_impl(xs))) 

  case class DenseMatrixObjectFromVec[A:Manifest](xs: Rep[DenseVector[DenseVector[A]]])
    extends DeliteOpSingleWithManifest[A,DenseMatrix[A]](reifyEffectsHere(densematrix_obj_fromvec_impl(xs)))

  case class DenseMatrixObjectDiag[A:Manifest](w: Exp[Int], vals: Interface[Vector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_obj_diag_impl(w, vals))) 

  case class DenseMatrixObjectIdentity(w: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_obj_identity_impl(w)))

  //case class DenseMatrixObjectZeros(numRows: Exp[Int], numCols: Exp[Int])
  //  extends DeliteOpSingleTask(reifyEffectsHere(densematrix_obj_zeros_impl(numRows, numCols)))

  //case class DenseMatrixObjectZerosF(numRows: Exp[Int], numCols: Exp[Int])
  //  extends DeliteOpSingleTask(reifyEffectsHere(densematrix_obj_zerosf_impl(numRows, numCols)))

  case class DenseMatrixObjectOnes(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_obj_ones_impl(numRows, numCols)))

  case class DenseMatrixObjectOnesF(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_obj_onesf_impl(numRows, numCols)))

  case class DenseMatrixObjectRand(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_obj_rand_impl(numRows, numCols)))

  case class DenseMatrixObjectRandF(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_obj_randf_impl(numRows, numCols)))

  case class DenseMatrixObjectRandn(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_obj_randn_impl(numRows, numCols)))

  case class DenseMatrixObjectRandnF(numRows: Exp[Int], numCols: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_obj_randnf_impl(numRows, numCols)))

  case class DenseMatrixVView[A:Manifest](x: Exp[DenseMatrix[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_vview_impl(x, start, stride, length, isRow)))

  case class DenseMatrixApply[A:Manifest](x: Exp[DenseMatrix[A]], i: Exp[Int], j: Exp[Int])
    extends DeliteOpSingleWithManifest[A,A](reifyEffectsHere(densematrix_apply_impl(x, i, j)))

  case class DenseMatrixRawApply[A:Manifest](x: Exp[DenseMatrix[A]], i: Exp[Int])
    extends DefWithManifest[A,A]
    //extends DeliteOpSingleWithManifest[A,A](reifyEffectsHere(densematrix_rawapply_impl(x,i))) 
    
  case class DenseMatrixUpdate[A:Manifest](x: Exp[DenseMatrix[A]], i: Exp[Int], j: Exp[Int], y: Exp[A]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densematrix_update_impl(x,i,j,y)))
    
  case class DenseMatrixRawUpdate[A:Manifest](x: Exp[DenseMatrix[A]], i: Exp[Int], y: Exp[A])
    extends DefWithManifest[A,Unit]
    //extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densematrix_rawupdate_impl(x,i,y)))

  case class DenseMatrixInsertRow[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Exp[DenseVector[A]]) 
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_insertrow_impl(x,pos,y)))
    
  case class DenseMatrixInsertAllRows[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Exp[DenseMatrix[A]]) 
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_insertallrows_impl(x,pos,y)))
    
  case class DenseMatrixInsertCol[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Exp[DenseVector[A]]) 
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_insertcol_impl(x,pos,y)))
    
  case class DenseMatrixInsertAllCols[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Exp[DenseMatrix[A]]) 
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_insertallcols_impl(x,pos,y)))
    
  case class DenseMatrixRemoveRows[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_removerows_impl(x,pos,len)))
    
  case class DenseMatrixRemoveCols[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_removecols_impl(x,pos,len)))

//  case class DenseMatrixUpdateRow[A:Manifest](x: Exp[DenseMatrix[A]], row: Exp[Int], y: Exp[Vector[A]])
//    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_updaterow_impl(x,row,y)))

  // this is a single task right now because of the likely early exit. should we have a delite op for this?
  // case class DenseMatrixEquals[A:Manifest](x: Exp[DenseMatrix[A]], y: Exp[DenseMatrix[A]])
  //   extends DeliteOpSingleTask(reifyEffectsHere(densematrix_equals_impl[A](x,y)))
    
  case class DenseMatrixInverse[A:Manifest](x: Exp[DenseMatrix[A]])(implicit val conv: Exp[A] => Exp[Double])
    extends DeliteOpSingleWithManifest[A,DenseMatrix[Double]](reifyEffectsHere(densematrix_inverse_impl(x))) 

  ///////////////////////////////////////////////////////////////////
  // BLAS enabled routines 

  // TODO: generalize this so that we can generate fused, delite parallel op, or BLAS variants
  // having separate IR nodes breaks pattern matching optimizations... 

  case class DenseMatrixMultiply[A:Manifest:Arith](x: Exp[DenseMatrix[A]], y: Exp[DenseMatrix[A]])
    extends DeliteOpSingleWithManifest[A,DenseMatrix[A]](reifyEffectsHere(densematrix_multiply_impl(x,y))) {
    
    val a = implicitly[Arith[A]]
  }

  case class DenseMatrixMultiplyBLAS[A:Manifest:Arith](x: Exp[DenseMatrix[A]], y: Exp[DenseMatrix[A]]) extends DeliteOpExternal[DenseMatrix[A]] {
    def alloc = DenseMatrix[A](x.numRows, y.numCols)
    val funcName = "matMult"

    val m = manifest[A]
    val a = implicitly[Arith[A]]
  }

  case class DenseMatrixTimesVectorBLAS[A:Manifest:Arith](x: Exp[DenseMatrix[A]], y: Exp[DenseVector[A]]) extends DeliteOpExternal[DenseVector[A]] {
    def alloc = Vector[A](x.numRows, unit(false))
    val funcName = "matMultV"

    val m = manifest[A]
    val a = implicitly[Arith[A]]    
  }
  
  case class DenseMatrixSigmoidVectorized[A:Manifest](in: Exp[DenseMatrix[A]]) extends DeliteOpExternal[DenseMatrix[A]] {
    def alloc = DenseMatrix[A](in.numRows, in.numCols)    
    val funcName = "matSigmoid"
    val m = manifest[A]
  }

  ////////////////////////////////
  // implemented via delite ops
  
  case class DenseMatrixMapRows[A:Manifest,B:Manifest](x: Exp[DenseMatrix[A]], block: Exp[VectorView[A]] => Exp[DenseVector[B]], out: Exp[DenseMatrix[B]])
    extends DeliteOpIndexedLoop {

    val size = x.numRows
    def func = i => { out(i) = block(x(i)) } // updateRow should be fused with function application
  }

  // More efficient (though slightly uglier) to express this as a loop directly. 
  // TODO: nicer DeliteOpLoop templates? e.g. DeliteOpReductionLoop, ...
  // case class DenseMatrixReduceRows[A:Manifest](x: Exp[DenseMatrix[A]], func: (Exp[VectorView[A]], Exp[DenseVector[A]]) => Exp[DenseVector[A]])
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


  /////////////////////
  // delite collection
  
  def isDenseMatrix[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.Type.erasure,classOf[DenseMatrix[A]])  
  def asDenseMatrix[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[DenseMatrix[A]]]
  
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isDenseMatrix(x)) densematrix_size(asDenseMatrix(x))
    else super.dc_size(x)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isDenseMatrix(x)) densematrix_rawapply(asDenseMatrix(x),n)
    else super.dc_apply(x,n)    
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isDenseMatrix(x)) densematrix_rawupdate(asDenseMatrix(x),n,y)
    else super.dc_update(x,n,y)        
  }
  
  
  ////////////////////
  // object interface

  //def symdensematrix_obj_new[A:Manifest](n: Exp[Int]) = reflectMutable(SymmetricDenseMatrixObjectNew[A](n))
  def densematrix_obj_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) = reflectMutable(DenseMatrixObjectNew[A](numRows, numCols)) //XXX
  def densematrix_obj_fromseq[A:Manifest](xs: Seq[Interface[Vector[A]]]) = reflectPure(DenseMatrixObjectFromSeq(xs)) //XXX
  def densematrix_obj_fromvec[A:Manifest](xs: Rep[DenseVector[DenseVector[A]]]) = reflectPure(DenseMatrixObjectFromVec(xs))
  def densematrix_obj_diag[A:Manifest](w: Exp[Int], vals: Interface[Vector[A]]) = reflectPure(DenseMatrixObjectDiag(w, vals))
  def densematrix_obj_identity(w: Exp[Int]) = reflectPure(DenseMatrixObjectIdentity(w))
  def densematrix_obj_zeros(numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(DenseMatrixObjectNew[Double](numRows, numCols))//DenseMatrixObjectZeros(numRows, numCols))
  def densematrix_obj_zerosf(numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(DenseMatrixObjectNew[Float](numRows, numCols))//DenseMatrixObjectZerosF(numRows, numCols))
  def densematrix_obj_mzerosf(numRows: Exp[Int], numCols: Exp[Int]) = reflectMutable(DenseMatrixObjectNew[Float](numRows, numCols))//reflectPure(DenseMatrixObjectZerosF(numRows, numCols))
  def densematrix_obj_ones(numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(DenseMatrixObjectOnes(numRows, numCols))
  def densematrix_obj_onesf(numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(DenseMatrixObjectOnesF(numRows, numCols))
  def densematrix_obj_rand(numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(DenseMatrixObjectRand(numRows, numCols))
  def densematrix_obj_randf(numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(DenseMatrixObjectRandF(numRows, numCols))
  def densematrix_obj_randn(numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(DenseMatrixObjectRandn(numRows, numCols))
  def densematrix_obj_randnf(numRows: Rep[Int], numCols: Rep[Int]) = reflectPure(DenseMatrixObjectRandnF(numRows, numCols))
  def densematrix_obj_mrandnf(numRows: Rep[Int], numCols: Rep[Int]) = reflectMutable(DenseMatrixObjectRandnF(numRows, numCols)) //TR was reflectPure (why?)


  ///////////////////
  // class interface

  def densematrix_apply[A:Manifest](x: Exp[DenseMatrix[A]], i: Exp[Int], j: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DenseMatrixApply[A](x,i,j))
  def densematrix_numrows[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(DenseMatrixNumRows(x))
  def densematrix_numcols[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(DenseMatrixNumCols(x))
  def densematrix_vview[A:Manifest](x: Exp[DenseMatrix[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(DenseMatrixVView(x,start,stride,length,isRow))

  def densematrix_update[A:Manifest](x: Exp[DenseMatrix[A]], i: Exp[Int], j: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixUpdate[A](x,i,j,y))
  def densematrix_insertrow[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixInsertRow(x,pos,y))
  def densematrix_insertallrows[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixInsertAllRows(x,pos,y))
  def densematrix_insertcol[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixInsertCol(x,pos,y))
  def densematrix_insertallcols[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixInsertAllCols(x,pos,y))
  def densematrix_removerows[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixRemoveRows(x,pos,len))
  def densematrix_removecols[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixRemoveCols(x,pos,len))
  def densematrix_multiply[A:Manifest:Arith](x: Exp[DenseMatrix[A]], y: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = {
    if (Config.useBlas && (manifest[A] == manifest[Double] || manifest[A] == manifest[Float])) reflectPure(DenseMatrixMultiplyBLAS(x,y))
    else reflectPure(DenseMatrixMultiply(x,y))
  }
  def densematrix_times_vector[A:Manifest:Arith](x: Exp[DenseMatrix[A]], y: Exp[DenseVector[A]])(implicit ctx: SourceContext) = {
    if (Config.useBlas && (manifest[A] == manifest[Double] || manifest[A] == manifest[Float])) reflectPure(DenseMatrixTimesVectorBLAS(x,y))
    else reflectPure(MatrixTimesVector[A,DenseVector[A]](x,y))
  }
  
  def densematrix_inverse[A:Manifest](x: Exp[DenseMatrix[A]])(implicit conv: Exp[A] => Exp[Double], ctx: SourceContext) = reflectPure(DenseMatrixInverse(x))
  
  def densematrix_sigmoid[A:Manifest](x: Exp[DenseMatrix[A]])(implicit conv: Exp[A] => Exp[Double], ctx: SourceContext) = {
    if (Config.useBlas && manifest[A] == manifest[Double]) reflectPure(DenseMatrixSigmoidVectorized(x.asInstanceOf[Exp[DenseMatrix[Double]]]))    
    else reflectPure(MatrixSigmoid[A,DenseMatrix[Double]](x))
  }
  def densematrix_sigmoidf[A:Manifest](x: Exp[DenseMatrix[A]])(implicit conv: Exp[A] => Exp[Double], ctx: SourceContext) = {
    if (Config.useBlas && manifest[A] == manifest[Float]) reflectPure(DenseMatrixSigmoidVectorized(x.asInstanceOf[Exp[DenseMatrix[Float]]]))    
    else reflectPure(MatrixSigmoidF[A,DenseMatrix[Float]](x))
  }
  
  def densematrix_maprows[A:Manifest,B:Manifest](x: Exp[DenseMatrix[A]], f: Exp[VectorView[A]] => Exp[DenseVector[B]])(implicit ctx: SourceContext) = {
    val out = Matrix.dense[B](x.numRows, x.numCols)
    reflectWrite(out)(DenseMatrixMapRows(x,f,out))
    out.unsafeImmutable // will this work?
  }  
  def densematrix_reducerows[A:Manifest](x: Exp[DenseMatrix[A]], f: (Exp[VectorView[A]],Exp[VectorView[A]]) => Exp[DenseVector[A]])(implicit ctx: SourceContext) = {
    //reflectPure(DenseMatrixReduceRows(x, f))
    throw new UnsupportedOperationException("temporarily removed until new DeliteOpReduce[A,R] is supported")  // TODO AKS
  }
  

  //////////////////
  // internal

  def densematrix_size[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = x.numRows * x.numCols
  def densematrix_rawapply[A:Manifest](x: Exp[DenseMatrix[A]], n: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DenseMatrixRawApply(x,n))//reflectPure(DeliteCollectionApply(x,n))//matrix_raw_data(x).apply(n)  // AKS TODO
  def densematrix_rawupdate[A:Manifest](x: Exp[DenseMatrix[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixRawUpdate(x,n,y))///*reflectWrite(x)*/reflectPure(DeliteCollectionUpdate(x,n,y))//matrix_raw_data(x).update(n,y)  // AKS TODO  
  def densematrix_raw_data[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(DenseMatrixRawData(x))//reflectMutable(DenseMatrixRawData(x.unsafeImmutable))  
  def densematrix_set_numrows[A:Manifest](x: Exp[DenseMatrix[A]], newVal: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixSetNumRows(x,newVal))
  def densematrix_set_numcols[A:Manifest](x: Exp[DenseMatrix[A]], newVal: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixSetNumCols(x,newVal))
  def densematrix_set_raw_data[A:Manifest](x: Exp[DenseMatrix[A]], data: Exp[Array[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixSetRawData(x,data))

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = {
    (e match {
      case e@DenseMatrixObjectIdentity(x) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectIdentity(f(x)))(mtype(manifest[A]),implicitly[SourceContext])
      case e@DenseMatrixObjectFromVec(x) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectFromVec(f(x))(e.m))(mtype(manifest[A]),implicitly[SourceContext])
      //case e@DenseMatrixObjectDiag(x,y) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectDiag(f(x),f(y))(e.m))(mtype(manifest[A]))
      //case e@DenseMatrixTimesVector(x,y) => reflectPure(new {override val original = Some(f,e) } with DenseMatrixTimesVector(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
      case e@DenseMatrixTimesVectorBLAS(x,y) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixTimesVectorBLAS(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
      case e@DenseMatrixMultiply(x,y) => reflectPure(new {override val original = Some(f,e) } with DenseMatrixMultiply(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
      case e@DenseMatrixMultiplyBLAS(x,y) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixMultiplyBLAS(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
      case e@DenseMatrixInverse(x) => reflectPure(new {override val original = Some(f,e) } with DenseMatrixInverse(f(x))(e.m,f(e.conv)))(mtype(manifest[A]),implicitly[SourceContext])
      
      case DenseMatrixNumRows(x) => densematrix_numrows(f(x))
      case DenseMatrixNumCols(x) => densematrix_numcols(f(x))
      case e@DenseMatrixRawData(x) => densematrix_raw_data(f(x))(e.m, implicitly[SourceContext])
      case e@DenseMatrixRawApply(x,n) => densematrix_rawapply(f(x),f(n))(e.m, implicitly[SourceContext])
      case e@DenseMatrixApply(x,i,j) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixApply(f(x),f(i),f(j)))(mtype(manifest[A]),implicitly[SourceContext])
      // reflected
      case Reflect(e@DenseMatrixRawData(x), u, es) => reflectMirrored(Reflect(DenseMatrixRawData(f(x))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DenseMatrixRawApply(x,n), u, es) => reflectMirrored(Reflect(DenseMatrixRawApply(f(x),f(n))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DenseMatrixNumRows(x), u, es) => reflectMirrored(Reflect(DenseMatrixNumRows(f(x))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DenseMatrixNumCols(x), u, es) => reflectMirrored(Reflect(DenseMatrixNumCols(f(x))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DenseMatrixApply(x,i,j), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixApply(f(x),f(i),f(j))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DenseMatrixUpdate(x,i,j,r), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixUpdate(f(x),f(i),f(j),f(r))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DenseMatrixInsertAllCols(x,y,z), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixInsertAllCols(f(x),f(y),f(z)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DenseMatrixRemoveCols(x,y,z), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixRemoveCols(f(x),f(y),f(z)), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case Reflect(e@DenseMatrixObjectNew(x,y), u, es) => reflectMirrored(Reflect(DenseMatrixObjectNew(f(x),f(y))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
      case _ => super.mirror(e, f)
    }).asInstanceOf[Exp[A]] // why??
  }  
  
  /////////////////////
  // aliases and sharing

  // TODO: precise sharing info for other IR types (default is conservative)

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case DenseMatrixMultiply(a,b) => Nil
    //case DenseMatrixTimesVector(a,v) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case DenseMatrixMultiply(a,b) => Nil
    //case DenseMatrixTimesVector(a,v) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case DenseMatrixMultiply(a,b) => Nil
    //case DenseMatrixTimesVector(a,v) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case DenseMatrixMultiply(a,b) => Nil
    //case DenseMatrixTimesVector(a,v) => Nil
    case _ => super.copySyms(e)
  } 
  
}

/**
 *  Optimizations for composite DenseMatrixOps operations.
 */

trait DenseMatrixOpsExpOpt extends DenseMatrixOpsExp {
  this: DenseMatrixImplOps with OptiLAExp =>

  // override def densematrix_equals[A:Manifest](x: Exp[DenseMatrix[A]], y: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = (x, y) match {
  //   case (a,b) if (a == b) => unit(true) // same symbol
  //   case _ => super.densematrix_equals(x,y)
  // }

  override def densematrix_numrows[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = x match {
    case Def(s@Reflect(DenseMatrixObjectNew(rows,cols), u, es)) if context.contains(s) => rows // only if not modified! // TODO: check writes
    case Def(DenseMatrixObjectNew(rows,cols)) => rows
    case _ => super.densematrix_numrows(x)
  }
  
  override def densematrix_numcols[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = x match {
    case Def(s@Reflect(DenseMatrixObjectNew(rows,cols), u, es)) if context.contains(s) => cols // only if not modified! // TODO: check writes
    case Def(DenseMatrixObjectNew(rows,cols)) => cols
    case _ => super.densematrix_numcols(x)
  }

  override def densematrix_size[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = x match {
    case Def(e: DeliteOpMap[_,_,_]) => e.size
    case Def(e: DeliteOpZipWith[_,_,_,_]) => e.size
    case _ => super.densematrix_size(x)
  }
  
}


trait ScalaGenDenseMatrixOps extends ScalaGenBase {
  val IR: DenseMatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case m@DenseMatrixObjectNew(numRows, numCols) => emitValDef(sym, "new " + remap("generated.scala.DenseMatrix[" + remap(m.m) + "]")+"(" + quote(numRows) + "," + quote(numCols) + ")")    
    case DenseMatrixNumRows(x)  => emitValDef(sym, quote(x) + "._numRows")
    case DenseMatrixNumCols(x)  => emitValDef(sym, quote(x) + "._numCols")
    //case DenseMatrixUpdate(x,i,j,y)  => emitValDef(sym, quote(x) + "(" + quote(i) + ", " + quote(j) + ") = " + quote(y))
    // case DenseMatrixInsertRow(x,pos,y)  => emitValDef(sym, quote(x) + ".insertRow(" + quote(pos) + "," + quote(y) + ")")
    // case DenseMatrixInsertAllRows(x,pos,y) => emitValDef(sym, quote(x) + ".insertAllRows(" + quote(pos) + "," + quote(y) + ")")
    // case DenseMatrixInsertCol(x,pos,y) => emitValDef(sym, quote(x) + ".insertCol(" + quote(pos) + "," + quote(y) + ")")
    // case DenseMatrixInsertAllCols(x,pos,y) => emitValDef(sym, quote(x) + ".insertAllCols(" + quote(pos) + "," + quote(y) + ")")
    // case DenseMatrixRemoveRows(x,pos,len) => emitValDef(sym, quote(x) + ".removeRows(" + quote(pos) + "," + quote(len) + ")")
    // case DenseMatrixRemoveCols(x,pos,len) => emitValDef(sym, quote(x) + ".removeCols(" + quote(pos) + "," + quote(len) + ")")
    case DenseMatrixRawApply(x,i) => emitValDef(sym, quote(x) + "._data(" + quote(i) + ")")
    case DenseMatrixRawUpdate(x,i,y) => emitValDef(sym, quote(x) + "._data(" + quote(i) + ") = "  + quote(y))
    case DenseMatrixRawData(x) => emitValDef(sym, quote(getBlockResult(x)) + "._data")  // getBlockResult necessary?? should it be everywhere?
    case DenseMatrixSetNumRows(x,v) => emitValDef(sym, quote(x) + "._numRows = " + quote(v))
    case DenseMatrixSetNumCols(x,v) => emitValDef(sym, quote(x) + "._numCols = " + quote(v))
    case DenseMatrixSetRawData(x,data) => emitValDef(sym, quote(x) + "._data = " + quote(data))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenDenseMatrixOps extends CudaGenBase with CudaGenDataStruct {
  val IR: DenseMatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DenseMatrixObjectNew(numRows,numCols) => stream.println("%s *%s_ptr = new %s(%s,%s);".format(remap(sym.Type),quote(sym),remap(sym.Type),quote(numRows),quote(numCols)))
    case DenseMatrixNumRows(x)  => emitValDef(sym, quote(x) + ".numRows")
    case DenseMatrixNumCols(x)  => emitValDef(sym, quote(x) + ".numCols")
    case DenseMatrixRawData(x) => emitValDef(sym, quote(getBlockResult(x)) + ".getdata()")
    case DenseMatrixSetNumRows(x,v) => stream.println(quote(x) + ".numRows = " + quote(v) + ";")
    case DenseMatrixSetNumCols(x,v) => stream.println(quote(x) + ".numCols = " + quote(v) + ";")
    case DenseMatrixSetRawData(x,data) => stream.println(quote(x) + ".setdata(" + quote(data) + ");")
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenDenseMatrixOps extends OpenCLGenBase with OpenCLGenDataStruct {
  val IR: DenseMatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenDenseMatrixOps extends CGenBase {
  val IR: DenseMatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}
