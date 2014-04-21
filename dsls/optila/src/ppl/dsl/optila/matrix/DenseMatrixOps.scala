package ppl.dsl.optila.matrix

import java.io.{PrintWriter}

import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import scala.virtualization.lms.common.{CudaGenEffect, CudaGenBase, ScalaGenEffect, ScalaGenBase, OpenCLGenBase, CGenBase}
import scala.virtualization.lms.internal.{GenerationFailedException}
import scala.reflect.SourceContext

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.DeliteCollection
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.datastructures.{DeliteArray, DeliteStructsExp}
import ppl.delite.framework.Config
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.Util._

import ppl.dsl.optila._

trait DenseMatrixOps extends Variables {
  this: OptiLA =>

  implicit def repToDenseMatOps[A:Manifest](x: Rep[DenseMatrix[A]]) = new DenseMatOpsCls(x)
  implicit def varToDenseMatOps[A:Manifest](x: Var[DenseMatrix[A]]) = new DenseMatOpsCls(readVar(x))  
  implicit def repToDenseMatBuildableOps[A:Manifest](x: Rep[DenseMatrix[A]]) = new DenseMatBuildableOpsCls(x)
  implicit def varToDenseMatBuildableOps[A:Manifest](x: Var[DenseMatrix[A]]) = new DenseMatBuildableOpsCls(readVar(x))    
  implicit def denseMatToInterface[A:Manifest](lhs: Rep[DenseMatrix[A]]) = new MInterface[A](new DenseMatOpsCls[A](lhs))
  implicit def denseMatVarToInterface[A:Manifest](lhs: Var[DenseMatrix[A]]) = new MInterface[A](new DenseMatOpsCls[A](readVar(lhs)))
  implicit def denseMatToBuildableInterface[A:Manifest](lhs: Rep[DenseMatrix[A]]) = new MBuildableInterface[A](new DenseMatBuildableOpsCls[A](lhs))
  implicit def denseMatVarToBuildableInterface[A:Manifest](lhs: Var[DenseMatrix[A]]) = new MBuildableInterface[A](new DenseMatBuildableOpsCls[A](readVar(lhs)))
  
  implicit def denseMatrixBuilder[A:Manifest](implicit ctx: SourceContext) = new MatrixBuilder[A,DenseMatrix[A],DenseMatrix[A]] {
    def alloc(numRows: Rep[Int], numCols: Rep[Int]) = {
      Matrix.dense[A](numRows, numCols)
    }
    def toBuildableIntf(x: Rep[DenseMatrix[A]]): Interface[MatrixBuildable[A]] = denseMatToBuildableInterface(x)
    def finalizer(x: Rep[DenseMatrix[A]]) = x
    def toIntf(x: Rep[DenseMatrix[A]]): Interface[Matrix[A]] = denseMatToInterface(x)    
  }  

  object DenseMatrix {
    def apply[A:Manifest](numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_new(numRows, numCols)
    def apply[A:Manifest](xs: Rep[DenseVector[DenseVector[A]]])(implicit ctx: SourceContext): Rep[DenseMatrix[A]] = densematrix_obj_fromvec(xs)
    //def apply[A:Manifest](xs: Rep[DenseVector[DenseVectorView[A]]])(implicit o: Overloaded1): Rep[DenseMatrix[A]] = densematrix_obj_fromvec(xs.asInstanceOf[Rep[DenseVector[DenseVector[A]]]])  // AKS TODO
    def apply[A:Manifest](xs: Rep[DenseVector[A]]*)(implicit ctx: SourceContext): Rep[DenseMatrix[A]] = DenseMatrix(DenseVector(xs: _*))

    def diag[A:Manifest](w: Rep[Int], vals: Interface[Vector[A]])(implicit ctx: SourceContext) = densematrix_obj_diag(w, vals)
    def identity(w: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_identity(w)
    def zeros(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_zeros(numRows, numCols)
    def zerosf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_zerosf(numRows, numCols)
    def mzeros(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_mzeros(numRows, numCols)
    def mzerosf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_mzerosf(numRows, numCols)
    def ones(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_ones(numRows, numCols)
    def onesf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_onesf(numRows, numCols)
    def rand(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_rand(numRows, numCols)
    def randf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_randf(numRows, numCols)
    def randn(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_randn(numRows, numCols)
    def randnf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_randnf(numRows, numCols)
    def mrandnf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = densematrix_obj_mrandnf(numRows, numCols)
  }

  class DenseMatBuildableOpsCls[A:Manifest](val elem: Rep[DenseMatrix[A]]) extends MatBuildableOpsCls[A] {
    type Self = DenseMatrix[A]
    def wrap(x: Rep[DenseMatrix[A]]): Interface[MatrixBuildable[A]] = denseMatToBuildableInterface(x)
    type M[X] = DenseMatrix[X]
    type V[X] = DenseVector[X]
    def mA: Manifest[A] = manifest[A]
    def toIntf[B:Manifest](x: Rep[M[B]]) = denseMatToBuildableInterface(x)
    def vecToIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]] = denseVecToInterface[B](x)        
      
    // FIXME: see MatrixBuildableOps.scala
    protected def _numRows(implicit ctx: SourceContext) = densematrix_numrows(x)
    protected def _numCols(implicit ctx: SourceContext) = densematrix_numcols(x)
        
    def update(i: Rep[Int], j: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = densematrix_update(x,i,j,y)
    def insertRow(pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = densematrix_insertrow(x,pos,y)
    def insertAllRows(pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = densematrix_insertallrows(x,pos,y)
    def insertCol(pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = densematrix_insertcol(x,pos,y)
    def insertAllCols(pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = densematrix_insertallcols(x,pos,y)
    def removeRows(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = densematrix_removerows(x,pos,len)
    def removeCols(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = densematrix_removecols(x,pos,len)
  }
  
  class DenseMatOpsCls[A:Manifest](val elem: Rep[DenseMatrix[A]]) extends MatOpsCls[A] {
    type M[X] = DenseMatrix[X]
    type V[X] = DenseVector[X]
    type View[X] = DenseVectorView[X]
    type I[X] = DenseMatrix[X]
    type Self = DenseMatrix[A]
    type MA = DenseMatrix[A]
    type IA = DenseMatrix[A]
    def mA: Manifest[A] = manifest[A]
    def mM[B:Manifest]: Manifest[M[B]] = manifest[DenseMatrix[B]]    
    def mMA: Manifest[MA] = mM[A]    
    def mI[B:Manifest]: Manifest[I[B]] = mM[B]
    def mIA: Manifest[IA] = mI[A]
    def wrap(x: Rep[DenseMatrix[A]]): Interface[Matrix[A]] = denseMatToInterface(x)
    def maToOps(x: Rep[MA]): MatOpsCls[A] = matToOps[A](x)
    def maToIntf(x: Rep[MA]): Interface[Matrix[A]] = matToIntf[A](x)
    def maBuilder: MatrixBuilder[A,IA,MA] = matBuilder[A]
    def matToOps[B:Manifest](x: Rep[M[B]]): MatOpsCls[B] = repToDenseMatOps[B](x)
    def matToIntf[B:Manifest](x: Rep[M[B]]): Interface[Matrix[B]] = denseMatToInterface[B](x)        
    def matBuilder[B:Manifest](implicit ctx: SourceContext): MatrixBuilder[B,I[B],M[B]] = denseMatrixBuilder[B]      
    def mV[B:Manifest]: Manifest[V[B]] = manifest[DenseVector[B]]
    def vecToIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]] = denseVecToInterface[B](x)        
    def vecBuilder[B:Manifest](implicit ctx: SourceContext): VectorBuilder[B,V[B]] = denseVectorBuilder[B]
    def viewToIntf[B:Manifest](x: Rep[View[B]]) = denseViewToInterface(x)
    
    // delite collection
    def dcApply(n: Rep[Int])(implicit ctx: SourceContext): Rep[A] = densematrix_rawapply(x,n)
    def dcUpdate(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit] = densematrix_rawupdate(x,n,y)
    
    // accessors
    def apply(i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext) = densematrix_apply(x,i,j)
    def numRows(implicit ctx: SourceContext) = densematrix_numrows(x)
    def numCols(implicit ctx: SourceContext) = densematrix_numcols(x)
    def vview(start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext) = densematrix_vview(x,start,stride,length,isRow)
        
    // not supported by interface right now
    def inv(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext) = densematrix_inverse(x)    
    
    // overrides
    def *(y: Rep[DenseVector[A]])(implicit a: Arith[A], o: Overloaded1, ctx: SourceContext): Rep[DenseVector[A]] = densematrix_times_vector(x,y)
    def *(y: Rep[DenseMatrix[A]])(implicit a: Arith[A], ctx: SourceContext): Rep[MA] = densematrix_multiply(x,y)    
    override def sigmoid(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext): Rep[DenseMatrix[Double]] = densematrix_sigmoid(x)
    override def sigmoidf(implicit conv: Rep[A] => Rep[Float], ctx: SourceContext): Rep[DenseMatrix[Float]] = densematrix_sigmoidf(x)
  }
  
  // object defs
  //def symmatrix_obj_new[A:Manifest](n: Rep[Int]): Rep[SymmetricMatrix[A]]
  def densematrix_obj_new[A:Manifest](numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[A]]
  def densematrix_obj_fromseq[A:Manifest](xs: Seq[Interface[Vector[A]]])(implicit ctx: SourceContext): Rep[DenseMatrix[A]]
  def densematrix_obj_fromvec[A:Manifest](xs: Rep[DenseVector[DenseVector[A]]])(implicit ctx: SourceContext): Rep[DenseMatrix[A]]
  def densematrix_obj_diag[A:Manifest](w: Rep[Int], vals: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[DenseMatrix[A]]
  def densematrix_obj_identity(w: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[Double]]
  def densematrix_obj_zeros(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[Double]]
  def densematrix_obj_zerosf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[Float]]
  def densematrix_obj_mzeros(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[Double]]
  def densematrix_obj_mzerosf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[Float]]
  def densematrix_obj_ones(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[Double]]
  def densematrix_obj_onesf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[Float]]
  def densematrix_obj_rand(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[Double]]
  def densematrix_obj_randf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[Float]]
  def densematrix_obj_randn(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[Double]]
  def densematrix_obj_randnf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[Float]]
  def densematrix_obj_mrandnf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[Float]]
  
  def densematrix_fromarray[A:Manifest](x: Rep[DeliteArray[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[A]]
  
  // class defs
  def densematrix_apply[A:Manifest](x: Rep[DenseMatrix[A]], i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext): Rep[A]
  def densematrix_numrows[A:Manifest](x: Rep[DenseMatrix[A]])(implicit ctx: SourceContext): Rep[Int]
  def densematrix_numcols[A:Manifest](x: Rep[DenseMatrix[A]])(implicit ctx: SourceContext): Rep[Int]
  def densematrix_vview[A:Manifest](x: Rep[DenseMatrix[A]], start: Rep[Int], stride: Rep[Int], length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext): Rep[DenseVectorView[A]] 

  def densematrix_update[A:Manifest](x: Rep[DenseMatrix[A]], i: Rep[Int], j: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_insertrow[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_insertallrows[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_insertcol[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_insertallcols[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_removerows[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_removecols[A:Manifest](x: Rep[DenseMatrix[A]], pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  
  def densematrix_multiply[A:Manifest:Arith](x: Rep[DenseMatrix[A]], y: Rep[DenseMatrix[A]])(implicit ctx: SourceContext): Rep[DenseMatrix[A]]
  def densematrix_times_vector[A:Manifest:Arith](x: Rep[DenseMatrix[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[DenseVector[A]]
  def densematrix_inverse[A:Manifest](x: Rep[DenseMatrix[A]])(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext): Rep[DenseMatrix[Double]]  
  def densematrix_sigmoid[A:Manifest](x: Rep[DenseMatrix[A]])(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext): Rep[DenseMatrix[Double]]
  def densematrix_sigmoidf[A:Manifest](x: Rep[DenseMatrix[A]])(implicit conv: Rep[A] => Rep[Float], ctx: SourceContext): Rep[DenseMatrix[Float]]
    
  def densematrix_rawapply[A:Manifest](x: Rep[DenseMatrix[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[A]
  def densematrix_rawupdate[A:Manifest](x: Rep[DenseMatrix[A]], n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
}

trait DenseMatrixCompilerOps extends DenseMatrixOps {
  this: OptiLA =>
  
  def densematrix_raw_data[A:Manifest](x: Rep[DenseMatrix[A]])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
  def densematrix_set_raw_data[A:Manifest](x: Rep[DenseMatrix[A]], data: Rep[DeliteArray[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_set_numrows[A:Manifest](x: Rep[DenseMatrix[A]], newVal: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def densematrix_set_numcols[A:Manifest](x: Rep[DenseMatrix[A]], newVal: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
}

trait DenseMatrixOpsExp extends DenseMatrixCompilerOps with DeliteCollectionOpsExp with DeliteStructsExp with VariablesExp {
  this: DenseMatrixImplOps with OptiLAExp  =>

  //////////////////////////////////////////////////
  // implemented via method on real data structure
  case class DenseMatrixObjectNew[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) extends DeliteStruct[DenseMatrix[A]] {
    val elems = copyTransformedElems(collection.Seq("_data" -> var_new(DeliteArray[A](numRows*numCols)).e, "_numRows" -> var_new(numRows).e, "_numCols" -> var_new(numCols).e))
    val mA = manifest[A]
  }

  case class DenseMatrixObjectNewImm[A:Manifest](_data: Exp[DeliteArray[A]], _numRows: Exp[Int], _numCols: Exp[Int]) extends DeliteStruct[DenseMatrix[A]] {
    val elems = copyTransformedElems(collection.Seq("_data" -> _data, "_numRows" -> _numRows, "_numCols" -> _numCols))
    val mA = manifest[A]
  }
  
  case class DenseMatrixObjectZeros[A:Manifest](numRows: Exp[Int], numCols: Exp[Int]) extends DeliteStruct[DenseMatrix[A]] {
    val elems = copyTransformedElems(collection.Seq("_data" -> DeliteArray.imm(numRows*numCols), "_numRows" -> numRows, "_numCols" -> numCols))
    val mA = manifest[A]
  }

  //case class DenseMatrixRawData[A:Manifest](x: Exp[DenseMatrix[A]]) extends DefWithManifest[A,DeliteArray[A]]
  //case class DenseMatrixNumRows[A:Manifest](x: Exp[DenseMatrix[A]]) extends DefWithManifest[A,Int] 
  //case class DenseMatrixNumCols[A:Manifest](x: Exp[DenseMatrix[A]]) extends DefWithManifest[A,Int]
  //case class DenseMatrixSetNumRows[A:Manifest](x: Exp[DenseMatrix[A]], newVal: Exp[Int]) extends DefWithManifest[A,Unit]
  //case class DenseMatrixSetNumCols[A:Manifest](x: Exp[DenseMatrix[A]], newVal: Exp[Int]) extends DefWithManifest[A,Unit]
  //case class DenseMatrixSetRawData[A:Manifest](x: Exp[DenseMatrix[A]], data: Exp[DeliteArray[A]]) extends DefWithManifest[A,Unit]
    
  /////////////////////////////////////
  // implemented via kernel embedding

  case class DenseMatrixObjectFromSeq[A:Manifest](xs: Seq[Interface[Vector[A]]])
    extends DeliteOpSingleWithManifest[A,DenseMatrix[A]](reifyEffectsHere(densematrix_obj_fromseq_impl(xs))) 

  case class DenseMatrixObjectFromVec[A:Manifest](xs: Rep[DenseVector[DenseVector[A]]])
    extends DeliteOpSingleWithManifest[A,DenseMatrix[A]](reifyEffectsHere(densematrix_obj_fromvec_impl(xs)))

  case class DenseMatrixObjectDiag[A:Manifest](w: Exp[Int], vals: Interface[Vector[A]])
    extends DeliteOpSingleWithManifest[A,DenseMatrix[A]](reifyEffectsHere(densematrix_obj_diag_impl(w, vals))) 

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
    extends DeliteOpSingleWithManifest[A,DenseVectorView[A]](reifyEffectsHere(densematrix_vview_impl(x, start, stride, length, isRow)))

  case class DenseMatrixApply[A:Manifest](x: Exp[DenseMatrix[A]], i: Exp[Int], j: Exp[Int])
    extends DeliteOpSingleWithManifest[A,A](reifyEffectsHere(densematrix_apply_impl(x, i, j)))

  // case class DenseMatrixRawApply[A:Manifest](x: Exp[DenseMatrix[A]], i: Exp[Int])
  //   extends DefWithManifest[A,A]
  //   //extends DeliteOpSingleWithManifest[A,A](reifyEffectsHere(densematrix_rawapply_impl(x,i))) 
    
  case class DenseMatrixUpdate[A:Manifest](x: Exp[DenseMatrix[A]], i: Exp[Int], j: Exp[Int], y: Exp[A]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densematrix_update_impl(x,i,j,y)))
    
  // case class DenseMatrixRawUpdate[A:Manifest](x: Exp[DenseMatrix[A]], i: Exp[Int], y: Exp[A])
  //   extends DefWithManifest[A,Unit]
  //   //extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densematrix_rawupdate_impl(x,i,y)))

  case class DenseMatrixInsertRow[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Interface[Vector[A]]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densematrix_insertrow_impl(x,pos,y)))
    
  case class DenseMatrixInsertAllRows[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Interface[Matrix[A]]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densematrix_insertallrows_impl(x,pos,y)))
    
  case class DenseMatrixInsertCol[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Interface[Vector[A]]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densematrix_insertcol_impl(x,pos,y)))
    
  case class DenseMatrixInsertAllCols[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Interface[Matrix[A]]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densematrix_insertallcols_impl(x,pos,y)))
    
  case class DenseMatrixRemoveRows[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], len: Exp[Int])
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densematrix_removerows_impl(x,pos,len)))
    
  case class DenseMatrixRemoveCols[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], len: Exp[Int])
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densematrix_removecols_impl(x,pos,len)))

//  case class DenseMatrixUpdateRow[A:Manifest](x: Exp[DenseMatrix[A]], row: Exp[Int], y: Exp[Vector[A]])
//    extends DeliteOpSingleTask(reifyEffectsHere(densematrix_updaterow_impl(x,row,y)))

  // this is a single task right now because of the likely early exit. should we have a delite op for this?
  // case class DenseMatrixEquals[A:Manifest](x: Exp[DenseMatrix[A]], y: Exp[DenseMatrix[A]])
  //   extends DeliteOpSingleTask(reifyEffectsHere(densematrix_equals_impl[A](x,y)))
    
  case class DenseMatrixInverse[A:Manifest](x: Exp[DenseMatrix[A]])(implicit val conv: Exp[A] => Exp[Double])
    extends DeliteOpSingleWithManifest[A,DenseMatrix[Double]](reifyEffectsHere(densematrix_inverse_impl(x))) 
   
  case class DenseMatrixObjectConst[A:Manifest](numRows: Exp[Int], numCols: Exp[Int], c: Exp[A])
    extends DeliteOpMap[Int,A,DenseMatrix[A]] {

    val in = (unit(0)::numRows*numCols)
    val size = copyTransformedOrElse(_.size)(numRows*numCols)

    override def alloc = DenseMatrix[A](numRows, numCols)
    def func = e => c

    val mA = manifest[A]
  }

  ///////////////////////////////////////////////////////////////////
  // BLAS enabled routines 

  // TODO: generalize this so that we can generate fused, delite parallel op, or BLAS variants
  // having separate IR nodes breaks pattern matching optimizations... 

  // case class DenseMatrixMultiply[A:Manifest:Arith](x: Exp[DenseMatrix[A]], y: Exp[DenseMatrix[A]])
  //   extends DeliteOpSingleWithManifest[A,DenseMatrix[A]](reifyEffectsHere(densematrix_multiply_impl(x,y))) {
  //   
  //   val a = implicitly[Arith[A]]
  // }

  case class DenseMatrixMultiplyBLAS[A:Manifest:Arith](xR: Exp[Int], xC: Exp[Int], x: Exp[DeliteArray[A]], yR: Exp[Int], yC: Exp[Int], y: Exp[DeliteArray[A]]) extends DeliteOpExternal[DeliteArray[A]] {
    override def inputs = scala.List(xR,xC,x,yR,yC,y)
    def alloc = DeliteArray[A](xR * yC)
    val funcName = "matMult"

    val mA = manifest[A]
    val a = implicitly[Arith[A]]
  }

  case class DenseMatrixTimesVectorBLAS[A:Manifest:Arith](xR: Exp[Int], xC: Exp[Int], x: Exp[DeliteArray[A]], y: Exp[DeliteArray[A]]) extends DeliteOpExternal[DeliteArray[A]] {
    override def inputs = scala.List(x,y)
    def alloc = DeliteArray[A](xR)
    val funcName = "matMultV"

    val mA = manifest[A]
    val a = implicitly[Arith[A]]    
  }
  
  case class DenseMatrixSigmoidVectorized[A:Manifest](xR: Exp[Int], xC: Exp[Int], x: Exp[DeliteArray[A]]) extends DeliteOpExternal[DeliteArray[A]] {
    def alloc = DeliteArray[A](xR * xC)    
    val funcName = "matSigmoid"
    
    val mA = manifest[A]
  }

  case class DenseMatrixMultiply3by3[A:Manifest:Arith](x: Exp[DenseMatrix[A]], y: Exp[DenseMatrix[A]]) extends Def[DenseMatrix[A]] {
    type OpType <: DenseMatrixMultiply3by3[A]
    def original: Option[(Transformer,Def[_])] = None
    def copyTransformedBlockOrElse[B:Manifest](f: OpType => Block[B])(e: => Block[B]): Block[B] = original.map(p=>p._1(f(p._2.asInstanceOf[OpType]))).getOrElse(e)

    def alloc:Exp[DenseMatrix[A]] = DenseMatrix[A](unit(3),unit(3))
    final lazy val allocBlock: Block[DenseMatrix[A]] = copyTransformedBlockOrElse(_.allocBlock)(reifyEffects(alloc))
    final lazy val implBlock: Block[DenseMatrix[A]] = copyTransformedBlockOrElse(_.implBlock)(reifyEffects(matrix_multiply_impl[A,DenseMatrix[A],DenseMatrix[A]](x,y)))
    val mA = manifest[A]
    val a = implicitly[Arith[A]]
  }
  

  ////////////////////
  // object interface

  //def symdensematrix_obj_new[A:Manifest](n: Exp[Int]) = reflectMutable(SymmetricDenseMatrixObjectNew[A](n))
  def densematrix_obj_new[A:Manifest](numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(DenseMatrixObjectNew[A](numRows, numCols)) //XXX
  def densematrix_obj_fromarray[A:Manifest](data: Exp[DeliteArray[A]], numRows: Exp[Int], numCols: Exp[Int]) = reflectPure(DenseMatrixObjectNewImm[A](data, numRows, numCols))
  def densematrix_obj_fromseq[A:Manifest](xs: Seq[Interface[Vector[A]]])(implicit ctx: SourceContext) = reflectPure(DenseMatrixObjectFromSeq(xs)) //XXX
  def densematrix_obj_fromvec[A:Manifest](xs: Rep[DenseVector[DenseVector[A]]])(implicit ctx: SourceContext) = reflectPure(DenseMatrixObjectFromVec(xs))
  def densematrix_obj_diag[A:Manifest](w: Exp[Int], vals: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectPure(DenseMatrixObjectDiag(w, vals))
  def densematrix_obj_identity(w: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DenseMatrixObjectIdentity(w))
  def densematrix_obj_zeros(numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DenseMatrixObjectConst[Double](numRows, numCols, unit(0.0)))//DenseMatrixObjectZeros(numRows, numCols))
  def densematrix_obj_zerosf(numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DenseMatrixObjectConst[Float](numRows, numCols, unit(0.0f)))//DenseMatrixObjectZerosF(numRows, numCols))
  def densematrix_obj_mzeros(numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(DenseMatrixObjectConst[Double](numRows, numCols, unit(0.0)))//DenseMatrixObjectZeros(numRows, numCols))
  def densematrix_obj_mzerosf(numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectMutable(DenseMatrixObjectConst[Float](numRows, numCols, unit(0.0f)))//reflectPure(DenseMatrixObjectZerosF(numRows, numCols))
  def densematrix_obj_ones(numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DenseMatrixObjectConst(numRows, numCols, unit(1.0)))
  def densematrix_obj_onesf(numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DenseMatrixObjectConst(numRows, numCols, unit(1.0f)))
  def densematrix_obj_rand(numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DenseMatrixObjectRand(numRows, numCols))
  def densematrix_obj_randf(numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DenseMatrixObjectRandF(numRows, numCols))
  def densematrix_obj_randn(numRows: Exp[Int], numCols: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DenseMatrixObjectRandn(numRows, numCols))
  def densematrix_obj_randnf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = reflectPure(DenseMatrixObjectRandnF(numRows, numCols))
  def densematrix_obj_mrandnf(numRows: Rep[Int], numCols: Rep[Int])(implicit ctx: SourceContext) = reflectMutable(DenseMatrixObjectRandnF(numRows, numCols)) //TR was reflectPure (why?)


  ///////////////////
  // class interface

  def densematrix_apply[A:Manifest](x: Exp[DenseMatrix[A]], i: Exp[Int], j: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DenseMatrixApply[A](x,i,j))
  def densematrix_numrows[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = field[Int](x, "_numRows")
  def densematrix_numcols[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = field[Int](x, "_numCols")
  def densematrix_vview[A:Manifest](x: Exp[DenseMatrix[A]], start: Exp[Int], stride: Exp[Int], length: Exp[Int], isRow: Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(DenseMatrixVView(x,start,stride,length,isRow))

  def densematrix_update[A:Manifest](x: Exp[DenseMatrix[A]], i: Exp[Int], j: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixUpdate[A](x,i,j,y))
  def densematrix_insertrow[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixInsertRow(x,pos,y))
  def densematrix_insertallrows[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixInsertAllRows(x,pos,y))
  def densematrix_insertcol[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixInsertCol(x,pos,y))
  def densematrix_insertallcols[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], y: Interface[Matrix[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixInsertAllCols(x,pos,y))
  def densematrix_removerows[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixRemoveRows(x,pos,len))
  def densematrix_removecols[A:Manifest](x: Exp[DenseMatrix[A]], pos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(DenseMatrixRemoveCols(x,pos,len))
  def densematrix_multiply[A:Manifest:Arith](x: Exp[DenseMatrix[A]], y: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = {
    if (Config.useBlas && (manifest[A] == manifest[Double] || manifest[A] == manifest[Float])) reflectPure(DenseMatrixObjectNewImm(reflectPure(DenseMatrixMultiplyBLAS(x.numRows,x.numCols,densematrix_raw_data(x),y.numRows,y.numCols,densematrix_raw_data(y))),x.numRows,y.numCols))
    else {
      (x.numRows,y.numRows) match {
        case (Const(3),Const(3)) if Config.generateCUDA => reflectPure(DenseMatrixMultiply3by3[A](x,y))
        case _ => reflectPure(MatrixMultiply[A,DenseMatrix[A],DenseMatrix[A]](x,y))
      }
    }
  }
  def densematrix_times_vector[A:Manifest:Arith](x: Exp[DenseMatrix[A]], y: Exp[DenseVector[A]])(implicit ctx: SourceContext) = {
    if (Config.useBlas && (manifest[A] == manifest[Double] || manifest[A] == manifest[Float])) reflectPure(DenseVectorNewImm(reflectPure(DenseMatrixTimesVectorBLAS(x.numRows,x.numCols,densematrix_raw_data(x),densevector_raw_data(y))),x.numRows,unit(false)))
    else reflectPure(MatrixTimesVector[A,DenseVector[A]](x,y))
  }
  
  def densematrix_inverse[A:Manifest](x: Exp[DenseMatrix[A]])(implicit conv: Exp[A] => Exp[Double], ctx: SourceContext) = reflectPure(DenseMatrixInverse(x))
  
  def densematrix_sigmoid[A:Manifest](x: Exp[DenseMatrix[A]])(implicit conv: Exp[A] => Exp[Double], ctx: SourceContext) = {
    if (Config.useBlas && manifest[A] == manifest[Double]) {
      val r = reflectPure(DenseMatrixSigmoidVectorized(x.numRows,x.numCols,densematrix_raw_data(x.asInstanceOf[Exp[DenseMatrix[Double]]])))
      reflectPure(DenseMatrixObjectNewImm(r,x.numRows,x.numCols))    
    }
    else reflectPure(MatrixSigmoid[A,DenseMatrix[Double],DenseMatrix[Double]](x))
  }
  def densematrix_sigmoidf[A:Manifest](x: Exp[DenseMatrix[A]])(implicit conv: Exp[A] => Exp[Float], ctx: SourceContext) = {
    if (Config.useBlas && manifest[A] == manifest[Float]) {
      val r = reflectPure(DenseMatrixSigmoidVectorized(x.numRows,x.numCols,densematrix_raw_data(x.asInstanceOf[Exp[DenseMatrix[Float]]])))
      reflectPure(DenseMatrixObjectNewImm(r,x.numRows,x.numCols))     
    }
    else reflectPure(MatrixSigmoidF[A,DenseMatrix[Float],DenseMatrix[Float]](x))
  }  

  def densematrix_fromarray[A:Manifest](x: Rep[DeliteArray[A]], n: Rep[Int])(implicit ctx: SourceContext) = {
    // expecting x to be row-major...
    //val out = DenseMatrix[A](unit(0),unit(0))
    //densematrix_set_numrows(out,x.length/n)
    //densematrix_set_numcols(out,n)    
    //densematrix_set_raw_data(out,x)
    //out//.unsafeImmutable

    reflectPure(DenseMatrixObjectNewImm[A](x,x.length/n,n))
  }
  
  //////////////////
  // internal

  def densematrix_rawapply[A:Manifest](x: Exp[DenseMatrix[A]], n: Exp[Int])(implicit ctx: SourceContext) = densematrix_raw_data(x).apply(n)
  def densematrix_rawupdate[A:Manifest](x: Exp[DenseMatrix[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = densematrix_raw_data(x).update(n,y)
  def densematrix_raw_data[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = field[DeliteArray[A]](x, "_data")
  def densematrix_set_numrows[A:Manifest](x: Exp[DenseMatrix[A]], newVal: Exp[Int])(implicit ctx: SourceContext) = field_update[Int](x, "_numRows", newVal)
  def densematrix_set_numcols[A:Manifest](x: Exp[DenseMatrix[A]], newVal: Exp[Int])(implicit ctx: SourceContext) = field_update[Int](x, "_numCols", newVal)
  def densematrix_set_raw_data[A:Manifest](x: Exp[DenseMatrix[A]], data: Exp[DeliteArray[A]])(implicit ctx: SourceContext) = field_update[DeliteArray[A]](x, "_data", data)

  /////////////////////
  // delite collection
  
  def isDenseMat[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[DenseMatrix[A]])  
  def asDenseMat[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[DenseMatrix[A]]]
  
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isDenseMat(x)) asDenseMat(x).size
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

  override def dc_parallelization[A:Manifest](x: Exp[DeliteCollection[A]], hasConditions: Boolean)(implicit ctx: SourceContext) = {
    if (isDenseMat(x)) ParFlat // parallel filter not supported with matrices yet. how will this work with sparse matrices?
    else super.dc_parallelization(x, hasConditions)
  }    

  override def dc_data_field[A:Manifest](x: Exp[DeliteCollection[A]]) = {
    if (isDenseMat(x)) "_data"
    else super.dc_data_field(x)
  }

  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = {
    val m = manifest[T]
    if (m.erasure == classOf[DenseMatrix[_]]) Some((classTag(m), collection.immutable.List("_data" -> darrayManifest(m.typeArguments(0)), "_numRows" -> manifest[Int], "_numCols" -> manifest[Int])))
    else super.unapplyStructType
  }
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@DenseMatrixObjectNew(r,c) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectNew(f(r),f(c))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixObjectNewImm(d,r,c) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectNewImm(f(d),f(r),f(c))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixObjectZeros(r,c) => reflectPure(new {override val original = Some(f,e) } with DenseMatrixObjectZeros(f(r),f(c))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    //case e@DenseMatrixRawData(x) => reflectPure(DenseMatrixRawData(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    //case e@DenseMatrixNumRows(x) => reflectPure(DenseMatrixNumRows(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    //case e@DenseMatrixNumCols(x) => reflectPure(DenseMatrixNumCols(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    //case e@DenseMatrixRawApply(x,i) => reflectPure(DenseMatrixRawApply(f(x),f(i))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])

    // delite ops
    //case e@DenseMatrixObjectFromSeq(xs) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectFromSeq(f(xs))(e.mA))(mtype(manifest[A]),implicitly[SourceContext]) // AKS TODO: fix
    case e@DenseMatrixObjectFromVec(x) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectFromVec(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixObjectDiag(x,y) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectDiag(f(x),f(y))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixObjectIdentity(x) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectIdentity(f(x)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixObjectOnes(r,c) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectOnes(f(r),f(c)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixObjectOnesF(r,c) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectOnesF(f(r),f(c)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixObjectRand(r,c) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectRand(f(r),f(c)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixObjectRandF(r,c) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectRandF(f(r),f(c)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixObjectRandn(r,c) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectRandn(f(r),f(c)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixObjectRandnF(r,c) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectRandnF(f(r),f(c)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixVView(x,s,str,l,r) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixVView(f(x),f(s),f(str),f(l),f(r))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixApply(x,i,j) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixApply(f(x),f(i),f(j))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    //case e@DenseMatrixRawApply(x,i) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixRawApply(f(x),f(i))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixInverse(x) => reflectPure(new {override val original = Some(f,e) } with DenseMatrixInverse(f(x))(e.mA,e.conv))(mtype(manifest[A]),implicitly[SourceContext])      
    //case e@DenseMatrixMultiply(x,y) => reflectPure(new {override val original = Some(f,e) } with DenseMatrixMultiply(f(x),f(y))(e.mA,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixMultiplyBLAS(xR,xC,x,yR,yC,y) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixMultiplyBLAS(f(xR),f(xC),f(x),f(yR),f(yC),f(y))(e.mA,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixTimesVectorBLAS(xR,xC,x,y) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixTimesVectorBLAS(f(xR),f(xC),f(x),f(y))(e.mA,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixSigmoidVectorized(xR,xC,x) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixSigmoidVectorized(f(xR),f(xC),f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    //case e@DenseMatrixTimesVector(x,y) => reflectPure(new {override val original = Some(f,e) } with DenseMatrixTimesVector(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixObjectConst(numRows,numCols,c) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixObjectConst(f(numRows),f(numCols),f(c))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseMatrixMultiply3by3(x,y) => reflectPure(new { override val original = Some(f,e) } with DenseMatrixMultiply3by3(f(x),f(y))(e.mA,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    // reflected
    case Reflect(e@DenseMatrixObjectNew(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixObjectNew(f(x),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)    
    case Reflect(e@DenseMatrixObjectNewImm(d,r,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixObjectNewImm(f(d),f(r),f(c))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    //case Reflect(e@DenseMatrixRawData(x), u, es) => reflectMirrored(Reflect(DenseMatrixRawData(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    //case Reflect(e@DenseMatrixNumRows(x), u, es) => reflectMirrored(Reflect(DenseMatrixNumRows(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    //case Reflect(e@DenseMatrixNumCols(x), u, es) => reflectMirrored(Reflect(DenseMatrixNumCols(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))   
    //case Reflect(e@DenseMatrixSetNumRows(x,v), u, es) => reflectMirrored(Reflect(DenseMatrixSetNumRows(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    //case Reflect(e@DenseMatrixSetNumCols(x,v), u, es) => reflectMirrored(Reflect(DenseMatrixSetNumCols(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))  
    //case Reflect(e@DenseMatrixSetRawData(x,v), u, es) => reflectMirrored(Reflect(DenseMatrixSetRawData(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))       
    //case Reflect(e@DenseMatrixRawApply(x,n), u, es) => reflectMirrored(Reflect(DenseMatrixRawApply(f(x),f(n))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    //case Reflect(e@DenseMatrixRawUpdate(x,i,y), u, es) => reflectMirrored(Reflect(DenseMatrixRawUpdate(f(x),f(i),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@DenseMatrixObjectFromVec(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixObjectFromVec(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)      
    case Reflect(e@DenseMatrixObjectDiag(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixObjectDiag(f(x),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)        
    case Reflect(e@DenseMatrixObjectIdentity(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixObjectIdentity(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)       
    case Reflect(e@DenseMatrixObjectOnes(r,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixObjectOnes(f(r),f(c)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DenseMatrixObjectOnesF(r,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixObjectOnesF(f(r),f(c)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)              
    case Reflect(e@DenseMatrixObjectRand(r,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixObjectRand(f(r),f(c)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DenseMatrixObjectRandF(r,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixObjectRandF(f(r),f(c)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)              
    case Reflect(e@DenseMatrixObjectRandn(r,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixObjectRandn(f(r),f(c)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DenseMatrixObjectRandnF(r,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixObjectRandnF(f(r),f(c)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)     
    case Reflect(e@DenseMatrixVView(x,s,str,l,r), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixVView(f(x),f(s),f(str),f(l),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)  
    case Reflect(e@DenseMatrixApply(x,i,j), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixApply(f(x),f(i),f(j))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx) 
    //case Reflect(e@DenseMatrixRawApply(x,n), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixRawApply(f(x),f(n))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))          
    case Reflect(e@DenseMatrixInverse(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixInverse(f(x))(e.mA,e.conv), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)   
    //case Reflect(e@DenseMatrixMultiply(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixMultiply(f(x),f(y))(e.mA,e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))         
    case Reflect(e@DenseMatrixMultiplyBLAS(xR,xC,x,yR,yC,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixMultiplyBLAS(f(xR),f(xC),f(x),f(yR),f(yC),f(y))(e.mA,e.a), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)   
    case Reflect(e@DenseMatrixTimesVectorBLAS(xR,xC,x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixTimesVectorBLAS(f(xR),f(xC),f(x),f(y))(e.mA,e.a), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx) 
    case Reflect(e@DenseMatrixSigmoidVectorized(xR,xC,x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixSigmoidVectorized(f(xR),f(xC),f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)  
    case Reflect(e@DenseMatrixUpdate(x,i,j,r), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixUpdate(f(x),f(i),f(j),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    //case Reflect(e@DenseMatrixRawUpdate(x,i,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixRawUpdate(f(x),f(i),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseMatrixInsertRow(x,pos,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixInsertRow(f(x),f(pos),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)              
    case Reflect(e@DenseMatrixInsertAllRows(x,pos,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixInsertAllRows(f(x),f(pos),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DenseMatrixInsertCol(x,pos,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixInsertCol(f(x),f(pos),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)    
    case Reflect(e@DenseMatrixInsertAllCols(x,y,z), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixInsertAllCols(f(x),f(y),f(z))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)    
    case Reflect(e@DenseMatrixRemoveRows(x,y,z), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixRemoveRows(f(x),f(y),f(z))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DenseMatrixRemoveCols(x,y,z), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixRemoveCols(f(x),f(y),f(z))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)    
    case Reflect(e@DenseMatrixObjectConst(numRows,numCols,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseMatrixObjectConst(f(numRows),f(numCols),f(c))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DenseMatrixMultiply3by3(x,y), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with DenseMatrixMultiply3by3(f(x),f(y))(e.mA,e.a), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
  
  
  /////////////////////
  // aliases and sharing

  // TODO: precise sharing info for other IR types (default is conservative)
  override def blocks(e: Any): List[Block[Any]] = e match {
    case e@DenseMatrixMultiply3by3(x,y) => super.blocks(e) ::: blocks(e.allocBlock) ::: blocks(e.implBlock)
    case _ => super.blocks(e)
  }
  override def syms(e: Any): List[Sym[Any]] = e match {
    case e@DenseMatrixMultiply3by3(x,y) => super.syms(e) ::: syms(e.allocBlock) ::: syms(e.implBlock)
    case _ => super.syms(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case e@DenseMatrixMultiply3by3(x,y) => super.syms(e) ::: syms(e.allocBlock) ::: syms(e.implBlock)
    case _ => super.readSyms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e@DenseMatrixMultiply3by3(x,y) => effectSyms(e.allocBlock) ::: effectSyms(e.implBlock)//scala.List(x,y,e.alloc)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case e@DenseMatrixMultiply3by3(x,y) => super.symsFreq(e) ::: freqNormal(e.allocBlock) ::: freqNormal(e.implBlock)
    case _ => super.symsFreq(e)
  }

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case DenseMatrixMultiplyBLAS(a,b,c,d,e,f) => Nil
    case DenseMatrixTimesVectorBLAS(xR,xC,x,y) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case DenseMatrixMultiplyBLAS(a,b,c,d,e,f) => Nil
    case DenseMatrixTimesVectorBLAS(xR,xC,x,y) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case DenseMatrixMultiplyBLAS(a,b,c,d,e,f) => Nil
    case DenseMatrixTimesVectorBLAS(xR,xC,x,y) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case DenseMatrixMultiplyBLAS(a,b,c,d,e,f) => Nil
    case DenseMatrixTimesVectorBLAS(xR,xC,x,y) => Nil
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

  override def densematrix_apply[A:Manifest](x: Exp[DenseMatrix[A]], i: Exp[Int], j: Exp[Int])(implicit ctx: SourceContext) = (x,i) match {
    // matrix from a vector of vectors
    case (Def(DenseMatrixObjectFromVec(Def(DenseVectorObjectFromUnliftedSeq(xs)))), Const(a)) => (xs(a))(j) 
    case _ => super.densematrix_apply(x,i,j)
  }
  
  override def densematrix_numrows[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = x match {
    //case Def(IndexVector2Construct(lrows, lcols, f, fblk)) => lrows.length 
    //case Def(Reflect(IndexVector2Construct(lrows, lcols, f, fblk))) => lrows.length 
    case Def(s@Reflect(DenseMatrixObjectNew(rows,cols), u, es)) if context.contains(s) => rows // only if not modified! // TODO: check writes
    case Def(DenseMatrixObjectNew(rows,cols)) => rows
    case Def(DenseMatrixObjectFromVec(v)) => v.length    
    case _ => super.densematrix_numrows(x)
  }
  
  override def densematrix_numcols[A:Manifest](x: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = x match {
    case Def(s@Reflect(DenseMatrixObjectNew(rows,cols), u, es)) if context.contains(s) => cols // only if not modified! // TODO: check writes
    case Def(DenseMatrixObjectNew(rows,cols)) => cols
    case Def(DenseMatrixObjectFromVec(Def(DenseVectorObjectFromUnliftedSeq(xs)))) if (xs.length > 0) => xs(0) match {  
      case Def(DenseVectorObjectFromUnliftedSeq(xs2)) => Const(xs2.length)
      case _ => super.densematrix_numcols(x)
    }
    case _ => super.densematrix_numcols(x)
  } 


}


trait ScalaGenDenseMatrixOps extends ScalaGenEffect {
  val IR: DenseMatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@DenseMatrixMultiply3by3(x,y) => 
      emitBlock(e.implBlock)
      emitValDef(sym, quote(getBlockResult(e.implBlock)))
      //stream.println("val " + quote(sym) + ": " + remap(sym.tp) + " = throw new Exception(\"should never be executed\")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenDenseMatrixOps extends CudaGenEffect {
  val IR: DenseMatrixOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {

    case e@DenseMatrixMultiply3by3(x,y) => 
      emitBlock(e.allocBlock)
      emitValDef(sym, quote(getBlockResult(e.allocBlock)))
      stream.println("float localSum_00 = 0; float localSum_01 = 0; float localSum_02 = 0;")
      stream.println("float localSum_10 = 0; float localSum_11 = 0; float localSum_12 = 0;")
      stream.println("float localSum_20 = 0; float localSum_21 = 0; float localSum_22 = 0;")
      stream.println("for(int k=0; k<" + quote(x) + "._numCols; k++) {")
      stream.println("localSum_00 += " + quote(x) + "._data.apply(0*" + quote(x) + "._numCols" + "+k) * " + quote(y) + "._data.apply(k*3+0);")
      stream.println("localSum_01 += " + quote(x) + "._data.apply(0*" + quote(x) + "._numCols" + "+k) * " + quote(y) + "._data.apply(k*3+1);")
      stream.println("localSum_02 += " + quote(x) + "._data.apply(0*" + quote(x) + "._numCols" + "+k) * " + quote(y) + "._data.apply(k*3+2);")
      stream.println("localSum_10 += " + quote(x) + "._data.apply(1*" + quote(x) + "._numCols" + "+k) * " + quote(y) + "._data.apply(k*3+0);")
      stream.println("localSum_11 += " + quote(x) + "._data.apply(1*" + quote(x) + "._numCols" + "+k) * " + quote(y) + "._data.apply(k*3+1);")
      stream.println("localSum_12 += " + quote(x) + "._data.apply(1*" + quote(x) + "._numCols" + "+k) * " + quote(y) + "._data.apply(k*3+2);")
      stream.println("localSum_20 += " + quote(x) + "._data.apply(2*" + quote(x) + "._numCols" + "+k) * " + quote(y) + "._data.apply(k*3+0);")
      stream.println("localSum_21 += " + quote(x) + "._data.apply(2*" + quote(x) + "._numCols" + "+k) * " + quote(y) + "._data.apply(k*3+1);")
      stream.println("localSum_22 += " + quote(x) + "._data.apply(2*" + quote(x) + "._numCols" + "+k) * " + quote(y) + "._data.apply(k*3+2);")
      stream.println("}")
      stream.println(quote(sym) + "._data.update(0*3+0, localSum_00);")
      stream.println(quote(sym) + "._data.update(1*3+0, localSum_10);")
      stream.println(quote(sym) + "._data.update(2*3+0, localSum_20);")
      stream.println(quote(sym) + "._data.update(0*3+1, localSum_01);")
      stream.println(quote(sym) + "._data.update(1*3+1, localSum_11);")
      stream.println(quote(sym) + "._data.update(2*3+1, localSum_21);")
      stream.println(quote(sym) + "._data.update(0*3+2, localSum_02);")
      stream.println(quote(sym) + "._data.update(1*3+2, localSum_12);")
      stream.println(quote(sym) + "._data.update(2*3+2, localSum_22);")

    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenDenseMatrixOps extends OpenCLGenBase {
  val IR: DenseMatrixOpsExp
}

trait CGenDenseMatrixOps extends CGenBase {
  val IR: DenseMatrixOpsExp
}
 
