package ppl.dsl.optila.generic

import reflect.{Manifest, SourceContext}
import ppl.dsl.optila._

/* 
 * for reasons I have been unable to decipher, having this file
 * uncommented breaks other OptiLA implicit conversions - 
 * even if the enclosing trait is not used by anybody.
 * 
 * it also causes this fun error sometimes: "erroneous or inaccessible type"   
 * 
 * the problematic method signatures appear to be the ones that take
 * arguments,
 * 
 * e.g. def vecToOps[B:Manifest](x: Rep[DenseVector[B]]) = repToDenseVecOps(x)    
 * 
 * but this is in no way has been narrowed down for certain.
 */
 
trait GenericDefs {
  this: OptiLA =>
  /*
  trait GenericCommon[A] {
    type Self
      
    val elem: Rep[Self] 
    val x = elem
  
    // generic return types, unless overlaoded for the op below
    // VA might be different than V[A], e.g. in IndexVectorDenseOps
    type M[X] <: Matrix[X]
    type I[X] <: MatrixBuildable[X]
    type V[X] <: Vector[X]  
    type MA = M[A]  
    type IA = I[A]       
    type VA <: Vector[A]       
  
    implicit def mA: Manifest[A]     
      
    implicit def mV[B:Manifest]: Manifest[V[B]]         
    implicit def vecToOps[B:Manifest](x: Rep[V[B]]): VecOpsCls[B]
    implicit def vecToIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]]        
    implicit def vecBuilder[B:Manifest](implicit ctx: SourceContext): VectorBuilder[B,V[B]]    
    
    implicit def mVA: Manifest[VA]    
    implicit def vaToOps(x: Rep[VA]): VecOpsCls[A]
    implicit def vaToIntf(x: Rep[VA]): Interface[Vector[A]]
    implicit def vaBuilder(implicit ctx: SourceContext): VectorBuilder[A,VA]
      
    implicit def mM[B:Manifest]: Manifest[M[B]]         
    implicit def mI[B:Manifest]: Manifest[I[B]]   
    implicit def matToIntf[B:Manifest](x: Rep[M[B]]): Interface[Matrix[B]]        
    implicit def matToOps[B:Manifest](x: Rep[M[B]]): MatOpsCls[B]
    implicit def matBuilder[B:Manifest](implicit ctx: SourceContext): MatrixBuilder[B,I[B],M[B]]                      
  }
 
 
  trait GenericVectorDense[A] extends VecOpsCls[A] {    
    type M[X] = DenseMatrix[X]   
    type I[X] = DenseMatrix[X]         
    type V[X] = DenseVector[X]     
       
    def vecToOps[B:Manifest](x: Rep[DenseVector[B]]) = repToDenseVecOps(x)    
    def vecToIntf[B:Manifest](x: Rep[DenseVector[B]]) = denseVecToInterface(x)
    def matToIntf[B:Manifest](x: Rep[DenseMatrix[B]]) = denseMatToInterface(x)
    def vecBuilder[B:Manifest](implicit ctx: SourceContext) = denseVectorBuilder[B]    
    def matBuilder[B:Manifest](implicit ctx: SourceContext) = denseMatrixBuilder[B]    
    def mV[B:Manifest]: Manifest[V[B]] = manifest[DenseVector[B]] 
    def mM[B:Manifest]: Manifest[M[B]] = manifest[DenseMatrix[B]]    
    def mI[B:Manifest]: Manifest[I[B]] = mM[B]        
  }
  
  trait GenericVectorSparse[A] extends VecOpsCls[A] {
    type M[X] = SparseMatrix[X]       
    type I[X] = SparseMatrixBuildable[X] 
    type V[X] = SparseVector[X]          
    
    def vecToOps[B:Manifest](x: Rep[SparseVector[B]]) = repToSparseVecOps(x)
    def vecToIntf[B:Manifest](x: Rep[SparseVector[B]]) = sparseVecToInterface(x)
    def matToIntf[B:Manifest](x: Rep[SparseMatrix[B]]) = sparseMatToInterface(x)
    def vecBuilder[B:Manifest](implicit ctx: SourceContext) = sparseVectorBuilder[B]    
    def matBuilder[B:Manifest](implicit ctx: SourceContext) = sparseMatrixBuilder[B] 
    def mV[B:Manifest]: Manifest[V[B]] = manifest[SparseVector[B]] 
    def mM[B:Manifest]: Manifest[M[B]] = manifest[SparseMatrix[B]] 
    def mI[B:Manifest]: Manifest[I[B]] = manifest[SparseMatrixBuildable[B]]
  }

  trait GenericMatrixDense[A] extends MatOpsCls[A] {
    type M[X] = DenseMatrix[X]
    type I[X] = DenseMatrix[X]  
    type V[X] = DenseVector[X]
    type View[X] = DenseVectorView[X]
    
    def vaToOps(x: Rep[VA]) = repToDenseVecOps(x) //vecToOps[A](x)
    def vaToIntf(x: Rep[VA]) = vecToIntf[A](x)
    def vaBuilder(implicit ctx: SourceContext) = vecBuilder[A]      
    def mVA = mV[A]
  
    def mM[B:Manifest]: Manifest[M[B]] = manifest[DenseMatrix[B]]    
    def mI[B:Manifest]: Manifest[I[B]] = mM[B]
    def matToOps[B:Manifest](x: Rep[M[B]]) = repToDenseMatOps[B](x)
    def matToIntf[B:Manifest](x: Rep[M[B]]) = denseMatToInterface[B](x)        
    def matBuilder[B:Manifest](implicit ctx: SourceContext) = denseMatrixBuilder[B]            
    def mV[B:Manifest]: Manifest[V[B]] = manifest[DenseVector[B]]
    def vecToOps[B:Manifest](x: Rep[DenseVector[B]]) = repToDenseVecOps(x)
    def vecToIntf[B:Manifest](x: Rep[V[B]]) = denseVecToInterface[B](x)        
    def vecBuilder[B:Manifest](implicit ctx: SourceContext) = denseVectorBuilder[B]
    def viewToIntf[B:Manifest](x: Rep[View[B]]) = denseViewToInterface(x)  
  }
  
  trait GenericMatrixSparse[A] extends MatOpsCls[A] {
    type M[X] = SparseMatrix[X]
    type I[X] = SparseMatrixBuildable[X]  
    type V[X] = SparseVector[X]
    type View[X] = SparseVectorView[X]

    def vaToOps(x: Rep[VA]) = vecToOps[A](x)
    def vaToIntf(x: Rep[VA]) = vecToIntf[A](x)
    def vaBuilder(implicit ctx: SourceContext) = vecBuilder[A]      
    def mVA = mV[A]
  
    def mM[B:Manifest]: Manifest[M[B]] = manifest[SparseMatrix[B]]    
    def mI[B:Manifest]: Manifest[I[B]] = manifest[SparseMatrixBuildable[B]]
    def matToOps[B:Manifest](x: Rep[M[B]]) = repToSparseMatOps[B](x)
    def matToIntf[B:Manifest](x: Rep[M[B]]) = sparseMatToInterface[B](x)        
    def matBuilder[B:Manifest](implicit ctx: SourceContext) = sparseMatrixBuilder[B]            
    def mV[B:Manifest]: Manifest[V[B]] = manifest[SparseVector[B]]
    def vecToOps[B:Manifest](x: Rep[SparseVector[B]]) = repToSparseVecOps(x)
    def vecToIntf[B:Manifest](x: Rep[V[B]]) = sparseVecToInterface[B](x)        
    def vecBuilder[B:Manifest](implicit ctx: SourceContext) = sparseVectorBuilder[B]
    def viewToIntf[B:Manifest](x: Rep[View[B]]) = sparseViewToInterface(x)
  }
  */
}