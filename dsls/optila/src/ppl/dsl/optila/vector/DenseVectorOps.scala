package ppl.dsl.optila.vector

import java.io.{PrintWriter}
import scala.reflect.Manifest
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.datastructures.DeliteArray
import ppl.delite.framework.Util._
import ppl.dsl.optila._

trait DenseVectorOps extends Variables {
  this: OptiLA =>

  implicit def repToDenseVecOps[A:Manifest](x: Rep[DenseVector[A]]) = new DenseVecOpsCls(x)
  implicit def varToDenseVecOps[A:Manifest](x: Var[DenseVector[A]]) = new DenseVecOpsCls(readVar(x))
  implicit def denseVecToInterface[A:Manifest](lhs: Rep[DenseVector[A]]) = new VInterface[A](new DenseVecOpsCls[A](lhs))
  implicit def denseVecVarToInterface[A:Manifest](lhs: Var[DenseVector[A]]) = new VInterface[A](new DenseVecOpsCls[A](readVar(lhs)))

  implicit def denseVectorBuilder[A:Manifest](implicit ctx: SourceContext) = new VectorBuilder[A,DenseVector[A]] {
    def alloc(length: Rep[Int], isRow: Rep[Boolean]) = {
      Vector.dense[A](length, isRow)
    }
    def toIntf(x: Rep[DenseVector[A]]): Interface[Vector[A]] = denseVecToInterface(x)
  }  

  object DenseVector {
    def apply[A:Manifest](len: Int, isRow: Boolean)(implicit ctx: SourceContext) = densevector_obj_new(unit(len), unit(isRow)) // needed to resolve ambiguities
    def apply[A](len: Rep[Int], isRow: Rep[Boolean])(implicit mA: Manifest[A], o: Overloaded1, ctx: SourceContext) = densevector_obj_new(len, isRow)
    def apply[A](xs: Rep[A]*)(implicit mA: Manifest[A], o: Overloaded2, ctx: SourceContext) = densevector_obj_fromunliftedseq(xs)
    def fromSeq[A:Manifest](xs: Rep[Seq[A]])(implicit o: Overloaded3, ctx: SourceContext) = densevector_obj_fromseq(xs)
    
    def ones(len: Rep[Int])(implicit ctx: SourceContext) = densevector_obj_ones(len)
    def onesf(len: Rep[Int])(implicit ctx: SourceContext) = densevector_obj_onesf(len)
    def zeros(len: Rep[Int])(implicit ctx: SourceContext) = densevector_obj_zeros(len)
    def zerosf(len: Rep[Int])(implicit ctx: SourceContext) = densevector_obj_zerosf(len)
    def rand(len: Rep[Int])(implicit ctx: SourceContext) = densevector_obj_rand(len)
    def randf(len: Rep[Int])(implicit ctx: SourceContext) = densevector_obj_randf(len)
    def uniform(start: Rep[Double], step_size: Rep[Double], end: Rep[Double], isRow: Rep[Boolean] = unit(true))(implicit ctx: SourceContext) =
      densevector_obj_uniform(start, step_size, end, isRow)    
    def flatten[A:Manifest](pieces: Rep[DenseVector[DenseVector[A]]])(implicit ctx: SourceContext) = densevector_obj_flatten(pieces)
  }

  class DenseVecOpsCls[A:Manifest](val elem: Rep[DenseVector[A]]) extends VecOpsCls[A] {
    type Self = DenseVector[A]
    type VA = Self
    
    def wrap(x: Rep[DenseVector[A]]) = denseVecToInterface(x)
    def vaToOps(x: Rep[VA]) = vecToOps[A](x) //repToDenseVecOps[A](x)
    def vaToIntf(x: Rep[VA]) = vecToIntf[A](x) //denseVecToInterface[A](x)
    def vaBuilder(implicit ctx: SourceContext) = vecBuilder[A] //denseVectorBuilder[A]      
    def mVA = mV[A]
    
    // -- without generic defs
    
    /* attempts at refactoring these definitions into an encapsulated trait
     * have not worked out very well -- see GenericDefs.scala */    
    type V[X] = DenseVector[X]        
    type M[X] = DenseMatrix[X]   
    type I[X] = DenseMatrix[X]         
    def vecToOps[B:Manifest](x: Rep[DenseVector[B]]) = repToDenseVecOps(x)
    def vecToIntf[B:Manifest](x: Rep[DenseVector[B]]): Interface[Vector[B]] = denseVecToInterface(x)
    def matToIntf[B:Manifest](x: Rep[DenseMatrix[B]]): Interface[Matrix[B]] = denseMatToInterface(x)
    def vecBuilder[B:Manifest](implicit ctx: SourceContext): VectorBuilder[B,V[B]] = denseVectorBuilder[B]    
    def matBuilder[B:Manifest](implicit ctx: SourceContext): MatrixBuilder[B,I[B],M[B]] = denseMatrixBuilder[B]    
    def mV[B:Manifest] = manifest[DenseVector[B]] 
    def mM[B:Manifest] = manifest[DenseMatrix[B]] 
    // -- end without generic defs
    def mA: Manifest[A] = manifest[A]        
    
    // accessors
    def length(implicit ctx: SourceContext) = densevector_length(elem)
    def isRow(implicit ctx: SourceContext) = densevector_isrow(elem)
    def apply(n: Rep[Int])(implicit ctx: SourceContext) = densevector_apply(elem, n)
    
    // general
    def t(implicit ctx: SourceContext) = densevector_trans(elem)
    def mt()(implicit ctx: SourceContext) = {densevector_mutable_trans(elem); elem}
    
    // data operations
    def update(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = densevector_update(elem,n,y)
    def copyFrom(pos: Rep[Int], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = densevector_copyfrom(elem,pos,y)
    def insert(pos: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = densevector_insert(elem,pos,y)
    def insertAll(pos: Rep[Int], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = densevector_insertall(elem,pos,y)
    def removeAll(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = densevector_removeall(elem,pos,len)
    def trim()(implicit ctx: SourceContext) = densevector_trim(elem)
    def clear()(implicit ctx: SourceContext) = densevector_clear(elem)
    
    /**
     * there is some redundancy in the specializations for DenseVector/DenseVectorView and SparseVector/SparseVectorView
     * due to the fact that they have no inheritance relation. can we factor this out somehow?
     */
    // specializations
    def *(y: Rep[SparseVector[A]])(implicit a: Arith[A], o: Overloaded1, ctx: SourceContext) 
      = vector_times[A,SparseVector[A]](denseVecToInterface(elem),sparseVecToInterface(y))(manifest[A],implicitly[Arith[A]],manifest[SparseVector[A]],sparseVectorBuilder[A],ctx)            
    def *(y: Rep[SparseVectorView[A]])(implicit a: Arith[A], o: Overloaded2, ctx: SourceContext) 
      = vector_times[A,SparseVector[A]](denseVecToInterface(elem),sparseViewToInterface(y))(manifest[A],implicitly[Arith[A]],manifest[SparseVector[A]],sparseVectorBuilder[A],ctx)                
    def *(y: Rep[DenseMatrix[A]])(implicit a: Arith[A], o: Overloaded3, ctx: SourceContext) = densevector_times_matrix(elem,y)
    
    // ordering operations
    def sort(implicit o: Ordering[A], ctx: SourceContext) = densevector_sort(elem)    
  }

  // object defs
  def densevector_obj_new[A:Manifest](len: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext): Rep[DenseVector[A]]
  def densevector_obj_fromseq[A:Manifest](xs: Rep[Seq[A]])(implicit ctx: SourceContext): Rep[DenseVector[A]]
  def densevector_obj_fromunliftedseq[A:Manifest](xs: Seq[Rep[A]])(implicit ctx: SourceContext): Rep[DenseVector[A]]
  def densevector_obj_ones(len: Rep[Int])(implicit ctx: SourceContext): Rep[DenseVector[Double]]
  def densevector_obj_onesf(len: Rep[Int])(implicit ctx: SourceContext): Rep[DenseVector[Float]]
  def densevector_obj_zeros(len: Rep[Int])(implicit ctx: SourceContext): Rep[DenseVector[Double]]
  def densevector_obj_zerosf(len: Rep[Int])(implicit ctx: SourceContext): Rep[DenseVector[Float]]
  def densevector_obj_rand(len: Rep[Int])(implicit ctx: SourceContext): Rep[DenseVector[Double]]
  def densevector_obj_randf(len: Rep[Int])(implicit ctx: SourceContext): Rep[DenseVector[Float]]
  def densevector_obj_uniform(start: Rep[Double], step_size: Rep[Double], end: Rep[Double], isRow: Rep[Boolean])(implicit ctx: SourceContext): Rep[DenseVector[Double]]
  def densevector_obj_flatten[A:Manifest](pieces: Rep[DenseVector[DenseVector[A]]])(implicit ctx: SourceContext): Rep[DenseVector[A]]  
  
  // class defs
  def densevector_length[A:Manifest](x: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[Int]
  def densevector_isrow[A:Manifest](x: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[Boolean]
  def densevector_apply[A:Manifest](x: Rep[DenseVector[A]], n: Rep[Int])(implicit ctx: SourceContext): Rep[A]  
  def densevector_trans[A:Manifest](x: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[DenseVector[A]]
  def densevector_mutable_trans[A:Manifest](x: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densevector_update[A:Manifest](x: Rep[DenseVector[A]], n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def densevector_copyfrom[A:Manifest](x: Rep[DenseVector[A]], pos: Rep[Int], y: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densevector_insert[A:Manifest](x: Rep[DenseVector[A]], pos: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def densevector_insertall[A:Manifest](x: Rep[DenseVector[A]], pos: Rep[Int], y: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densevector_removeall[A:Manifest](x: Rep[DenseVector[A]], pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def densevector_trim[A:Manifest](x: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densevector_clear[A:Manifest](x: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densevector_times_matrix[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseMatrix[A]])(implicit ctx: SourceContext): Rep[DenseVector[A]]
  def densevector_sort[A:Manifest:Ordering](x: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[DenseVector[A]]
  
  // other defs
  def densevector_empty_double(implicit ctx: SourceContext): Rep[DenseVector[Double]]
  def densevector_empty_float(implicit ctx: SourceContext): Rep[DenseVector[Float]]
  def densevector_empty_int(implicit ctx: SourceContext): Rep[DenseVector[Int]]
  def densevector_empty[A:Manifest](implicit ctx: SourceContext): Rep[DenseVector[A]]
  def densevector_zero_double(length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext): Rep[DenseVector[Double]]
  def densevector_zero_float(length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext): Rep[DenseVector[Float]]
  def densevector_zero_int(length: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext): Rep[DenseVector[Int]]  
}

trait DenseVectorCompilerOps extends DenseVectorOps {
  this: OptiLACompiler =>
  
  def densevector_raw_data[A:Manifest](x: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[DeliteArray[A]]
  def densevector_set_raw_data[A:Manifest](x: Rep[DenseVector[A]], data: Rep[DeliteArray[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densevector_set_length[A:Manifest](x: Rep[DenseVector[A]], newVal: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def densevector_set_isrow[A:Manifest](x: Rep[DenseVector[A]], newVal: Rep[Boolean])(implicit ctx: SourceContext): Rep[Unit]
}

trait DenseVectorOpsExp extends DenseVectorOps with DeliteCollectionOpsExp {

  this: DenseVectorImplOps with OptiLAExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class DenseVectorNew[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) extends DefWithManifest[A,DenseVector[A]] 
  case class DenseVectorEmptyDouble() extends Def[DenseVector[Double]]
  case class DenseVectorEmptyFloat() extends Def[DenseVector[Float]]
  case class DenseVectorEmptyInt() extends Def[DenseVector[Int]]
  case class DenseVectorEmpty[A:Manifest]() extends DefWithManifest[A,DenseVector[A]] 
  case class DenseVectorZeroDouble(length: Exp[Int], isRow: Exp[Boolean]) extends Def[DenseVector[Double]]
  case class DenseVectorZeroFloat(length: Exp[Int], isRow: Exp[Boolean]) extends Def[DenseVector[Float]]
  case class DenseVectorZeroInt(length: Exp[Int], isRow: Exp[Boolean]) extends Def[DenseVector[Int]]
  
  case class DenseVectorLength[A:Manifest](x: Exp[DenseVector[A]]) extends DefWithManifest[A,Int]
  case class DenseVectorIsRow[A:Manifest](x: Exp[DenseVector[A]]) extends DefWithManifest[A,Boolean]
  case class DenseVectorRawData[A:Manifest](x: Exp[DenseVector[A]]) extends DefWithManifest[A,DeliteArray[A]]
  case class DenseVectorSetLength[A:Manifest](x: Exp[DenseVector[A]], newVal: Exp[Int]) extends DefWithManifest[A,Unit]
  case class DenseVectorSetIsRow[A:Manifest](x: Exp[DenseVector[A]], newVal: Exp[Boolean]) extends DefWithManifest[A,Unit]
  case class DenseVectorSetRawData[A:Manifest](x: Exp[DenseVector[A]], newVal: Exp[DeliteArray[A]]) extends DefWithManifest[A,Unit]
    
  /////////////////////////////////////////////////
  // implemented via kernel embedding (sequential)
  
  case class DenseVectorObjectFromSeq[A:Manifest](xs: Exp[Seq[A]])
    extends DeliteOpSingleWithManifest[A,DenseVector[A]](reifyEffectsHere(densevector_obj_fromseq_impl(xs)))

  case class DenseVectorObjectFromUnliftedSeq[A:Manifest](xs: Seq[Exp[A]])
    extends DeliteOpSingleWithManifest[A,DenseVector[A]](reifyEffectsHere(densevector_obj_fromunliftedseq_impl(xs)))  

  case class DenseVectorObjectOnes(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_ones_impl(len)))

  case class DenseVectorObjectOnesF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_onesf_impl(len)))

  case class DenseVectorObjectZeros(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_zeros_impl(len))) //reflectPure(DenseVectorNew[Double](len, Const(true))))) 

  case class DenseVectorObjectZerosF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_zerosf_impl(len))) //reflectPure(DenseVectorNew[Float](len, Const(true))))) 
    
  case class DenseVectorObjectRand(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_rand_impl(len)))

  case class DenseVectorObjectRandF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_randf_impl(len)))

  case class DenseVectorObjectUniform(start: Exp[Double], step_size: Exp[Double], end: Exp[Double], isRow: Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_uniform_impl(start, step_size, end, isRow)))

  case class DenseVectorObjectFlatten[A:Manifest](pieces: Exp[DenseVector[DenseVector[A]]])
    extends DeliteOpSingleWithManifest[A,DenseVector[A]](reifyEffectsHere(densevector_obj_flatten_impl(pieces)))
        
  case class DenseVectorApply[A:Manifest](x: Exp[DenseVector[A]], n: Exp[Int]) 
      extends DefWithManifest[A,A]
  //  extends DeliteOpSingleWithManifest[A,A](reifyEffectsHere(densevector_apply_impl(x,n)))
    
  case class DenseVectorUpdate[A:Manifest](x: Exp[DenseVector[A]], n: Exp[Int], y: Exp[A]) 
      extends DefWithManifest[A,Unit]
  //  extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densevector_update_impl(x,n,y)))
    
  case class DenseVectorCopyFrom[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[DenseVector[A]])
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densevector_copyfrom_impl(x,pos,y)))
    
  case class DenseVectorInsert[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[A]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densevector_insert_impl(x,pos,y)))
    
  case class DenseVectorInsertAll[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[DenseVector[A]])
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densevector_insertall_impl(x,pos,y)))
    
  case class DenseVectorRemoveAll[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], len: Exp[Int])
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densevector_removeall_impl(x,pos,len)))
    
  case class DenseVectorTrim[A:Manifest](x: Exp[DenseVector[A]]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densevector_trim_impl(x)))
    
  case class DenseVectorClear[A:Manifest](x: Exp[DenseVector[A]]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densevector_clear_impl(x)))
    
  case class DenseVectorMutableTrans[A:Manifest](x: Exp[DenseVector[A]]) 
    extends DeliteOpSingleWithManifest[A,Unit](reifyEffectsHere(densevector_mutabletrans_impl(x)))
    
  // TODO: right now we just use the underlying data structure sort, but we should implement our own
  // fast parallel sort with delite ops
  case class DenseVectorSort[A:Manifest:Ordering](x: Exp[DenseVector[A]]) 
    extends DeliteOpSingleWithManifest[A,DenseVector[A]](reifyEffectsHere(densevector_sort_impl(x))) {
   
    val o = implicitly[Ordering[A]]   
  }

  case class DenseVectorTimesMatrix[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseMatrix[A]])
    extends DeliteOpSingleWithManifest[A,DenseVector[A]](reifyEffectsHere(densevector_times_matrix_impl[A](x,y))) {
    
    val a = implicitly[Arith[A]]
  }
  
  
  ////////////////////////////////
  // implemented via delite ops

  case class DenseVectorTrans[A:Manifest](in: Exp[DenseVector[A]])
    extends DeliteOpMap[A,A,DenseVector[A]] {
    val size = copyTransformedOrElse(_.size)(in.length)

    override def alloc = DenseVector[A](in.length, !in.isRow)
    def func = e => e 

    val mA = manifest[A]
  }

  /////////////////////
  // object interface

  def densevector_obj_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean])(implicit ctx: SourceContext) = reflectMutable(DenseVectorNew[A](len, isRow)) //XXX
  def densevector_obj_fromseq[A:Manifest](xs: Exp[Seq[A]])(implicit ctx: SourceContext) = reflectPure(DenseVectorObjectFromSeq(xs)) //XXX
  def densevector_obj_fromunliftedseq[A:Manifest](xs: Seq[Exp[A]])(implicit ctx: SourceContext) = reflectPure(DenseVectorObjectFromUnliftedSeq(xs)) 
  def densevector_obj_ones(len: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DenseVectorObjectOnes(len))
  def densevector_obj_onesf(len: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DenseVectorObjectOnesF(len))
  def densevector_obj_zeros(len: Exp[Int])(implicit ctx: SourceContext) = densevector_zero_double(len,unit(true)) //reflectPure(DenseVectorObjectZeros(len))
  def densevector_obj_zerosf(len: Exp[Int])(implicit ctx: SourceContext) = densevector_zero_float(len,unit(true))//reflectPure(DenseVectorObjectZerosF(len))
  def densevector_obj_rand(len: Exp[Int])(implicit ctx: SourceContext) = reflectEffect(DenseVectorObjectRand(len))
  def densevector_obj_randf(len: Exp[Int])(implicit ctx: SourceContext) = reflectEffect(DenseVectorObjectRandF(len))
  def densevector_obj_uniform(start: Exp[Double], step_size: Exp[Double], end: Exp[Double], isRow: Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(DenseVectorObjectUniform(start, step_size, end, isRow))
  def densevector_obj_flatten[A:Manifest](pieces: Exp[DenseVector[DenseVector[A]]])(implicit ctx: SourceContext) = reflectPure(DenseVectorObjectFlatten(pieces))  

  /////////////////////
  // class interface

  def densevector_length[A:Manifest](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(DenseVectorLength(x))
  def densevector_isrow[A:Manifest](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(DenseVectorIsRow(x))
  def densevector_apply[A:Manifest](x: Exp[DenseVector[A]], n: Exp[Int])(implicit ctx: SourceContext) = reflectPure(DenseVectorApply(x, n))
  def densevector_trans[A:Manifest](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(DenseVectorTrans(x))
  def densevector_mutable_trans[A:Manifest](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorMutableTrans(x))
  def densevector_update[A:Manifest](x: Exp[DenseVector[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorUpdate(x, n, y))
  def densevector_copyfrom[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorCopyFrom(x, pos, y))
  def densevector_insert[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorInsert(x, pos, y))
  def densevector_insertall[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorInsertAll(x, pos, y))
  def densevector_removeall[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorRemoveAll(x, pos, len))
  def densevector_trim[A:Manifest](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorTrim(x))
  def densevector_clear[A:Manifest](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorClear(x))
  def densevector_times_matrix[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(DenseVectorTimesMatrix(x,y))
  def densevector_sort[A:Manifest:Ordering](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(DenseVectorSort(x))
  
  /////////////
  // internal
  
  def densevector_raw_data[A:Manifest](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(DenseVectorRawData(x))
  def densevector_set_raw_data[A:Manifest](x: Exp[DenseVector[A]], newVal: Exp[DeliteArray[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorSetRawData(x, newVal))
  def densevector_set_length[A:Manifest](x: Exp[DenseVector[A]], newVal: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorSetLength(x, newVal))
  def densevector_set_isrow[A:Manifest](x: Exp[DenseVector[A]], newVal: Exp[Boolean])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorSetIsRow(x, newVal))
  
  ///////////
  // other
  
  def densevector_empty_double(implicit ctx: SourceContext) = DenseVectorEmptyDouble()
  def densevector_empty_float(implicit ctx: SourceContext) = DenseVectorEmptyFloat()
  def densevector_empty_int(implicit ctx: SourceContext) = DenseVectorEmptyInt()
  def densevector_empty[A:Manifest](implicit ctx: SourceContext) = DenseVectorEmpty[A]()
  def densevector_zero_double(length: Exp[Int], isRow: Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(DenseVectorZeroDouble(length, isRow))
  def densevector_zero_float(length: Exp[Int], isRow: Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(DenseVectorZeroFloat(length, isRow))
  def densevector_zero_int(length: Exp[Int], isRow: Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(DenseVectorZeroInt(length, isRow))
  
  
  // /////////////////////
  // delite collection
  
  def isDenseVec[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[DenseVector[A]])  
  def asDenseVec[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[DenseVector[A]]]
    
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isDenseVec(x)) asDenseVec(x).length
    else super.dc_size(x)
  }
  
  override def dc_set_logical_size[A:Manifest](x: Exp[DeliteCollection[A]], y: Exp[Int])(implicit ctx: SourceContext) = {
    if (isDenseVec(x)) densevector_set_length(asDenseVec(x), y)
    else super.dc_set_logical_size(x,y)        
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isDenseVec(x)) asDenseVec(x).apply(n)
    else super.dc_apply(x,n)    
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isDenseVec(x)) asDenseVec(x).update(n,y)
    else super.dc_update(x,n,y)        
  }
  
  override def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isDenseVec(x)) { asDenseVec(x) += y; unit(true) }
    else super.dc_append(x,i,y)        
  }  
  
  override def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext): Exp[CA] = {
    if (isDenseVec(x)) {
      val out = DenseVector[A](size, asDenseVec(x).isRow)
      out.asInstanceOf[Exp[CA]]
    }
    else super.dc_alloc[A,CA](x,size)
  }  
  
  override def dc_copy[A:Manifest](src: Exp[DeliteCollection[A]], srcPos: Exp[Int], dst: Exp[DeliteCollection[A]], dstPos: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    if (isDenseVec(src) && isDenseVec(dst)) {
      darray_unsafe_copy(densevector_raw_data(asDenseVec(src)), srcPos, densevector_raw_data(asDenseVec(dst)), dstPos, size)
    }
    else super.dc_copy(src,srcPos,dst,dstPos,size)
  }      
    
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case DenseVectorEmptyDouble() => reflectPure(DenseVectorEmptyDouble())(mtype(manifest[A]),implicitly[SourceContext])
    case DenseVectorEmptyFloat() => reflectPure(DenseVectorEmptyFloat())(mtype(manifest[A]),implicitly[SourceContext])
    case DenseVectorEmptyInt() => reflectPure(DenseVectorEmptyInt())(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorEmpty() => reflectPure(DenseVectorEmpty()(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case DenseVectorZeroDouble(l,r) => reflectPure(DenseVectorZeroDouble(f(l),f(r)))(mtype(manifest[A]),implicitly[SourceContext])
    case DenseVectorZeroFloat(l,r) => reflectPure(DenseVectorZeroFloat(f(l),f(r)))(mtype(manifest[A]),implicitly[SourceContext])
    case DenseVectorZeroInt(l,r) => reflectPure(DenseVectorZeroInt(f(l),f(r)))(mtype(manifest[A]),implicitly[SourceContext])        
    case e@DenseVectorLength(x) => reflectPure(DenseVectorLength(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorIsRow(x) => reflectPure(DenseVectorIsRow(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorRawData(x) => reflectPure(DenseVectorRawData(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorApply(x,n) => reflectPure(DenseVectorApply(f(x),f(n))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    
    // delite ops
    case e@DenseVectorObjectFromSeq(xs) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectFromSeq(f(xs))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorObjectFromUnliftedSeq(xs) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectFromUnliftedSeq(f(xs))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorObjectOnes(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectOnes(f(x)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorObjectOnesF(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectOnesF(f(x)))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@DenseVectorObjectZeros(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectZeros(f(x)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorObjectZerosF(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectZerosF(f(x)))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@DenseVectorObjectRand(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectRand(f(x)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorObjectRandF(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectRandF(f(x)))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@DenseVectorObjectUniform(x,y,z,w) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectUniform(f(x),f(y),f(z),f(w)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorObjectFlatten(xs) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectFlatten(f(xs))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])        
    //case e@DenseVectorApply(x,n) => reflectPure(new { override val original = Some(f,e) } with DenseVectorApply(f(x),f(n))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorSort(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorSort(f(x))(e.mA,e.o))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorTimesMatrix(x,y) => reflectPure(new { override val original = Some(f,e) } with DenseVectorTimesMatrix(f(x),f(y))(e.mA,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorTrans(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorTrans(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    // read/write effects
    case Reflect(e@DenseVectorZeroDouble(l,r), u, es) => reflectMirrored(Reflect(DenseVectorZeroDouble(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@DenseVectorZeroFloat(l,r), u, es) => reflectMirrored(Reflect(DenseVectorZeroFloat(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@DenseVectorZeroInt(l,r), u, es) => reflectMirrored(Reflect(DenseVectorZeroInt(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@DenseVectorNew(l,r), u, es) => reflectMirrored(Reflect(DenseVectorNew(f(l),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@DenseVectorSetLength(x,v), u, es) => reflectMirrored(Reflect(DenseVectorSetLength(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@DenseVectorSetIsRow(x,v), u, es) => reflectMirrored(Reflect(DenseVectorSetIsRow(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))  
    case Reflect(e@DenseVectorSetRawData(x,v), u, es) => reflectMirrored(Reflect(DenseVectorSetRawData(f(x),f(v))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))              
    case Reflect(e@DenseVectorLength(x), u, es) => reflectMirrored(Reflect(DenseVectorLength(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorIsRow(x), u, es) => reflectMirrored(Reflect(DenseVectorIsRow(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@DenseVectorRawData(x), u, es) => reflectMirrored(Reflect(DenseVectorRawData(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorApply(x,n), u, es) => reflectMirrored(Reflect(DenseVectorApply(f(x),f(n))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorUpdate(x,n,y), u, es) => reflectMirrored(Reflect(DenseVectorUpdate(f(x),f(n),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorObjectFromSeq(xs), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorObjectFromSeq(f(xs))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorObjectOnes(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorObjectOnes(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorObjectOnesF(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorObjectOnesF(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorObjectZeros(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorObjectZeros(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorObjectZerosF(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorObjectZerosF(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorObjectRand(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorObjectRand(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorObjectRandF(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorObjectRandF(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorObjectUniform(x,y,z,w), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorObjectUniform(f(x),f(y),f(z),f(w)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorObjectFlatten(xs), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorObjectFlatten(f(xs))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    //case Reflect(e@DenseVectorApply(x,n), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorApply(f(x),f(n))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorSort(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorSort(f(x))(e.mA,e.o), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorTimesMatrix(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorTimesMatrix(f(x),f(y))(e.mA,e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorTrans(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorTrans(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorCopyFrom(x,pos,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorCopyFrom(f(x),f(pos),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorInsert(x,pos,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorInsert(f(x),f(pos),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorInsertAll(x,pos,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorInsertAll(f(x),f(pos),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorRemoveAll(x,pos,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorRemoveAll(f(x),f(pos),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorTrim(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorTrim(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorClear(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorClear(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    //case Reflect(e@DenseVectorUpdate(x,n,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorUpdate(f(x),f(n),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorMutableTrans(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorMutableTrans(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??


  /////////////////////
  // aliases and sharing

  // TODO: precise sharing info for other IR types (default is conservative)
  
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case DenseVectorApply(a,i) => Nil
    case DenseVectorUpdate(a,i,x) => Nil           // syms(a) <-- any use to return a?
    case DenseVectorInsert(a,i,x) => Nil           // syms(a) <-- any use to return a?
    case DenseVectorInsertAll(a,i,x) => Nil        // syms(a) <-- any use to return a?
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case DenseVectorApply(a,i) => Nil
    case DenseVectorUpdate(a,i,x) => syms(x)
    case DenseVectorInsert(a,i,x) => syms(x)
    case DenseVectorInsertAll(a,i,x) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case DenseVectorApply(a,i) => syms(a)
    case DenseVectorUpdate(a,i,x) => Nil
    case DenseVectorInsert(a,i,x) => Nil
    case DenseVectorInsertAll(a,i,x) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case DenseVectorApply(a,i) => Nil
    case DenseVectorUpdate(a,i,x) => syms(a)
    case DenseVectorInsert(a,i,x) => syms(a)
    case DenseVectorInsertAll(a,i,x) => syms(a) ++ syms(x)
    case _ => super.copySyms(e)
  }  
}

/**
 * Optimizations for composite DenseVectorOps operations.
 */

// have to extend DeliteCollectionOps to override dc_apply...
trait DenseVectorOpsExpOpt extends DenseVectorOpsExp with DeliteCollectionOpsExp {
  this: DenseVectorImplOps with OptiLAExp =>

  override def densevector_length[A:Manifest](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = x match {
    /* these are essential for fusing:    */
//    case Def(Reflect(e @ DenseVectorTimes(_,_), _,_)) => e.asInstanceOf[DeliteOpDenseVectorLoop[A]].size // FIXME: in general this is unsafe, but hey...
    case Def(DenseVectorNew(len, isRow)) => len
    //case Def(Reflect(DenseVectorNew(len, isRow), _,_)) => len // FIXME: in general this is unsafe, but hey...
    //case Def(Reflect(e @ DenseVectorObjectZeros(l), _,_)) => l // FIXME: in general this is unsafe, but hey...
    //case Def(Reflect(e @ DenseVectorClone(a), _,_)) => densevector_length(a) // FIXME: in general this is unsafe, but hey...
    case Def(DenseVectorObjectZeros(l)) => l
    case Def(DenseVectorEmptyDouble()) => Const(0)
    case Def(DenseVectorEmptyFloat()) => Const(0)
    case Def(DenseVectorEmptyInt()) => Const(0)
    case Def(DenseVectorEmpty()) => Const(0)
    case Def(DenseVectorZeroDouble(l,r)) => l
    case Def(DenseVectorZeroFloat(l,r)) => l
    case Def(DenseVectorZeroInt(l,r)) => l
    //case Def(DenseVectorClone(a)) => densevector_length(a)
    //case Def(DenseVectorObjectRange(s,e,d,r)) => (e - s + d - 1) / d

    /* propagate output size information */
    // some of this could be handled in DeliteCollectionOps, but we need a way to link length (for single tasks)
    // and size (for data parallel tasks) together. DenseVector can override dc_size, but has to deal with erasure.    
    case Def(e: DeliteOpMap[_,_,_]) => e.size
    case Def(e: DeliteOpZipWith[_,_,_,_]) => e.size
    //case Def(Reflect(e: DeliteOpMap[_,_,_], _,_)) => e.size // reasonable? mutable things may change size...
    //case Def(Reflect(e: DeliteOpZipWith[_,_,_,_], _,_)) => e.size // reasonable?
    //    case Def(e: DeliteOpDenseVectorLoop[A]) => e.size
    
    case Def(VectorSlice(a, start, end)) => end - start
    //case Def(DenseVectorMutableTrans(a)) => a.length
    //case Def(DenseVectorConcatenate(a,b)) => a.length + b.length   
    case Def(DenseVectorSort(a)) => a.length
    case Def(DenseVectorObjectFromUnliftedSeq(xs)) => Const(xs.length)
    case _ => 
      //printerr("could not short-circuit call to " + x.toString + ".length")
      //printerr(findDefinition(x.asInstanceOf[Sym[DenseVector[A]]]))
      super.densevector_length(x)
  }

  override def densevector_isrow[A:Manifest](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = x match {
    case Def(e: VectorArithmeticMap[A,_]) => e.intf.isRow 
    case Def(e: VectorArithmeticZipWith[A,_]) => e.intfA.isRow 
    //case Def(e: DeliteOpDenseVectorLoop[A]) => e.isRow
    //case Def(e: DenseVectorDeliteOp[A] => e.isRow)
    case Def(VectorClone(a)) => a.isRow
    case Def(DenseVectorEmptyDouble()) => Const(true)
    case Def(DenseVectorEmptyFloat()) => Const(true)
    case Def(DenseVectorEmptyInt()) => Const(true)
    case Def(DenseVectorEmpty()) => Const(true)
    case Def(DenseVectorZeroDouble(l,r)) => r
    case Def(DenseVectorZeroFloat(l,r)) => r
    case Def(DenseVectorZeroInt(l,r)) => r
    case Def(DenseVectorObjectFromUnliftedSeq(xs)) => Const(true)
    //case Def(Reflect(DenseVectorObjectZeros(l,r), _)) => r    
    //case Def(DenseVectorObjectRange(s,e,d,r)) => r
    case _ => super.densevector_isrow(x)
  }
  
  // and this one also helps in the example:
  def densevector_optimize_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext): Option[Exp[A]] = {
    val out = (x,n) match {
      case (Def(DenseVectorObjectFromUnliftedSeq(xs)), Const(a)) => Some(xs(a))
      case _ => None
    }
    
    if (out.isDefined) out
    else {
      x match {
        case Def(DenseVectorObjectZeros(l)) => Some(unit(0.0).asInstanceOf[Exp[A]])
        case Def(DenseVectorObjectOnes(l)) => Some(unit(1.0).asInstanceOf[Exp[A]])
        //case Def(DenseVectorObjectRange(s,e,d,r)) => Some((s + n*d).asInstanceOf[Exp[A]])
        case Def(s@DenseVectorEmptyDouble()) => throw new IndexOutOfBoundsException(s + " has no elements")
        case Def(s@DenseVectorEmptyFloat()) => throw new IndexOutOfBoundsException(s + " has no elements")
        case Def(s@DenseVectorEmptyInt()) => throw new IndexOutOfBoundsException(s + " has no elements")
        case Def(s@DenseVectorEmpty()) => throw new IndexOutOfBoundsException(s + " has no elements")
        case Def(DenseVectorZeroDouble(l,r)) => Some(Const(0.0).asInstanceOf[Exp[A]])
        case Def(DenseVectorZeroFloat(l,r)) => Some(Const(0f).asInstanceOf[Exp[A]])
        case Def(DenseVectorZeroInt(l,r)) => Some(Const(0).asInstanceOf[Exp[A]])
        case Def(DenseVectorTrans(x)) => Some(densevector_apply(x,n))
        case _ => None
      }
    }
  }
  
  override def densevector_apply[A:Manifest](x: Exp[DenseVector[A]], n: Exp[Int])(implicit ctx: SourceContext) = { 
    densevector_optimize_apply(x.asInstanceOf[Exp[DeliteCollection[A]]],n) getOrElse super.densevector_apply(x,n)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    densevector_optimize_apply(x,n) getOrElse super.dc_apply(x,n)
  }
}


trait BaseGenDenseVectorOps extends GenericFatCodegen {
  val IR: DenseVectorOpsExp
  import IR._
  
  override def unapplySimpleIndex(e: Def[Any]) = e match { // TODO: move elsewhere
    case DenseVectorApply(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }  
}

trait ScalaGenDenseVectorOps extends BaseGenDenseVectorOps with ScalaGenFat {
  val IR: DenseVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // these are the ops that call through to the underlying real data structure
    
    //     case DenseVectorZeroDouble(length, isRow) => emitValDef(sym, "new generated.scala.ZeroVectorDoubleImpl(" + quote(length) + ", " + quote(isRow) + ")")
    //     case DenseVectorZeroFloat(length, isRow) => emitValDef(sym, "new generated.scala.ZeroVectorFloatImpl(" + quote(length) + ", " + quote(isRow) + ")")
    //     case DenseVectorZeroInt(length, isRow) => emitValDef(sym, "new generated.scala.ZeroVectorIntImpl(" + quote(length) + ", " + quote(isRow) + ")")
    //     case DenseVectorEmptyDouble() => emitValDef(sym, "generated.scala.EmptyVectorDoubleImpl")
    //     case DenseVectorEmptyFloat() => emitValDef(sym, "generated.scala.EmptyVectorFloatImpl")
    //     case DenseVectorEmptyInt() => emitValDef(sym, "generated.scala.EmptyVectorIntImpl")
    //     case v@DenseVectorEmpty() => emitValDef(sym, "new generated.scala.EmptyVectorImpl[" + remap(v.mA) + "]")
    //     case v@DenseVectorNew(length, isRow) => emitValDef(sym, "new generated.scala.VectorImpl[" + remap(v.mA) + "](" + quote(length) + "," + quote(isRow) + ")")        
    case v@DenseVectorNew(length, isRow) => emitValDef(sym, "new " + remap("generated.scala.DenseVector[" + remap(v.mA) + "]")+"(" + quote(length) + "," + quote(isRow) + ")")        
    case DenseVectorApply(x,n) => emitValDef(sym, quote(x) + "._data(" + quote(n) + ")")
    case DenseVectorUpdate(x,n,y) => emitValDef(sym, quote(x) + "._data(" + quote(n) + ") = " + quote(y))
    case DenseVectorLength(x) => emitValDef(sym, quote(x) + "._length")
    case DenseVectorIsRow(x) => emitValDef(sym, quote(x) + "._isRow")
    case DenseVectorRawData(x) => emitValDef(sym, quote(x) + "._data")
    case DenseVectorSetLength(x,v) => emitValDef(sym, quote(x) + "._length = " + quote(v))
    case DenseVectorSetIsRow(x,v) => emitValDef(sym, quote(x) + "._isRow = " + quote(v))
    case DenseVectorSetRawData(x,v) => emitValDef(sym, quote(x) + "._data = " + quote(v))
    // case DenseVectorMutableTrans(x) => emitValDef(sym, quote(x) + ".mtrans")
    // case DenseVectorSort(x) => emitValDef(sym, quote(x) + ".sort")
    // case DenseVectorCopyFrom(x,pos,y) => emitValDef(sym, quote(x) + ".copyFrom(" + quote(pos) + ", " + quote(y) + ")")
    // case DenseVectorInsert(x,pos,y) => emitValDef(sym, quote(x) + ".insert(" + quote(pos) + ", " + quote(y) + ")")
    // case DenseVectorInsertAll(x,pos,y) => emitValDef(sym, quote(x) + ".insertAll(" + quote(pos) + ", " + quote(y) + ")")
    // case DenseVectorRemoveAll(x,pos,len) => emitValDef(sym, quote(x) + ".removeAll(" + quote(pos) + ", " + quote(len) + ")")
    // case DenseVectorTrim(x) => emitValDef(sym, quote(x) + ".trim")
    // case DenseVectorClear(x) => emitValDef(sym, quote(x) + ".clear()")
    
    case DenseVectorZeroDouble(length, isRow) => emitValDef(sym, "new " + remap("generated.scala.DenseVector[Double]")+"(" + quote(length) + ", " + quote(isRow) + ")")
    case DenseVectorZeroFloat(length, isRow) => emitValDef(sym, "new " + remap("generated.scala.DenseVector[Float]")+"(" + quote(length) + ", " + quote(isRow) + ")")
    case DenseVectorZeroInt(length, isRow) => emitValDef(sym, "new " + remap("generated.scala.DenseVector[Int]")+"(" + quote(length) + ", " + quote(isRow) + ")")
    case DenseVectorEmptyDouble() => emitValDef(sym, "new " + remap("generated.scala.DenseVector[Double]")+"(0,true)")
    case DenseVectorEmptyFloat() => emitValDef(sym, "new " + remap("generated.scala.DenseVector[Float]")+"(0,true)")
    case DenseVectorEmptyInt() => emitValDef(sym, "new " + remap("generated.scala.DenseVector[Int]")+"(0,true)")
    case v@DenseVectorEmpty() => emitValDef(sym, "new " + remap("generated.scala.DenseVector[" + remap(v.mA) + "]")+"(0,true)")
    
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenDenseVectorOps extends BaseGenDenseVectorOps with CudaGenFat with CudaGenDataStruct {
  val IR: DenseVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DenseVectorApply(x,n) => emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case DenseVectorUpdate(x,n,y) => stream.println(quote(x) + ".update(" + quote(n) + "," + quote(y) + ");")
    case DenseVectorLength(x) => emitValDef(sym, quote(x) + ".length")
    case DenseVectorIsRow(x) => emitValDef(sym, quote(x) + ".isRow")
    case DenseVectorRawData(x) => emitValDef(sym, quote(x) + ".getdata()")
    case DenseVectorSetLength(x,v) => stream.println(quote(x) + ".length = " + quote(v) + ";")
    case DenseVectorSetIsRow(x,v) => stream.println(quote(x) + ".isRow = " + quote(v) + ";")
    case DenseVectorSetRawData(x,v) => stream.println(quote(x) + ".setdata(" + quote(v) + ");")

    case DenseVectorNew(length, isRow) => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(%s,%s);".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(length),quote(isRow)))
    case DenseVectorZeroDouble(length, isRow) => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(%s,%s,0);".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(length),quote(isRow)))
    case DenseVectorZeroFloat(length, isRow) => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(%s,%s,0);".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(length),quote(isRow)))
    case DenseVectorZeroInt(length, isRow) => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(%s,%s,0);".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(length),quote(isRow)))
    case DenseVectorEmptyDouble() => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(0,true);".format(remap(sym.tp),quote(sym),remap(sym.tp)))
    case DenseVectorEmptyFloat() => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(0,true);".format(remap(sym.tp),quote(sym),remap(sym.tp)))
    case DenseVectorEmptyInt() => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(0,true);".format(remap(sym.tp),quote(sym),remap(sym.tp)))
    case DenseVectorEmpty() => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(0,true);".format(remap(sym.tp),quote(sym),remap(sym.tp)))
    
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenDenseVectorOps extends BaseGenDenseVectorOps with OpenCLGenFat with OpenCLGenDataStruct {
  val IR: DenseVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DenseVectorApply(x,n) => emitValDef(sym, remap(x.tp) + "_apply(" + quote(x) + "," + quote(n) + ")")
    case DenseVectorUpdate(x,n,y) => stream.println(remap(x.tp) + "_update(" + quote(x) + "," + quote(n) + "," + quote(y) + ");")
    case DenseVectorLength(x) => emitValDef(sym, quote(x) + ".length")
    case DenseVectorIsRow(x) => emitValDef(sym, quote(x) + ".isRow")
    case DenseVectorRawData(x) => emitValDef(sym, remap(x.tp) + "_getdata(" + quote(x) + ")")
    case DenseVectorSetLength(x,v) => stream.println(quote(x) + ".length = " + quote(v) + ";")
    case DenseVectorSetIsRow(x,v) => stream.println(quote(x) + ".isRow = " + quote(v) + ";")
    case DenseVectorSetRawData(x,v) => stream.println(remap(x.tp) + "_setdata(" + quote(x) + "," + quote(v) + ");")

    case DenseVectorNew(length, isRow) => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(%s,%s);".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(length),quote(isRow)))
    case DenseVectorZeroDouble(length, isRow) => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(%s,%s,0);".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(length),quote(isRow)))
    case DenseVectorZeroFloat(length, isRow) => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(%s,%s,0);".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(length),quote(isRow)))
    case DenseVectorZeroInt(length, isRow) => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(%s,%s,0);".format(remap(sym.tp),quote(sym),remap(sym.tp),quote(length),quote(isRow)))
    case DenseVectorEmptyDouble() => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(0,true);".format(remap(sym.tp),quote(sym),remap(sym.tp)))
    case DenseVectorEmptyFloat() => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(0,true);".format(remap(sym.tp),quote(sym),remap(sym.tp)))
    case DenseVectorEmptyInt() => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(0,true);".format(remap(sym.tp),quote(sym),remap(sym.tp)))
    case DenseVectorEmpty() => checkGPUAlloc(sym); stream.println(addTab()+"%s *%s_ptr = new %s(0,true);".format(remap(sym.tp),quote(sym),remap(sym.tp)))

    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenDenseVectorOps extends BaseGenDenseVectorOps with CGenFat {
  val IR: DenseVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DenseVectorApply(x, n) =>
      emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case DenseVectorUpdate(x,n,y) =>
      stream.println("%s.update(%s,%s);".format(quote(x),quote(n),quote(y)))
    case DenseVectorLength(x)    =>
      emitValDef(sym, quote(x) + ".length")
    case DenseVectorIsRow(x)     =>
      emitValDef(sym, quote(x) + ".isRow")

    case _ => super.emitNode(sym, rhs)
  }
}

