package ppl.dsl.optila.vector

import ppl.dsl.optila.CudaGenDataStruct
import java.io.{PrintWriter}

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import scala.reflect.Manifest
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import ppl.dsl.optila._

trait DenseVectorOps extends Variables {
  this: OptiLA =>

  implicit def repToDenseVecOps[A:Manifest](x: Rep[DenseVector[A]]) = new DenseVecOpsCls(x)
  implicit def varToDenseVecOps[A:Manifest](x: Var[DenseVector[A]]) = new DenseVecOpsCls(readVar(x))
  implicit def denseVecToInterface[A:Manifest](lhs: Rep[DenseVector[A]]) = new VInterface[A](new DenseVecOpsCls[A](lhs))

  implicit def denseVectorBuilder[A:Manifest] = new VectorBuilder[A,DenseVector[A]] {
    def alloc(length: Rep[Int], isRow: Rep[Boolean]) = {
      Vector.dense[A](length, isRow)
    }
    def toIntf(x: Rep[DenseVector[A]]): Interface[Vector[A]] = denseVecToInterface(x)
  }  

  object DenseVector {
    def apply[A:Manifest](len: Int, isRow: Boolean) = densevector_obj_new(unit(len), unit(isRow)) // needed to resolve ambiguities
    def apply[A](len: Rep[Int], isRow: Rep[Boolean])(implicit mA: Manifest[A], o: Overloaded1) = densevector_obj_new(len, isRow)
    def apply[A](xs: Rep[A]*)(implicit mA: Manifest[A], o: Overloaded2) = {
      val out = densevector_obj_new[A](unit(0),unit(true))
      // interpreted (not lifted)
      xs.foreach { out += _ }
      out.unsafeImmutable // return immutable object
    }
    
    def ones(len: Rep[Int]) = densevector_obj_ones(len)
    def onesf(len: Rep[Int]) = densevector_obj_onesf(len)
    def zeros(len: Rep[Int]) = densevector_obj_zeros(len)
    def zerosf(len: Rep[Int]) = densevector_obj_zerosf(len)
    def rand(len: Rep[Int]) = densevector_obj_rand(len)
    def randf(len: Rep[Int]) = densevector_obj_randf(len)
    def uniform(start: Rep[Double], step_size: Rep[Double], end: Rep[Double], isRow: Rep[Boolean] = unit(true)) =
      densevector_obj_uniform(start, step_size, end, isRow)    
    def flatten[A:Manifest](pieces: Rep[DenseVector[DenseVector[A]]]) = densevector_obj_flatten(pieces)
  }

  class DenseVecOpsCls[A:Manifest](val elem: Rep[DenseVector[A]]) extends VecOpsCls[A] {
    type V[X] = DenseVector[X]        
    type Self = DenseVector[A]
    
    def wrap(x: Rep[DenseVector[A]]) = denseVecToInterface(x)
    def toOps[B:Manifest](x: Rep[DenseVector[B]]) = repToDenseVecOps(x)
    def toIntf[B:Manifest](x: Rep[DenseVector[B]]): Interface[Vector[B]] = denseVecToInterface(x)
    def builder[B:Manifest]: VectorBuilder[B,V[B]] = denseVectorBuilder[B]    
    def mV[B:Manifest] = manifest[DenseVector[B]] 
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
        
    // generic arithmetic
    type VPLUSR = DenseVector[A]
    val mVPLUSR = manifest[VPLUSR]
    val vplusBuilder = builder[A]
    def vplusToIntf(x: Rep[VPLUSR]) = toIntf(x)

    type VMINUSR = DenseVector[A]
    val mVMINUSR = manifest[VMINUSR]
    val vminusBuilder = builder[A]
    def vminusToIntf(x: Rep[VMINUSR]) = toIntf(x)    
    
    type VTIMESR = DenseVector[A]
    val mVTIMESR = manifest[VTIMESR]
    val vtimesBuilder = builder[A]
    def vtimesToIntf(x: Rep[VTIMESR]) = toIntf(x)        
    
    def *(y: Rep[DenseMatrix[A]])(implicit a: Arith[A],o: Overloaded2, ctx: SourceContext) = densevector_times_matrix(elem,y)
    
    // ordering operations
    def sort(implicit o: Ordering[A], ctx: SourceContext) = densevector_sort(elem)
    
    // bulk operations
    def flatMap[B:Manifest](f: Rep[A] => Rep[DenseVector[B]])(implicit ctx: SourceContext) = densevector_flatmap(elem,f)
    def partition(pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext) = densevector_partition(elem,pred)
    def groupBy[K:Manifest](pred: Rep[A] => Rep[K])(implicit ctx: SourceContext) = densevector_groupby(elem,pred)                  
  }

  // object defs
  def densevector_obj_new[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]): Rep[DenseVector[A]]
  def densevector_obj_fromseq[A:Manifest](xs: Rep[Seq[A]]): Rep[DenseVector[A]]
  def densevector_obj_ones(len: Rep[Int]): Rep[DenseVector[Double]]
  def densevector_obj_onesf(len: Rep[Int]): Rep[DenseVector[Float]]
  def densevector_obj_zeros(len: Rep[Int]): Rep[DenseVector[Double]]
  def densevector_obj_zerosf(len: Rep[Int]): Rep[DenseVector[Float]]
  def densevector_obj_rand(len: Rep[Int]): Rep[DenseVector[Double]]
  def densevector_obj_randf(len: Rep[Int]): Rep[DenseVector[Float]]
  def densevector_obj_uniform(start: Rep[Double], step_size: Rep[Double], end: Rep[Double], isRow: Rep[Boolean]): Rep[DenseVector[Double]]
  def densevector_obj_flatten[A:Manifest](pieces: Rep[DenseVector[DenseVector[A]]]): Rep[DenseVector[A]]  
  
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
  def densevector_flatmap[A:Manifest,B:Manifest](x: Rep[DenseVector[A]], f: Rep[A] => Rep[DenseVector[B]])(implicit ctx: SourceContext): Rep[DenseVector[B]]
  def densevector_partition[A:Manifest](x: Rep[DenseVector[A]], pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): (Rep[DenseVector[A]], Rep[DenseVector[A]])
  def densevector_groupby[A:Manifest,K:Manifest](x: Rep[DenseVector[A]], pred: Rep[A] => Rep[K])(implicit ctx: SourceContext): Rep[DenseVector[DenseVector[A]]]         
  
  // other defs
  def densevector_empty_double: Rep[DenseVector[Double]]
  def densevector_empty_float: Rep[DenseVector[Float]]
  def densevector_empty_int: Rep[DenseVector[Int]]
  def densevector_empty[A:Manifest]: Rep[DenseVector[A]]
  def densevector_zero_double(length: Rep[Int], isRow: Rep[Boolean]): Rep[DenseVector[Double]]
  def densevector_zero_float(length: Rep[Int], isRow: Rep[Boolean]): Rep[DenseVector[Float]]
  def densevector_zero_int(length: Rep[Int], isRow: Rep[Boolean]): Rep[DenseVector[Int]]  
}

trait DenseVectorCompilerOps extends DenseVectorOps {
  this: OptiLA =>
  
  def densevector_raw_data[A:Manifest](x: Rep[DenseVector[A]])(implicit ctx: SourceContext): Rep[Array[A]]
  def densevector_set_raw_data[A:Manifest](x: Rep[DenseVector[A]], data: Rep[Array[A]])(implicit ctx: SourceContext): Rep[Unit]
  def densevector_set_length[A:Manifest](x: Rep[DenseVector[A]], newVal: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def densevector_set_isrow[A:Manifest](x: Rep[DenseVector[A]], newVal: Rep[Boolean])(implicit ctx: SourceContext): Rep[Unit]
}

trait DenseVectorOpsExp extends DenseVectorOps with VariablesExp with BaseFatExp {

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
  
  case class DenseVectorLength[A:Manifest](x: Exp[DenseVector[A]]) extends Def[Int]
  case class DenseVectorIsRow[A:Manifest](x: Exp[DenseVector[A]]) extends Def[Boolean]
  case class DenseVectorRawData[A:Manifest](x: Exp[DenseVector[A]]) extends DefWithManifest[A,Array[A]]
  case class DenseVectorSetLength[A:Manifest](x: Exp[DenseVector[A]], newVal: Exp[Int]) extends Def[Unit]
  case class DenseVectorSetIsRow[A:Manifest](x: Exp[DenseVector[A]], newVal: Exp[Boolean]) extends Def[Unit]
  case class DenseVectorSetRawData[A:Manifest](x: Exp[DenseVector[A]], newVal: Exp[Array[A]]) extends Def[Unit]
    
  /////////////////////////////////////////////////
  // implemented via kernel embedding (sequential)
  
  case class DenseVectorObjectFromSeq[A:Manifest](xs: Exp[Seq[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_fromseq_impl(xs)))

  case class DenseVectorObjectOnes(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_ones_impl(len)))

  case class DenseVectorObjectOnesF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_onesf_impl(len)))

  case class DenseVectorObjectZeros(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(reflectPure(DenseVectorNew[Double](len, Const(true))))) //densevector_obj_zeros_impl(len)))

  case class DenseVectorObjectZerosF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(reflectPure(DenseVectorNew[Float](len, Const(true))))) //densevector_obj_zerosf_impl(len)))
    
  case class DenseVectorObjectRand(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_rand_impl(len)))

  case class DenseVectorObjectRandF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_randf_impl(len)))

  case class DenseVectorObjectUniform(start: Exp[Double], step_size: Exp[Double], end: Exp[Double], isRow: Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_uniform_impl(start, step_size, end, isRow)))

  case class DenseVectorObjectFlatten[A:Manifest](pieces: Exp[DenseVector[DenseVector[A]]])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_flatten_impl(pieces)))
        
  case class DenseVectorApply[A:Manifest](x: Exp[DenseVector[A]], n: Exp[Int]) 
    extends DeliteOpSingleWithManifest[A,A](reifyEffectsHere(densevector_apply_impl(x,n)))
    
  case class DenseVectorUpdate[A:Manifest](x: Exp[DenseVector[A]], n: Exp[Int], y: Exp[A]) 
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_update_impl(x,n,y)))
    
  case class DenseVectorCopyFrom[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[DenseVector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_copyfrom_impl(x,pos,y)))
    
  case class DenseVectorInsert[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[A]) 
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_insert_impl(x,pos,y)))
    
  case class DenseVectorInsertAll[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[DenseVector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_insertall_impl(x,pos,y)))
    
  case class DenseVectorRemoveAll[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_removeall_impl(x,pos,len)))
    
  case class DenseVectorTrim[A:Manifest](x: Exp[DenseVector[A]]) 
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_trim_impl(x)))
    
  case class DenseVectorClear[A:Manifest](x: Exp[DenseVector[A]]) 
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_clear_impl(x)))
    
  case class DenseVectorMutableTrans[A:Manifest](x: Exp[DenseVector[A]]) 
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_mutabletrans_impl(x)))
    
  // TODO: right now we just use the underlying data structure sort, but we should implement our own
  // fast parallel sort with delite ops
  case class DenseVectorSort[A:Manifest:Ordering](x: Exp[DenseVector[A]]) 
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_sort_impl(x)))

  case class DenseVectorTimesMatrix[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseMatrix[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_times_matrix_impl[A](x,y)))

  case class DenseVectorPPrint[A](x: Exp[DenseVector[A]])(block: Exp[Unit]) // stupid limitation...
    extends DeliteOpSingleTask(block)
    // reifyEffects(densevector_pprint_impl[A](x))

//  case class DenseVectorTrans[A:Manifest](x: Exp[DenseVector[A]])
//    extends DeliteOpSingleTask(reifyEffectsHere(densevector_trans_impl[A](x)))

//  case class DenseVectorFilter[A:Manifest](x: Exp[DenseVector[A]], pred: Exp[A] => Exp[Boolean])
//    extends DeliteOpSingleTask(reifyEffectsHere(densevector_filter_impl(x, pred)))

  case class DenseVectorPartition[A:Manifest](x: Exp[DenseVector[A]], pred: Exp[A] => Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_partition_impl(x, pred)))

//  case class DenseVectorMinIndex[A:Manifest:Ordering](x: Exp[DenseVector[A]])
//    extends DeliteOpSingleTask(reifyEffectsHere(densevector_min_index_impl[A](x)))
//
//  case class DenseVectorMaxIndex[A:Manifest:Ordering](x: Exp[DenseVector[A]])
//    extends DeliteOpSingleTask(reifyEffectsHere(densevector_max_index_impl[A](x)))

//  case class DenseVectorFind[A:Manifest](x: Exp[DenseVector[A]], pred: Exp[A] => Exp[Boolean])
//    extends DeliteOpSingleTask(reifyEffectsHere(densevector_find_impl[A](x, pred)))

  case class DenseVectorGroupBy[A:Manifest,K:Manifest](x: Exp[DenseVector[A]], pred: Exp[A] => Exp[K])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_groupby_impl(x,pred)))
  
  
  ////////////////////////////////
  // implemented via delite ops

  case class DenseVectorTrans[A:Manifest](in: Exp[DenseVector[A]])
    extends DeliteOpMap[A,A,DenseVector[A]] {
    val size = copyTransformedOrElse(_.size)(in.length)

    def alloc = DenseVector[A](in.length, !in.isRow)
    def func = e => e 

    def m = manifest[A]
  }

  case class DenseVectorFlatMap[A:Manifest,B:Manifest](in: Exp[DenseVector[A]], map: Exp[A] => Exp[DenseVector[B]])
    extends DeliteOpMapReduce[A,DenseVector[B]] {

    val size = in.length
    val zero = EmptyVector[B]
    def reduce = (a,b) => a ++ b
  }

  /////////////////////
  // object interface

  def densevector_obj_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) = reflectMutable(DenseVectorNew[A](len, isRow)) //XXX
  def densevector_obj_fromseq[A:Manifest](xs: Exp[Seq[A]]) = reflectPure(DenseVectorObjectFromSeq(xs)) //XXX
  def densevector_obj_ones(len: Exp[Int]) = reflectPure(DenseVectorObjectOnes(len))
  def densevector_obj_onesf(len: Exp[Int]) = reflectPure(DenseVectorObjectOnesF(len))
  def densevector_obj_zeros(len: Exp[Int]) = densevector_zero_double(len,unit(true)) //reflectPure(DenseVectorObjectZeros(len))
  def densevector_obj_zerosf(len: Exp[Int]) = densevector_zero_float(len,unit(true))//reflectPure(DenseVectorObjectZerosF(len))
  def densevector_obj_rand(len: Exp[Int]) = reflectEffect(DenseVectorObjectRand(len))
  def densevector_obj_randf(len: Exp[Int]) = reflectEffect(DenseVectorObjectRandF(len))
  def densevector_obj_uniform(start: Exp[Double], step_size: Exp[Double], end: Exp[Double], isRow: Exp[Boolean]) = reflectPure(DenseVectorObjectUniform(start, step_size, end, isRow))
  def densevector_obj_flatten[A:Manifest](pieces: Exp[DenseVector[DenseVector[A]]]) = reflectPure(DenseVectorObjectFlatten(pieces))  

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

  def densevector_flatmap[A:Manifest,B:Manifest](x: Exp[DenseVector[A]], f: Exp[A] => Exp[DenseVector[B]])(implicit ctx: SourceContext) = reflectPure(DenseVectorFlatMap(x, f))
  def densevector_partition[A:Manifest](x: Exp[DenseVector[A]], pred: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) = t2(reflectPure(DenseVectorPartition(x, pred)))
  def densevector_groupby[A:Manifest,K:Manifest](x: Exp[DenseVector[A]], pred: Exp[A] => Exp[K])(implicit ctx: SourceContext) = reflectPure(DenseVectorGroupBy(x,pred))
  
  /////////////
  // internal
  
  def densevector_raw_data[A:Manifest](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(DenseVectorRawData(x))
  def densevector_set_raw_data[A:Manifest](x: Exp[DenseVector[A]], newVal: Exp[Array[A]])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorSetRawData(x, newVal))
  def densevector_set_length[A:Manifest](x: Exp[DenseVector[A]], newVal: Exp[Int])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorSetLength(x, newVal))
  def densevector_set_isrow[A:Manifest](x: Exp[DenseVector[A]], newVal: Exp[Boolean])(implicit ctx: SourceContext) = reflectWrite(x)(DenseVectorSetIsRow(x, newVal))
  
  ///////////
  // other
  
  def densevector_empty_double = DenseVectorEmptyDouble()
  def densevector_empty_float = DenseVectorEmptyFloat()
  def densevector_empty_int = DenseVectorEmptyInt()
  def densevector_empty[A:Manifest] = DenseVectorEmpty[A]()
  def densevector_zero_double(length: Exp[Int], isRow: Exp[Boolean]) = reflectMutable(DenseVectorZeroDouble(length, isRow))
  def densevector_zero_float(length: Exp[Int], isRow: Exp[Boolean]) = reflectMutable(DenseVectorZeroFloat(length, isRow))
  def densevector_zero_int(length: Exp[Int], isRow: Exp[Boolean]) = reflectMutable(DenseVectorZeroInt(length, isRow))
  
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case DenseVectorLength(x) => densevector_length(f(x))
    case DenseVectorIsRow(x) => densevector_isrow(f(x))
    case e@DenseVectorRawData(x) => densevector_raw_data(f(x))(e.m, implicitly[SourceContext])    
    case e@DenseVectorObjectOnes(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectOnes(f(x)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorObjectOnesF(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectOnesF(f(x)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorObjectUniform(x,y,z,w) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectUniform(f(x),f(y),f(z),f(w)))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@DenseVectorApply(x,n) => reflectPure(new { override val original = Some(f,e) } with DenseVectorApply(f(x),f(n)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorTrans(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorTrans(f(x))(e.m))(mtype(manifest[A]),implicitly[SourceContext])
    // read/write effects
    case Reflect(e@DenseVectorNew(l,r), u, es) => reflectMirrored(Reflect(DenseVectorNew(f(l),f(r))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@DenseVectorRawData(x), u, es) => reflectMirrored(Reflect(DenseVectorRawData(f(x))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorLength(x), u, es) => reflectMirrored(Reflect(DenseVectorLength(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorIsRow(x), u, es) => reflectMirrored(Reflect(DenseVectorIsRow(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@DenseVectorApply(x,n), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorApply(f(x),f(n))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorUpdate(l,i,r), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorUpdate(f(l),f(i),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorInsert(l,i,r), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorInsert(f(l),f(i),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorObjectZeros(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorObjectZeros(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
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

  // override def densevector_equals[A:Manifest](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]])(implicit ctx: SourceContext) = (x, y) match {
  //   case (a,b) if (a == b) => unit(true) // same symbol
  //   case _ => super.densevector_equals(x,y)
  // }

  // override def densevector_plus[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = (x, y) match {
  //   // (TB + TD) == T(B + D)
  //   case (Def(DenseVectorTimes(a, b)), Def(DenseVectorTimes(c, d))) if (a == c) => densevector_times[A](a.asInstanceOf[Exp[DenseVector[A]]], densevector_plus[A](b.asInstanceOf[Exp[DenseVector[A]]],d.asInstanceOf[Exp[DenseVector[A]]]))
  //   // ...
  //   case _ => super.densevector_plus(x, y)
  // }
  // 
  // override def densevector_plusequals[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = (x, y) match {
  //   // remove runtime check on zero densevector being same length as argument
  //   case (a, Def(DenseVectorObjectZeros(len))) => ()
  //   //case (Def(DenseVectorObjectZeros(len)), b) => b  // this is unsafe because we lose the effectful operation (e.g. accumulation)
  //   case _ => super.densevector_plusequals(x,y)
  // }
  // 
  // override def densevector_times[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = (x, y) match {
  //   case _ => super.densevector_times(x, y)
  // }

  // override def densevector_mutable_clone[A:Manifest](x: Exp[DenseVector[A]]) = x match {
  //     // these are unsafe in general.. we can only short-circuit the clone if we know the allocation is dead
  //     // except for the .mutable call
  //     // e.g., val x = DenseVector(10, true)
  //     //       val y = x.mutable // should clone!
  //     //       val z = x + 5
  //     // val x = DenseVector(10, true).mutable // should not clone!
  //     case Def(d@DenseVectorNew(len, isRow)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]])
  //     case Def(d@DenseVectorObjectFromSeq(xs)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]])   
  //     case Def(d@DenseVectorObjectZeros(len)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]])
  //     case Def(d@DenseVectorObjectZerosF(len)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]])
  //     //case Def(d@DenseVectorObjectOnes(len)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]]) <--- actually a problem in testSumIf!
  //     //case Def(d@DenseVectorObjectOnesF(len)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]])
  //     case Def(d@DenseVectorObjectRand(len)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]])
  //     case Def(d@DenseVectorObjectRandF(len)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]])
  //     case _ => super.densevector_mutable_clone(x)
  //   }

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
    
    //case Def(DenseVectorSlice(a, start, end)) => end - start
    //case Def(DenseVectorMutableTrans(a)) => a.length
    //case Def(DenseVectorConcatenate(a,b)) => a.length + b.length   
    case Def(DenseVectorSort(a)) => a.length
      
    case _ => 
      //printerr("could not short-circuit call to " + x.toString + ".length")
      //printerr(findDefinition(x.asInstanceOf[Sym[DenseVector[A]]]))
      super.densevector_length(x)
  }

  override def densevector_isrow[A:Manifest](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = x match {
    //case Def(e: DenseVectorArithmeticMap[A]) => e.in.asInstanceOf[Exp[DenseVector[A]]].isRow 
    //case Def(e: DenseVectorArithmeticZipWith[A]) => e.inA.asInstanceOf[Exp[DenseVector[A]]].isRow 
    //case Def(e: DeliteOpDenseVectorLoop[A]) => e.isRow
    //case Def(e: DenseVectorDeliteOp[A] => e.isRow)
    //case Def(Reflect(DenseVectorObjectZeros(l,r), _)) => r
    //case Def(DenseVectorClone(a)) => densevector_isrow(a)
    case Def(DenseVectorEmptyDouble()) => Const(true)
    case Def(DenseVectorEmptyFloat()) => Const(true)
    case Def(DenseVectorEmptyInt()) => Const(true)
    case Def(DenseVectorEmpty()) => Const(true)
    case Def(DenseVectorZeroDouble(l,r)) => r
    case Def(DenseVectorZeroFloat(l,r)) => r
    case Def(DenseVectorZeroInt(l,r)) => r
    //case Def(DenseVectorObjectRange(s,e,d,r)) => r
    case _ => super.densevector_isrow(x)
  }
  
  // and this one also helps in the example:
  def densevector_optimize_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext): Option[Exp[A]] = x match {
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

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    
    //     case DenseVectorZeroDouble(length, isRow) => emitValDef(sym, "new generated.scala.ZeroVectorDoubleImpl(" + quote(length) + ", " + quote(isRow) + ")")
    //     case DenseVectorZeroFloat(length, isRow) => emitValDef(sym, "new generated.scala.ZeroVectorFloatImpl(" + quote(length) + ", " + quote(isRow) + ")")
    //     case DenseVectorZeroInt(length, isRow) => emitValDef(sym, "new generated.scala.ZeroVectorIntImpl(" + quote(length) + ", " + quote(isRow) + ")")
    //     case DenseVectorEmptyDouble() => emitValDef(sym, "generated.scala.EmptyVectorDoubleImpl")
    //     case DenseVectorEmptyFloat() => emitValDef(sym, "generated.scala.EmptyVectorFloatImpl")
    //     case DenseVectorEmptyInt() => emitValDef(sym, "generated.scala.EmptyVectorIntImpl")
    //     case v@DenseVectorEmpty() => emitValDef(sym, "new generated.scala.EmptyVectorImpl[" + remap(v.mA) + "]")
    //     case v@DenseVectorNew(length, isRow) => emitValDef(sym, "new generated.scala.VectorImpl[" + remap(v.mA) + "](" + quote(length) + "," + quote(isRow) + ")")        
    case v@DenseVectorNew(length, isRow) => emitValDef(sym, "new " + remap("generated.scala.DenseVector[" + remap(v.m) + "]")+"(" + quote(length) + "," + quote(isRow) + ")")        
    // case DenseVectorApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
    // case DenseVectorUpdate(x,n,y) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))
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
    case v@DenseVectorEmpty() => emitValDef(sym, "new " + remap("generated.scala.DenseVector[" + remap(v.m) + "]")+"(0,true)")
    
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenDenseVectorOps extends BaseGenDenseVectorOps with CudaGenFat with CudaGenDataStruct {
  val IR: DenseVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DenseVectorNew(length, isRow) => stream.println(addTab()+"%s *%s_ptr = new %s(%s,%s);".format(remap(sym.Type),quote(sym),remap(sym.Type),quote(length),quote(isRow)))
    //case DenseVectorApply(x, n) =>
    //  emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    //case DenseVectorUpdate(x,n,y) =>
    //  stream.println(addTab() + "%s.update(%s,%s);".format(quote(x),quote(n),quote(y)))
    case DenseVectorLength(x)    =>
      emitValDef(sym, quote(x) + ".length")
    case DenseVectorIsRow(x)     =>
      emitValDef(sym, quote(x) + ".isRow")

	//case DenseVectorPlusEquals(x,y) =>
	//	throw new GenerationFailedException("CudaGen: No dimension specified for this kernel.")

    // case DenseVectorRepmat(x,i,j) =>
    //   currDim += 1
    //   val currDimStr = getCurrDimStr()
    //   setCurrDimLength("%s->length * %s * %s".format(quote(x),quote(i),quote(j)))
    //   stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(sym)))
    //   tabWidth += 1
    //   stream.println(addTab()+"int i = %s / (%s.length * %s);".format(currDimStr,quote(x),quote(j)))
    //   stream.println(addTab()+"int j = " + currDimStr + " % " + "(%s.length * %s);".format(quote(x),quote(j)))
    //   stream.println(addTab()+"%s.update(i,j,%s.apply(%s));".format(quote(sym),quote(x),"j%"+quote(x)+".length"))
    //   tabWidth -= 1
    //   stream.println(addTab()+"}")
    //   emitMatrixAlloc(sym,"%s".format(quote(i)),"%s.length*%s".format(quote(x),quote(j)),false)
    //   currDim -= 1
    
    /*    
    case DenseVectorOuter(x,y) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength(quote(x)+"->length")
      stream.println(addTab()+"if( %s < %s ) {".format(currDimStr,quote(x)+".size()"))
      tabWidth += 1
      stream.println(addTab()+"for(int i=0; i<%s.length; i++) {".format(quote(x))); tabWidth += 1
      stream.println(addTab()+"%s.update(%s, %s, %s.apply(%s)*%s.apply(%s));".format(quote(sym),"i",currDimStr,quote(x),"i",quote(y),currDimStr))
      tabWidth -= 1; stream.println(addTab()+"}")
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s.length".format(quote(x)),"%s.length".format(quote(x)),false)
      currDim -= 1
    */

    /* Test for using local variables */
    // case DenseVectorMinus(x,y) if(useLocalVar) =>
    //   currDim += 1
    //   val currDimStr = getCurrDimStr()
    //   setCurrDimLength(quote(x)+"->length")
    //   val outLocalVar = getNewLocalVar()
    //   val outIndex = if(indexMap.contains(sym)) indexMap.get(sym).get else currDimStr+"%"+quote(sym)+".size()"
    //   val inIndex = outIndex.replace(quote(sym),quote(x))
    //   //TODO: Check whether inputs are all from kernel inputs (otherwise, the recalculations need to percolate up
    //   stream.println(addTab()+"%s %s = %s.apply(%s) - %s.apply(%s);".format(remap(sym.Type.typeArguments(0)),outLocalVar,quote(x),inIndex,quote(y),inIndex))
    //   saveLocalVar(sym,outIndex,outLocalVar)
    //   currDim -= 1
    //   emitVectorAlloc(sym,"%s.length".format(quote(x)),"true",false)

    // case DenseVectorTrans(x) if(useLocalVar) =>
    //       currDim += 1
    //       val currDimStr = getCurrDimStr()
    //       setCurrDimLength(quote(x)+"->length")
    //       val outLocalVar = getNewLocalVar()
    //       val outIndex = if(indexMap.contains(sym)) indexMap.get(sym).get else currDimStr+"%"+quote(sym)+".size()"
    //       val inIndex = outIndex.replace(quote(sym),quote(x))
    //       if(hasLocalVar(x,inIndex)) {
    //         val inLocalVar = getLocalVar(x,inIndex)
    //         stream.println(addTab()+"%s %s = %s;".format(remap(sym.Type.typeArguments(0)),outLocalVar,inLocalVar))
    //       }
    //       else {
    //         val tp=findDefinition(x.asInstanceOf[Sym[_]]).get
    //         currDim -= 1
    //         indexMap.put(x,inIndex)
    //         emitNode(tp.sym,tp.rhs)
    //         indexMap.remove(x)
    //         currDim += 1
    //         val inLocalVar = getLocalVar(x,inIndex)
    //         stream.println(addTab()+"%s %s = %s;".format(remap(sym.Type.typeArguments(0)),outLocalVar,inLocalVar))
    //       }
    //       saveLocalVar(sym,outIndex,outLocalVar)
    //       currDim -= 1
    //       emitVectorAlloc(sym,"%s.length".format(quote(x)),"true",false)
    
    // case DenseVectorOuter(x,y) if(useLocalVar) =>
    //   currDim += 1
    //   val currDimStr = getCurrDimStr()
    //   setCurrDimLength(quote(x)+"->length*"+quote(x)+"->length")
    //   val outLocalVar = getNewLocalVar()
    //   val varX = if(hasLocalVar(x,currDimStr+"/"+quote(x)+".size()")) getLocalVar(x,currDimStr+"/"+quote(x)+".size()")
    //              else {
    //                val tp=findDefinition(x.asInstanceOf[Sym[_]]).get
    //                indexMap.put(x,currDimStr+"/"+quote(x)+".size()")
    //                currDim -= 1
    //                emitNode(tp.sym,tp.rhs)
    //                currDim += 1
    //                indexMap.remove(x)
    //                getLocalVar(x,currDimStr+"/"+quote(x)+".size()")
    //              }
    //   val varY = if(hasLocalVar(y,currDimStr+"%"+quote(y)+".size()")) getLocalVar(y,currDimStr+"%"+quote(y)+".size()")
    //              else {
    //                val tp=findDefinition(y.asInstanceOf[Sym[_]]).get
    //                indexMap.put(y,currDimStr+"%"+quote(y)+".size()")
    //                currDim -= 1
    //                emitNode(tp.sym,tp.rhs)
    //                currDim += 1
    //                indexMap.remove(y)
    //                getLocalVar(y,currDimStr+"%"+quote(y)+".size()")
    //              }
    //   stream.println(addTab()+"%s %s = %s * %s;".format(remap(sym.Type.typeArguments(0)),outLocalVar,varX,varY))
    //   saveLocalVar(sym,currDimStr,outLocalVar)
    //   currDim -= 1
    //   emitMatrixAlloc(sym,"%s.length".format(quote(x)),"%s.length".format(quote(x)),false)

    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenDenseVectorOps extends BaseGenDenseVectorOps with OpenCLGenFat with OpenCLGenDataStruct {
  val IR: DenseVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

    // Only allow allocating primitive type Vectors
    case DenseVectorNew(length, isRow) => {
      stream.println(addTab()+"%s *devPtr;".format(remap(sym.Type.typeArguments(0))))
      stream.println(addTab()+"DeliteOpenCLMalloc((void**)&devPtr,%s*sizeof(%s));".format(quote(length),remap(sym.Type.typeArguments(0))))
      stream.println(addTab()+"%s *%s_ptr = new %s(%s,%s,devPtr);".format(remap(sym.Type),quote(sym),remap(sym.Type),quote(length),quote(isRow)))
    }
	
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenDenseVectorOps extends BaseGenDenseVectorOps with CGenFat {
  val IR: DenseVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
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

