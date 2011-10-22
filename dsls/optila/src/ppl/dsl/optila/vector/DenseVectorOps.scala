package ppl.dsl.optila.vector

import ppl.dsl.optila.CudaGenDataStruct
import java.io.{PrintWriter}

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import ppl.dsl.optila._

trait DenseVectorOps extends Variables {
  this: OptiLA =>

  implicit def repToDenseVecOps[A:Manifest](x: Rep[DenseVector[A]]) = new DenseVecOpsCls(x)
  implicit def varToDenseVecOps[A:Manifest](x: Var[DenseVector[A]]) = new DenseVecOpsCls(readVar(x))
  implicit def denseToInterface[A:Manifest](lhs: Rep[DenseVector[A]]) = new VInterface[A](new DenseVecOpsCls[A](lhs))

  implicit def denseVectorBuilder[A:Manifest] = new VectorBuilder[A,DenseVector[A]] {
    def alloc(length: Rep[Int], isRow: Rep[Boolean]) = Vector.dense[A](length, isRow)
    def toIntf(x: Rep[DenseVector[A]]): Interface[Vector[A]] = denseToInterface(x)
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
  }

  class DenseVecOpsCls[A:Manifest](val elem: Rep[DenseVector[A]]) extends VecOpsCls[A] {
    //type VA = DenseVector[A]
    def mA: Manifest[A] = manifest[A]    
    // def toOps(x: Rep[DenseVector[A]]) = toOpsB[A](x)
    // def toIntf(x: Rep[DenseVector[A]]) = toIntfB[A](x)
    // def builder: VectorBuilder[A,DenseVector[A]] = builderB[A]
    // def mVA = mVB[A]
    
    type V[X] = DenseVector[X]        
    type Self = DenseVector[A]
    def wrap(x: Rep[DenseVector[A]]) = denseToInterface(x)
    def toOps[B:Manifest](x: Rep[DenseVector[B]]) = repToDenseVecOps(x)
    def toIntf[B:Manifest](x: Rep[DenseVector[B]]): Interface[Vector[B]] = denseToInterface(x)
    def builder[B:Manifest]: VectorBuilder[B,V[B]] = denseVectorBuilder[B]    
    def mV[B:Manifest] = manifest[DenseVector[B]] 

    // def dcSize = densevector_length(x)
    // def dcApply(n: Rep[Int]): Rep[A] = densevector_apply(x,n)
    // def dcUpdate(n: Rep[Int], y: Rep[A]): Rep[Unit] = densevector_update(x,n,y)

    // conversions
    // def toBoolean(implicit conv: Rep[A] => Rep[Boolean]) =  map(e => conv(e))
    // def toDouble(implicit conv: Rep[A] => Rep[Double]) =  map(e => conv(e))
    // def toFloat(implicit conv: Rep[A] => Rep[Float]) = map(e => conv(e))
    // def toInt(implicit conv: Rep[A] => Rep[Int]) = map(e => conv(e))
    // def toLong(implicit conv: Rep[A] => Rep[Long]) = map(e => conv(e))

    // accessors
    def length = densevector_length(elem)
    def isRow = densevector_isrow(elem)
    def apply(n: Rep[Int]) = densevector_apply(elem, n)
    // def isEmpty = length == 0
    // def first = apply(0)
    // def last = apply(repArithToArithOps(length) - 1) // TODO: why doesn't this get invoked implicitly?
    // def indices = (0::length)
    // def drop(count: Rep[Int]) = slice(count, length)
    // def take(count: Rep[Int]) = slice(0, count)
    // def slice(start: Rep[Int], end: Rep[Int]) = densevector_slice(x, start, end)
    // def contains(y: Rep[A]) = densevector_contains(x,y)
    // def distinct = densevector_distinct(x)
    
    // general
    def t = densevector_trans(elem)
    def mt() = densevector_mutable_trans(elem)
    // def cloneL() = densevector_clone(x)
    // def mutable() = densevector_mutable_clone(x)
    // def pprint() = densevector_pprint(x)
    // def replicate(i: Rep[Int], j: Rep[Int]) = densevector_repmat(x,i,j)
    // def toList = densevector_tolist(x)
    // def mkString(sep: Rep[String] = unit("")) = densevector_mkstring(x, sep)

    // data operations
    //def ++(y: Rep[DenseVector[A]]) = densevector_concatenate(x,y)
    def update(n: Rep[Int], y: Rep[A]) = densevector_update(elem,n,y)
    def +=(y: Rep[A]) = densevector_insert(elem,elem.length,y)
    //def ++=(y: Rep[DenseVector[A]]) = insertAll(length,y)
    def copyFrom(pos: Rep[Int], y: Rep[DenseVector[A]]) = densevector_copyfrom(elem,pos,y)
    def insert(pos: Rep[Int], y: Rep[A]) = densevector_insert(elem,pos,y)
    def insertAll(pos: Rep[Int], y: Rep[DenseVector[A]]) = densevector_insertall(elem,pos,y)
    //def remove(pos: Rep[Int]) = removeAll(pos,1)
    def removeAll(pos: Rep[Int], len: Rep[Int]) = densevector_removeall(elem,pos,len)
    def trim() = densevector_trim(elem)
    def clear() = densevector_clear(elem)

    // arithmetic operations
    // def +(y: Rep[V[A]])(implicit a: Arith[A]) = densevector_plus_dense(x,y)
    type VPLUSR = DenseVector[A]
    val mVPLUSR = manifest[VPLUSR]
    val vplusBuilder = builder[A]
    def vplusToIntf(x: Rep[VPLUSR]) = toIntf(x)
    // def +(y: Interface[Vector[A]])(implicit a: Arith[A]) = densevector_plus_generic(x,y)    
    // def +(y: Rep[DenseVector[A]])(implicit a: Arith[A]) = densevector_plus(x,y)
    // def +(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = densevector_plus_scalar(x,y)
    // def +=(y: Rep[DenseVector[A]])(implicit a: Arith[A]) = { densevector_plusequals(x,y); x }
    
    type VMINUSR = DenseVector[A]
    val mVMINUSR = manifest[VMINUSR]
    val vminusBuilder = builder[A]
    def vminusToIntf(x: Rep[VMINUSR]) = toIntf(x)    
    // def -(y: Rep[DenseVector[A]])(implicit a: Arith[A]) = densevector_minus(x,y)
    // def -(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = densevector_minus_scalar(x,y)
    // def -=(y: Rep[DenseVector[A]])(implicit a: Arith[A]) = { densevector_minusequals(x,y); x }
    
    type VTIMESR = DenseVector[A]
    val mVTIMESR = manifest[VTIMESR]
    val vtimesBuilder = builder[A]
    def vtimesToIntf(x: Rep[VTIMESR]) = toIntf(x)        
    //def *(y: Rep[DenseVector[A]])(implicit a: Arith[A]) = densevector_times(x,y)
    //def *[B](y: Rep[DenseVector[B]])(implicit mB: Manifest[B], a: Arith[A], conv: Rep[B] => Rep[A]) = densevector_times_withconvert(x,y,conv)
    //def *[B](y: Rep[DenseVector[B]])(implicit mB: Manifest[B], aB: Arith[B], conv: Rep[A] => Rep[B], o: Overloaded1) = densevector_times_withconvertright(x,y,conv)
    //def *(y: Rep[A])(implicit a: Arith[A],o: Overloaded1) = densevector_times_scalar(x,y)
    def *(y: Rep[Matrix[A]])(implicit a: Arith[A],o: Overloaded2) = densevector_times_matrix(elem,y)
    //def **(y: Rep[DenseVector[A]])(implicit a: Arith[A]) = densevector_outer(x,y)
    //def *:*(y: Rep[DenseVector[A]])(implicit a: Arith[A]) = {val v = x*y; v.sum} //TODO: this is less efficient (space-wise) than: //densevector_dot_product(x,y)
    //def dot(y: Rep[DenseVector[A]])(implicit a: Arith[A]) = x *:* y
    
    // def /(y: Rep[DenseVector[A]])(implicit a: Arith[A]) = densevector_divide(x,y)
    // def /(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = densevector_divide_scalar(x,y)
    // def /[B](y: Rep[B])(implicit a: Arith[A], conv: Rep[B] => Rep[A]) = densevector_divide_scalar(x,conv(y))
    
    // def sum(implicit a: Arith[A]) = densevector_sum(x)
    // def abs(implicit a: Arith[A]) = densevector_abs(x)
    // def exp(implicit a: Arith[A]) = densevector_exp(x)
    
    // ordering operations
    def sort(implicit o: Ordering[A]) = densevector_sort(elem)
    // def min(implicit o: Ordering[A], mx: HasMinMax[A]) = densevector_min(x)
    // def minIndex(implicit o: Ordering[A], mx: HasMinMax[A]) = densevector_minindex(x)
    // def max(implicit o: Ordering[A], mx: HasMinMax[A]) = densevector_max(x)
    // def maxIndex(implicit o: Ordering[A], mx: HasMinMax[A]) = densevector_maxindex(x)
    // def median(implicit o: Ordering[A]) = densevector_median(x)
    // def :>(y: Rep[DenseVector[A]])(implicit o: Ordering[A]) = zip(y) { (a,b) => a > b }
    // def :<(y: Rep[DenseVector[A]])(implicit o: Ordering[A]) = zip(y) { (a,b) => a < b }

    // bulk operations
    // def map[B:Manifest](f: Rep[A] => Rep[B]) = densevector_map(x,f)
    // def mmap(f: Rep[A] => Rep[A]) = { densevector_mmap(x,f); x }
    // def foreach(block: Rep[A] => Rep[Unit]) = densevector_foreach(x, block)
    // def zip[B:Manifest,R:Manifest](y: Rep[DenseVector[B]])(f: (Rep[A],Rep[B]) => Rep[R]) = densevector_zipwith(x,y,f)
    // def mzip[B:Manifest](y: Rep[DenseVector[B]])(f: (Rep[A],Rep[B]) => Rep[A]) = densevector_mzipwith(x,y,f)
    // def reduce(f: (Rep[A],Rep[A]) => Rep[A])(implicit a: Arith[A]) = densevector_reduce(x,f)
    // def filter(pred: Rep[A] => Rep[Boolean]) = densevector_filter(x,pred)
    
    //type VFINDR = DenseVector[Int]
    //val mVFINDR = manifest[VFINDR]
    //val vfindBuilder = builderB[Int]
    //def vfindToIntf(x: Rep[VFINDR]) = toIntfB[Int](x)
    // def find(pred: Rep[A] => Rep[Boolean]) = densevector_find(x,pred)
    
    // def count(pred: Rep[A] => Rep[Boolean]) = densevector_count(x, pred)
    def flatMap[B:Manifest](f: Rep[A] => Rep[DenseVector[B]]) = densevector_flatmap(elem,f)
    def partition(pred: Rep[A] => Rep[Boolean]) = densevector_partition(elem,pred)
    def groupBy[K:Manifest](pred: Rep[A] => Rep[K]) = densevector_groupby(elem,pred)            
  }
  
  def __equal[A](a: Rep[DenseVector[A]], b: Rep[DenseVector[A]])(implicit o: Overloaded1, mA: Manifest[A]): Rep[Boolean] = densevector_equals(a,b)
  def __equal[A](a: Rep[DenseVector[A]], b: Var[DenseVector[A]])(implicit o: Overloaded2, mA: Manifest[A]): Rep[Boolean] = densevector_equals(a,b)
  def __equal[A](a: Var[DenseVector[A]], b: Rep[DenseVector[A]])(implicit o: Overloaded3, mA: Manifest[A]): Rep[Boolean] = densevector_equals(a,b)
  def __equal[A](a: Var[DenseVector[A]], b: Var[DenseVector[A]])(implicit o: Overloaded4, mA: Manifest[A]): Rep[Boolean] = densevector_equals(a,b)
  

  // def densevector_plus_dense[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  // def densevector_plus_generic[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Interface[Vector[A]]): Rep[DenseVector[A]]

  // class defs
  def densevector_length[A:Manifest](x: Rep[DenseVector[A]]): Rep[Int]
  def densevector_isrow[A:Manifest](x: Rep[DenseVector[A]]): Rep[Boolean]
  def densevector_apply[A:Manifest](x: Rep[DenseVector[A]], n: Rep[Int]): Rep[A]
  def densevector_slice[A:Manifest](x: Rep[DenseVector[A]], start: Rep[Int], end: Rep[Int]): Rep[DenseVector[A]]
  def densevector_contains[A:Manifest](x: Rep[DenseVector[A]], y: Rep[A]): Rep[Boolean]
  def densevector_distinct[A:Manifest](x: Rep[DenseVector[A]]): Rep[DenseVector[A]]

  def densevector_equals[A:Manifest](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]]): Rep[Boolean]
  def densevector_trans[A:Manifest](x: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_mutable_trans[A:Manifest](x: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_clone[A:Manifest](x: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_mutable_clone[A:Manifest](x: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_pprint[A:Manifest](x: Rep[DenseVector[A]]): Rep[Unit]
  def densevector_repmat[A:Manifest](x: Rep[DenseVector[A]], i: Rep[Int], j: Rep[Int]): Rep[Matrix[A]]
  def densevector_tolist[A:Manifest](x: Rep[DenseVector[A]]): Rep[List[A]]
  def densevector_mkstring[A:Manifest](x: Rep[DenseVector[A]], sep: Rep[String]): Rep[String]

  def densevector_concatenate[A:Manifest](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_update[A:Manifest](x: Rep[DenseVector[A]], n: Rep[Int], y: Rep[A]): Rep[Unit]
  def densevector_copyfrom[A:Manifest](x: Rep[DenseVector[A]], pos: Rep[Int], y: Rep[DenseVector[A]]): Rep[Unit]
  def densevector_insert[A:Manifest](x: Rep[DenseVector[A]], pos: Rep[Int], y: Rep[A]): Rep[Unit]
  def densevector_insertall[A:Manifest](x: Rep[DenseVector[A]], pos: Rep[Int], y: Rep[DenseVector[A]]): Rep[Unit]
  def densevector_removeall[A:Manifest](x: Rep[DenseVector[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit]
  def densevector_trim[A:Manifest](x: Rep[DenseVector[A]]): Rep[Unit]
  def densevector_clear[A:Manifest](x: Rep[DenseVector[A]]): Rep[Unit]

  def densevector_plus[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_plus_scalar[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[A]): Rep[DenseVector[A]]
  def densevector_plusequals[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]]): Rep[Unit]
  def densevector_minus[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_minus_scalar[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[A]): Rep[DenseVector[A]]
  def densevector_minusequals[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]]): Rep[Unit]
  def densevector_times[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_times_withconvert[A:Manifest:Arith,B:Manifest](x: Rep[DenseVector[A]], y: Rep[DenseVector[B]],  conv: Rep[B] => Rep[A]): Rep[DenseVector[A]]
  def densevector_times_withconvertright[A:Manifest,B:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[B]], conv: Rep[A] => Rep[B]): Rep[DenseVector[B]]
  def densevector_times_scalar[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[A]): Rep[DenseVector[A]]
  def densevector_times_matrix[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[Matrix[A]]): Rep[DenseVector[A]]
  def densevector_outer[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]]): Rep[Matrix[A]]
  def densevector_dot_product[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]]): Rep[A]
  def densevector_divide[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_divide_scalar[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[A]): Rep[DenseVector[A]]
  def densevector_sum[A:Manifest:Arith](x: Rep[DenseVector[A]]): Rep[A]
  def densevector_abs[A:Manifest:Arith](x: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_exp[A:Manifest:Arith](x: Rep[DenseVector[A]]): Rep[DenseVector[A]]

  def densevector_sort[A:Manifest:Ordering](x: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_min[A:Manifest:Ordering:HasMinMax](x: Rep[DenseVector[A]]): Rep[A]
  def densevector_minindex[A:Manifest:Ordering:HasMinMax](x: Rep[DenseVector[A]]): Rep[Int]
  def densevector_max[A:Manifest:Ordering:HasMinMax](x: Rep[DenseVector[A]]): Rep[A]
  def densevector_maxindex[A:Manifest:Ordering:HasMinMax](x: Rep[DenseVector[A]]): Rep[Int]
  def densevector_median[A:Manifest:Ordering](x: Rep[DenseVector[A]]): Rep[A]

  def densevector_map[A:Manifest,B:Manifest](x: Rep[DenseVector[A]], f: Rep[A] => Rep[B]): Rep[DenseVector[B]]
  def densevector_mmap[A:Manifest](x: Rep[DenseVector[A]], f: Rep[A] => Rep[A]): Rep[Unit]
  def densevector_foreach[A:Manifest](x: Rep[DenseVector[A]], block: Rep[A] => Rep[Unit]): Rep[Unit]
  def densevector_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Rep[DenseVector[A]], y: Rep[DenseVector[B]], f: (Rep[A],Rep[B]) => Rep[R]): Rep[DenseVector[R]]
  def densevector_mzipwith[A:Manifest,B:Manifest](x: Rep[DenseVector[A]], y: Rep[DenseVector[B]], f: (Rep[A],Rep[B]) => Rep[A]): Rep[DenseVector[A]]
  def densevector_reduce[A:Manifest:Arith](x: Rep[DenseVector[A]], f: (Rep[A],Rep[A]) => Rep[A]): Rep[A]
  def densevector_filter[A:Manifest](x: Rep[DenseVector[A]], pred: Rep[A] => Rep[Boolean]): Rep[DenseVector[A]]
  def densevector_find[A:Manifest](x: Rep[DenseVector[A]], pred: Rep[A] => Rep[Boolean]): Rep[DenseVector[Int]]
  def densevector_count[A:Manifest](x: Rep[DenseVector[A]], pred: Rep[A] => Rep[Boolean]): Rep[Int]
  def densevector_flatmap[A:Manifest,B:Manifest](x: Rep[DenseVector[A]], f: Rep[A] => Rep[DenseVector[B]]): Rep[DenseVector[B]]
  def densevector_partition[A:Manifest](x: Rep[DenseVector[A]], pred: Rep[A] => Rep[Boolean]): (Rep[DenseVector[A]], Rep[DenseVector[A]])
  def densevector_groupby[A:Manifest,K:Manifest](x: Rep[DenseVector[A]], pred: Rep[A] => Rep[K]): Rep[DenseVector[DenseVector[A]]]         
}

trait DenseVectorOpsExp extends DenseVectorOps with VariablesExp with BaseFatExp {

  this: DenseVectorImplOps with OptiLAExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class DenseVectorApply[A:Manifest](x: Exp[DenseVector[A]], n: Exp[Int]) extends Def[A]
  case class DenseVectorLength[A:Manifest](x: Exp[DenseVector[A]]) extends Def[Int]
  case class DenseVectorIsRow[A:Manifest](x: Exp[DenseVector[A]]) extends Def[Boolean]
  case class DenseVectorUpdate[A:Manifest](x: Exp[DenseVector[A]], n: Exp[Int], y: Exp[A]) extends Def[Unit]
  case class DenseVectorCopyFrom[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[DenseVector[A]]) extends Def[Unit]
  case class DenseVectorInsert[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[A]) extends Def[Unit]
  case class DenseVectorInsertAll[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[DenseVector[A]]) extends Def[Unit]
  case class DenseVectorRemoveAll[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], len: Exp[Int]) extends Def[Unit]
  case class DenseVectorTrim[A:Manifest](x: Exp[DenseVector[A]]) extends Def[Unit]
  case class DenseVectorClear[A:Manifest](x: Exp[DenseVector[A]]) extends Def[Unit]
  case class DenseVectorMutableTrans[A:Manifest](x: Exp[DenseVector[A]]) extends Def[DenseVector[A]]
  case class DenseVectorClone[A:Manifest](x: Exp[DenseVector[A]]) extends Def[DenseVector[A]]
  // TODO: right now we just use the underlying data structure sort, but we should implement our own
  // fast parallel sort with delite ops
  case class DenseVectorSort[A:Manifest:Ordering](x: Exp[DenseVector[A]]) extends Def[DenseVector[A]]
  case class DenseVectorToList[A:Manifest](x: Exp[DenseVector[A]]) extends Def[List[A]]
  case class DenseVectorRawData[A:Manifest](x: Exp[DenseVector[A]]) extends Def[Array[A]]
  
  /////////////////////////////////////////////////
  // implemented via kernel embedding (sequential)
  
  case class DenseVectorConcatenate[A:Manifest](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_concatenate_impl(x,y)))
  
  case class DenseVectorSlice[A:Manifest](x: Exp[DenseVector[A]], start: Exp[Int], end: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_slice_impl(x,start,end)))

  case class DenseVectorTimesMatrix[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_times_matrix_impl[A](x,y)))

  case class DenseVectorOuter[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_outer_impl[A](x,y))) {
      //TODO: should mixin implicit accessors
      def m = manifest[A]
      def a = implicitly[Arith[A]]
    }

  // this is a single task right now because of the likely early exit. should we have a delite op for this?
  case class DenseVectorEquals[A:Manifest](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_equals_impl[A](x,y)))

  case class DenseVectorPPrint[A](x: Exp[DenseVector[A]])(block: Exp[Unit]) // stupid limitation...
    extends DeliteOpSingleTask(block)
    // reifyEffects(densevector_pprint_impl[A](x))

//  case class DenseVectorTrans[A:Manifest](x: Exp[DenseVector[A]])
//    extends DeliteOpSingleTask(reifyEffectsHere(densevector_trans_impl[A](x)))

  case class DenseVectorRepmat[A:Manifest](x: Exp[DenseVector[A]], i: Exp[Int], j: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_repmat_impl[A](x,i,j)))

  case class DenseVectorMkString[A:Manifest](x: Exp[DenseVector[A]], sep: Exp[String])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_mkstring_impl[A](x, sep)))

  case class DenseVectorMedian[A:Manifest:Ordering](x: Exp[DenseVector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_median_impl[A](x)))

//  case class DenseVectorFilter[A:Manifest](x: Exp[DenseVector[A]], pred: Exp[A] => Exp[Boolean])
//    extends DeliteOpSingleTask(reifyEffectsHere(densevector_filter_impl(x, pred)))

  case class DenseVectorPartition[A:Manifest](x: Exp[DenseVector[A]], pred: Exp[A] => Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_partition_impl(x, pred)))

  case class DenseVectorContains[A:Manifest](x: Exp[DenseVector[A]], y: Exp[A])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_contains_impl[A](x, y)))

  case class DenseVectorDistinct[A:Manifest](x: Exp[DenseVector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_distinct_impl[A](x)))

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

  // abstract class DenseVectorArithmeticZipWith[A:Manifest:Arith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]]) extends DeliteOpZipWith[A,A,A,DenseVector[A]] {
  //     def alloc = Vector.dense[A](inA.length, unit(true))
  //     val size = copyTransformedOrElse(_.size)(inA.length)
  //     
  //     def m = manifest[A]
  //     def a = implicitly[Arith[A]]
  //   }

  // abstract class DenseVectorArithmeticZipWith[A:Manifest:Arith](inA: Interface[Vector[A]], inB: Interface[Vector[A]])
  //   extends DeliteOpZipWith[A,A,A,DenseVector[A]] {
  // 
  //   def alloc = Vector.dense[A](inA.length, unit(true))
  //   val size = copyTransformedOrElse(_.size)(inA.length)
  // 
  //   def m = manifest[A]
  //   def a = implicitly[Arith[A]]
  // }
  
  // case class DenseVectorPlusDense[A:Manifest:Arith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]])
  //   extends DenseVectorArithmeticZipWith[A](inA, inB) {
  // 
  //   def func = (a,b) => a + b
  // }
  

  // case class DenseVectorPlusGeneric[A:Manifest:Arith](inA: Interface[Vector[A]], inB: Interface[Vector[A]])   
  //   //extends Def[DenseVector[A]]
  //   extends DenseVectorArithmeticZipWith[A](inA, inB) {
  // 
  //   def func = (a,b) => a + b
  // }

  
  abstract class DenseVectorArithmeticMap[A:Manifest:Arith](in: Exp[DenseVector[A]]) extends DeliteOpMap[A,A,DenseVector[A]] {
    def alloc = DenseVector[A](in.length, in.isRow)
    val size = copyTransformedOrElse(_.size)(in.length)

    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  abstract class DenseVectorArithmeticZipWith[A:Manifest:Arith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]]) extends DeliteOpZipWith[A,A,A,DenseVector[A]] {
    def alloc = DenseVector[A](inA.length, inA.isRow)
    val size = copyTransformedOrElse(_.size)(inA.length)

    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  abstract class DenseVectorArithmeticIndexedLoop[A:Manifest:Arith](in: Exp[DenseVector[A]]) extends DeliteOpIndexedLoop {
    val size = copyTransformedOrElse(_.size)(in.length)

    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  abstract class DenseVectorArithmeticReduce[A:Manifest:Arith](in: Exp[DenseVector[A]]) extends DeliteOpReduce[A] {
    val size = copyTransformedOrElse(_.size)(in.length)

    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }



  case class DenseVectorTrans[A:Manifest](in: Exp[DenseVector[A]])
    extends DeliteOpMap[A,A,DenseVector[A]] {
    val size = copyTransformedOrElse(_.size)(in.length)

    def alloc = DenseVector[A](in.length, !in.isRow)
    def func = e => e 

    def m = manifest[A]
  }

  case class DenseVectorPlus[A:Manifest:Arith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]])
    extends DenseVectorArithmeticZipWith[A](inA, inB) {

    def func = (a,b) => a + b
  }

  case class DenseVectorPlusScalar[A:Manifest:Arith](in: Exp[DenseVector[A]], y: Exp[A])
    extends DenseVectorArithmeticMap[A](in) {

    def func = e => e + y
  }

  case class DenseVectorPlusEquals[A:Manifest:Arith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]])
    extends DenseVectorArithmeticIndexedLoop(inA) { 

    def func = i => { inA(i) = inA(i) + inB(i) } 
  } 

  case class DenseVectorMinus[A:Manifest:Arith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]])
    extends DenseVectorArithmeticZipWith[A](inA, inB) {

    def func = (a,b) => a - b
  }

  case class DenseVectorMinusScalar[A:Manifest:Arith](in: Exp[DenseVector[A]], y: Exp[A])
    extends DenseVectorArithmeticMap[A](in) {

    def func = e => e - y
  }

  case class DenseVectorMinusEquals[A:Manifest:Arith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]])
    extends DenseVectorArithmeticIndexedLoop(inA) { 

    def func = i => { inA(i) = inA(i) - inB(i) } 
  }

  case class DenseVectorTimes[A:Manifest:Arith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]])
    extends DenseVectorArithmeticZipWith[A](inA, inB) {

    def func = (a,b) => a * b
  }

  case class DenseVectorTimesWithConvert[A:Manifest:Arith,B:Manifest](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[B]], conv: Exp[B] => Exp[A])
    extends DeliteOpZipWith[A,B,A,DenseVector[A]] {

    def alloc = DenseVector[A](inA.length, inA.isRow)
    def func = (a,b) => a * conv(b)
    val size = inA.length
  }

  case class DenseVectorTimesWithConvertRight[A:Manifest,B:Manifest:Arith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[B]], conv: Exp[A] => Exp[B])
    extends DeliteOpZipWith[A,B,B,DenseVector[B]] {

    def alloc = DenseVector[B](inB.length, inB.isRow)    
    def func = (a,b) => conv(a) * b
    val size = inA.length
  }

  case class DenseVectorTimesScalar[A:Manifest:Arith](in: Exp[DenseVector[A]], y: Exp[A])
    extends DenseVectorArithmeticMap[A](in) {

    def func = e => e * y
  }

  case class DenseVectorDotProduct[A:Manifest:Arith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]])
    extends DeliteOpZipWithReduce[A,A,A] {

    def zip = (a,b) => a*b
    def reduce = (a,b) => a + b
    val size = inA.length
    val zero = implicitly[Arith[A]].empty
  }


  case class DenseVectorDivide[A:Manifest:Arith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]])
    extends DenseVectorArithmeticZipWith[A](inA, inB) {

    def func = (a,b) => a / b
  }

  case class DenseVectorDivideScalar[A:Manifest:Arith](in: Exp[DenseVector[A]], y: Exp[A])
    extends DenseVectorArithmeticMap[A](in) {

    def func = e => e / y
  }

  case class DenseVectorSum[A:Manifest:Arith](in: Exp[DenseVector[A]]) 
    extends DenseVectorArithmeticReduce[A](in) {

    val zero = a.empty 
    def func = (a,b) => a + b
  }

  case class DenseVectorAbs[A:Manifest:Arith](in: Exp[DenseVector[A]])
    extends DenseVectorArithmeticMap[A](in) {

    def func = e => e.abs
  }

  case class DenseVectorExp[A:Manifest:Arith](in: Exp[DenseVector[A]])
    extends DenseVectorArithmeticMap[A](in) {

    def func = e => e.exp
  }

  case class DenseVectorMin[A:Manifest:Ordering:HasMinMax](in: Exp[DenseVector[A]]) 
    extends DeliteOpReduce[A] {

    val size = in.length
    val zero = implicitly[HasMinMax[A]].maxValue
    def func = (a,b) => if (a < b) a else b
  }

  case class DenseVectorMax[A:Manifest:Ordering:HasMinMax](in: Exp[DenseVector[A]]) 
    extends DeliteOpReduce[A] {

    val size = in.length
    val zero = implicitly[HasMinMax[A]].minValue
    def func = (a,b) => if (a > b) a else b
  }

  case class DenseVectorMinIndex[A:Manifest:Ordering:HasMinMax](inB: Exp[DenseVector[A]]) 
    extends DeliteOpZipWithReduceTuple[Int,A,Int,A] {

    val inA = copyTransformedOrElse(_.inA)(0::inB.length)
    val size = copyTransformedOrElse(_.size)(inB.length)
    val zero = (copyTransformedOrElse(_.zero._1)(unit(0)),copyTransformedOrElse(_.zero._2)(implicitly[HasMinMax[A]].maxValue)) // 0 sensible? maybe -1?
    def zip = (a,b) => (a,b)
    def reduce = (a,b) => (if (a._2 < b._2) a._1 else b._1, if (a._2 < b._2) a._2 else b._2)

    val m = manifest[A]
    val o = implicitly[Ordering[A]]
    val p = implicitly[HasMinMax[A]]
  }

/*
  // TODO: need to get rid of tuple allocations for performance (if we're lucky HotSpot's scalar replacement does it for us)
  case class DenseVectorMinIndex[A:Manifest:Ordering:HasMinMax](inB: Exp[DenseVector[A]]) 
    extends DeliteOpZipWithReduce[Int,A,(Int,A)] {

    val inA = copyTransformedOrElse(_.inA)(0::inB.length)
    val size = copyTransformedOrElse(_.size)(inB.length)
    val zero = copyTransformedOrElse(_.zero)(make_tuple2(unit(0),implicitly[HasMinMax[A]].maxValue)) // 0 sensible? maybe -1?
    def zip = (a,b) => (a,b)
    def reduce = (a,b) => if (a._2 > b._2) a else b

    val m = manifest[A]
    val o = implicitly[Ordering[A]]
    val p = implicitly[HasMinMax[A]]
  }
*/
  case class DenseVectorMaxIndex[A:Manifest:Ordering:HasMinMax](inB: Exp[DenseVector[A]]) 
    extends DeliteOpZipWithReduce[Int,A,(Int,A)] {

    val inA = copyTransformedOrElse(_.inA)(0::inB.length)
    val size = copyTransformedOrElse(_.size)(inB.length)
    val zero = copyTransformedOrElse(_.zero)(make_tuple2(unit(0),implicitly[HasMinMax[A]].minValue)) // 0 sensible? maybe -1?
    def zip = (a,b) => (a,b)
    def reduce = (a,b) => if (a._2 > b._2) a else b

    val m = manifest[A]
    val o = implicitly[Ordering[A]]
    val p = implicitly[HasMinMax[A]]
  }

  case class DenseVectorMap[A:Manifest,B:Manifest](in: Exp[DenseVector[A]], func: Exp[A] => Exp[B])
    extends DeliteOpMap[A,B,DenseVector[B]] {

    val size = copyTransformedOrElse(_.size)(in.length)
    def alloc = DenseVector[B](in.length, in.isRow)

    val mA = manifest[A]
    val mB = manifest[B]
  }

  case class DenseVectorMutableMap[A:Manifest](in: Exp[DenseVector[A]], block: Exp[A] => Exp[A])
    extends DeliteOpIndexedLoop {

    val size = copyTransformedOrElse(_.size)(in.length)
    def func = i => in(i) = block(in(i))

    val m = manifest[A]
  }

  case class DenseVectorForeach[A:Manifest](in: Exp[DenseVector[A]], func: Exp[A] => Exp[Unit])
    extends DeliteOpForeach[A] {

    def sync = n => List()
    val size = copyTransformedOrElse(_.size)(in.length)
  }

  case class DenseVectorZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[B]],
                                                             func: (Exp[A], Exp[B]) => Exp[R])
    extends DeliteOpZipWith[A,B,R,DenseVector[R]] {

    def alloc = DenseVector[R](inA.length, inA.isRow)
    val size = inA.length
  }

  case class DenseVectorMutableZipWith[A:Manifest,B:Manifest](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[B]],
                                                         func: (Exp[A], Exp[B]) => Exp[A])
    extends DeliteOpZipWith[A,B,A,DenseVector[A]] {

    def alloc = inA
    val size = inA.length
  }

  // note: we may want to factor 'HasEmpty' out of 'Arith' to make this more general, if the need arises.
  case class DenseVectorReduce[A:Manifest:Arith](in: Exp[DenseVector[A]], func: (Exp[A], Exp[A]) => Exp[A])
    extends DeliteOpReduce[A] {

    val size = in.length
    val zero = implicitly[Arith[A]].empty
  }

  // TODO: this is an inefficient way to compute flatten (allocates a new buffer for every intermediate output)
  // should use a scan that allocates the output once (precumulate)
  // case class DenseVectorObjectFlatten[A:Manifest](in: Exp[DenseVector[DenseVector[A]]])
  //     extends DeliteOpReduce[DenseVector[A]] {
  // 
  //     val size = in.length
  //     val zero = EmptyVector[A]
  //     def func = (a,b) => a ++ b    
  //   } 

  case class DenseVectorFlatMap[A:Manifest,B:Manifest](in: Exp[DenseVector[A]], map: Exp[A] => Exp[DenseVector[B]])
    extends DeliteOpMapReduce[A,DenseVector[B]] {

    val size = in.length
    val zero = EmptyVector[B]
    def reduce = (a,b) => a ++ b
  }

  case class DenseVectorFilter[A:Manifest](in: Exp[DenseVector[A]], cond: Exp[A] => Exp[Boolean]) 
    extends DeliteOpFilter[A,A,DenseVector[A]] {

    def alloc = DenseVector[A](0, in.isRow)
    def func = e => e 
    val size = in.length

    def m = manifest[A]  
  }

  case class DenseVectorFind[A:Manifest](in: Exp[DenseVector[A]], cond: Exp[A] => Exp[Boolean])
    extends DeliteOpFilter[A,Int,DenseVector[Int]] {

    def alloc = DenseVector[Int](0,true)
    def func = e => v // should we make available and use a helper function like index(e)?
    val size = in.length

    def m = manifest[A]  
  }

  case class DenseVectorCount[A:Manifest](in: Exp[DenseVector[A]], cond: Exp[A] => Exp[Boolean]) 
    extends DeliteOpFilterReduce[A,Int] {

    val size = in.length
    val zero = unit(0)
    def func = e => unit(1)
    def reduce = (a,b) => a + b   

    def m = manifest[A]
  }

  /////////////////////
  // class interface

  // def densevector_plus_dense[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = reflectPure(DenseVectorPlusGeneric(denseToInterface(x),denseToInterface(y)))//reflectPure(DenseVectorPlusDense(x,y))
  // def densevector_plus_generic[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Interface[Vector[A]]) = reflectPure(DenseVectorPlusGeneric(x,y))
  
  def densevector_length[A:Manifest](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorLength(x))
  def densevector_isrow[A:Manifest](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorIsRow(x))
  def densevector_apply[A:Manifest](x: Exp[DenseVector[A]], n: Exp[Int]) = reflectPure(DenseVectorApply(x, n))
  def densevector_slice[A:Manifest](x: Exp[DenseVector[A]], start: Exp[Int], end: Exp[Int]) = reflectPure(DenseVectorSlice(x, start, end))
  def densevector_contains[A:Manifest](x: Exp[DenseVector[A]], y: Exp[A]) = reflectPure(DenseVectorContains(x, y))
  def densevector_distinct[A:Manifest](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorDistinct(x))
  def densevector_raw_data[A:Manifest](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorRawData(x))

  def densevector_equals[A:Manifest](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = reflectPure(DenseVectorEquals(x,y))
  def densevector_trans[A:Manifest](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorTrans(x))
  def densevector_mutable_trans[A:Manifest](x: Exp[DenseVector[A]]) = reflectWrite(x)(DenseVectorMutableTrans(x))
  def densevector_clone[A:Manifest](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorClone(x))
  def densevector_mutable_clone[A:Manifest](x: Exp[DenseVector[A]]) = reflectMutable(DenseVectorClone(x))
  def densevector_repmat[A:Manifest](x: Exp[DenseVector[A]], i: Exp[Int], j: Exp[Int]) = reflectPure(DenseVectorRepmat(x,i,j))
  def densevector_tolist[A:Manifest](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorToList(x))
  def densevector_mkstring[A:Manifest](x: Exp[DenseVector[A]], sep: Exp[String]) = reflectPure(DenseVectorMkString(x, sep))
  def densevector_pprint[A:Manifest](x: Exp[DenseVector[A]]) = reflectEffect(DenseVectorPPrint(x)(reifyEffectsHere(densevector_pprint_impl[A](x))))

  def densevector_concatenate[A:Manifest](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = reflectPure(DenseVectorConcatenate(x,y))
  def densevector_update[A:Manifest](x: Exp[DenseVector[A]], n: Exp[Int], y: Exp[A]) = reflectWrite(x)(DenseVectorUpdate(x, n, y))
  def densevector_copyfrom[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[DenseVector[A]]) = reflectWrite(x)(DenseVectorCopyFrom(x, pos, y))
  def densevector_insert[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[A]) = reflectWrite(x)(DenseVectorInsert(x, pos, y))
  def densevector_insertall[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], y: Exp[DenseVector[A]]) = reflectWrite(x)(DenseVectorInsertAll(x, pos, y))
  def densevector_removeall[A:Manifest](x: Exp[DenseVector[A]], pos: Exp[Int], len: Exp[Int]) = reflectWrite(x)(DenseVectorRemoveAll(x, pos, len))
  def densevector_trim[A:Manifest](x: Exp[DenseVector[A]]) = reflectWrite(x)(DenseVectorTrim(x))
  def densevector_clear[A:Manifest](x: Exp[DenseVector[A]]) = reflectWrite(x)(DenseVectorClear(x))

  def densevector_plus[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = reflectPure(DenseVectorPlus(x,y))
  def densevector_plus_scalar[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[A]) = reflectPure(DenseVectorPlusScalar(x,y))
  def densevector_plusequals[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = reflectWrite(x)(DenseVectorPlusEquals(x,y))
  def densevector_minus[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = reflectPure(DenseVectorMinus(x,y))
  def densevector_minus_scalar[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[A]) = reflectPure(DenseVectorMinusScalar(x,y))
  def densevector_minusequals[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = reflectWrite(x)(DenseVectorMinusEquals(x, y))
  def densevector_times[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = reflectPure(DenseVectorTimes(x,y))
  def densevector_times_withconvert[A:Manifest:Arith,B:Manifest](x: Exp[DenseVector[A]], y: Exp[DenseVector[B]], conv: Exp[B] => Exp[A]) = reflectPure(DenseVectorTimesWithConvert(x,y,conv)) // TODO: de-hoas
  def densevector_times_withconvertright[A:Manifest,B:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[B]], conv: Exp[A] => Exp[B]) = reflectPure(DenseVectorTimesWithConvertRight(x,y,conv))
  def densevector_times_scalar[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[A]) = reflectPure(DenseVectorTimesScalar(x,y))
  def densevector_times_matrix[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[Matrix[A]]) = reflectPure(DenseVectorTimesMatrix(x,y))
  def densevector_outer[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = reflectPure(DenseVectorOuter(x,y))
  def densevector_dot_product[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = reflectPure(DenseVectorDotProduct(x,y))
  def densevector_divide[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = reflectPure(DenseVectorDivide(x,y))
  def densevector_divide_scalar[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[A]) = reflectPure(DenseVectorDivideScalar(x,y))
  def densevector_sum[A:Manifest:Arith](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorSum(x))
  def densevector_abs[A:Manifest:Arith](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorAbs(x))
  def densevector_exp[A:Manifest:Arith](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorExp(x))

  def densevector_sort[A:Manifest:Ordering](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorSort(x))
  def densevector_min[A:Manifest:Ordering:HasMinMax](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorMin(x))
  def densevector_minindex[A:Manifest:Ordering:HasMinMax](x: Exp[DenseVector[A]]) = /*tuple2_get1*/(reflectPure(DenseVectorMinIndex(x)))
  def densevector_max[A:Manifest:Ordering:HasMinMax](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorMax(x))
  def densevector_maxindex[A:Manifest:Ordering:HasMinMax](x: Exp[DenseVector[A]]) = tuple2_get1(reflectPure(DenseVectorMaxIndex(x)))
  def densevector_median[A:Manifest:Ordering](x: Exp[DenseVector[A]]) = reflectPure(DenseVectorMedian(x))

  def densevector_map[A:Manifest,B:Manifest](x: Exp[DenseVector[A]], f: Exp[A] => Exp[B]) = reflectPure(DenseVectorMap(x, f)) // TODO: effect if func effectful!
  def densevector_mmap[A:Manifest](x: Exp[DenseVector[A]], f: Exp[A] => Exp[A]) = reflectWrite(x)(DenseVectorMutableMap(x, f)) // TODO: effect if func effectful!
  def densevector_foreach[A:Manifest](x: Exp[DenseVector[A]], block: Exp[A] => Exp[Unit]) = {
    reflectEffect(DenseVectorForeach(x, block))
  }
  def densevector_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Exp[DenseVector[A]], y: Exp[DenseVector[B]], f: (Exp[A],Exp[B]) => Exp[R]) = {
    reflectPure(DenseVectorZipWith(x, y, f))
  }
  def densevector_mzipwith[A:Manifest,B:Manifest](x: Exp[DenseVector[A]], y: Exp[DenseVector[B]], f: (Exp[A],Exp[B]) => Exp[A]) = {
    reflectWrite(x)(DenseVectorMutableZipWith(x, y, f))
  }
  def densevector_reduce[A:Manifest:Arith](x: Exp[DenseVector[A]], f: (Exp[A],Exp[A]) => Exp[A]) = reflectPure(DenseVectorReduce(x, f))
  def densevector_filter[A:Manifest](x: Exp[DenseVector[A]], pred: Exp[A] => Exp[Boolean]) = reflectPure(DenseVectorFilter(x, pred))
  def densevector_find[A:Manifest](x: Exp[DenseVector[A]], pred: Exp[A] => Exp[Boolean]) = reflectPure(DenseVectorFind(x, pred))
  def densevector_count[A:Manifest](x: Exp[DenseVector[A]], pred: Exp[A] => Exp[Boolean]) = reflectPure(DenseVectorCount(x, pred))
  def densevector_flatmap[A:Manifest,B:Manifest](x: Exp[DenseVector[A]], f: Exp[A] => Exp[DenseVector[B]]) = reflectPure(DenseVectorFlatMap(x, f))
  def densevector_partition[A:Manifest](x: Exp[DenseVector[A]], pred: Exp[A] => Exp[Boolean]) = t2(reflectPure(DenseVectorPartition(x, pred)))
  def densevector_groupby[A:Manifest,K:Manifest](x: Exp[DenseVector[A]], pred: Exp[A] => Exp[K]) = reflectPure(DenseVectorGroupBy(x,pred))
  
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case DenseVectorApply(x, n) => densevector_apply(f(x), f(n))
    case DenseVectorLength(x) => densevector_length(f(x))
    case DenseVectorIsRow(x) => densevector_isrow(f(x))
    // implemented as DeliteOpSingleTask and DeliteOpLoop
    case e@DenseVectorTrans(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorTrans(f(x))(e.m))(mtype(manifest[A]))
    case e@DenseVectorOuter(x,y) => reflectPure(new { override val original = Some(f,e) } with DenseVectorOuter(f(x),f(y))(e.m, e.a))(mtype(manifest[A]))
    case e@DenseVectorPlus(x,y) => reflectPure(new { override val original = Some(f,e) } with DenseVectorPlus(f(x),f(y))(e.m, e.a))(mtype(manifest[A]))
    case e@DenseVectorMinus(x,y) => reflectPure(new { override val original = Some(f,e) } with DenseVectorMinus(f(x),f(y))(e.m, e.a))(mtype(manifest[A]))
    case e@DenseVectorTimes(x,y) => reflectPure(new { override val original = Some(f,e) } with DenseVectorTimes(f(x),f(y))(e.m, e.a))(mtype(manifest[A]))
    case e@DenseVectorTimesScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with DenseVectorTimesScalar(f(x),f(y))(e.m, e.a))(mtype(manifest[A]))
    case e@DenseVectorDivideScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with DenseVectorDivideScalar(f(x),f(y))(e.m, e.a))(mtype(manifest[A]))
    case e@DenseVectorSum(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorSum(f(x))(e.m, e.a))(mtype(manifest[A]))
    case e@DenseVectorAbs(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorAbs(f(x))(e.m, e.a))(mtype(manifest[A]))
    case e@DenseVectorExp(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorExp(f(x))(e.m, e.a))(mtype(manifest[A]))
    case e@DenseVectorFilter(x,p) => reflectPure(new { override val original = Some(f,e) } with DenseVectorFilter(f(x),f(p))(e.m))(mtype(manifest[A]))
    case e@DenseVectorFind(x,p) => reflectPure(new { override val original = Some(f,e) } with DenseVectorFind(f(x),f(p))(e.m))(mtype(manifest[A]))
    case e@DenseVectorCount(x,p) => reflectPure(new { override val original = Some(f,e) } with DenseVectorCount(f(x),f(p))(e.m))(mtype(manifest[A]))
    case e@DenseVectorMinIndex(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorMinIndex(f(x))(e.m,e.o,e.p))(mtype(manifest[A]))
    case e@DenseVectorMap(x,p) => reflectPure(new { override val original = Some(f,e) } with DenseVectorMap(f(x),f(p))(e.mA,e.mB))(mtype(manifest[A]))
    // read/write effects
    case Reflect(DenseVectorApply(l,r), u, es) => reflectMirrored(Reflect(DenseVectorApply(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(DenseVectorLength(x), u, es) => reflectMirrored(Reflect(DenseVectorLength(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(DenseVectorIsRow(x), u, es) => reflectMirrored(Reflect(DenseVectorIsRow(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(DenseVectorUpdate(l,i,r), u, es) => reflectMirrored(Reflect(DenseVectorUpdate(f(l),f(i),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(DenseVectorInsert(l,i,r), u, es) => reflectMirrored(Reflect(DenseVectorInsert(f(l),f(i),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    // implemented as DeliteOpSingleTask and DeliteOpLoop
    case Reflect(e@DenseVectorTimesScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorTimesScalar(f(x),f(y))(e.m, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorDivideScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorDivideScalar(f(x),f(y))(e.m, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorPlus(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorPlus(f(x),f(y))(e.m, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorMinus(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorMinus(f(x),f(y))(e.m, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorForeach(a,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorForeach(f(a),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorPlusEquals(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorPlusEquals(f(x),f(y))(e.m, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorMutableMap(x,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorMutableMap(f(x),f(g))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorPPrint(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DenseVectorPPrint(f(x))(f(e.block)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    // allocations
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
    case DenseVectorRepmat(a,i,j) => Nil
    case DenseVectorClone(a) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case DenseVectorApply(a,i) => Nil
    case DenseVectorUpdate(a,i,x) => syms(x)
    case DenseVectorInsert(a,i,x) => syms(x)
    case DenseVectorInsertAll(a,i,x) => Nil
    case DenseVectorRepmat(a,i,j) => Nil
    case DenseVectorClone(a) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case DenseVectorApply(a,i) => syms(a)
    case DenseVectorUpdate(a,i,x) => Nil
    case DenseVectorInsert(a,i,x) => Nil
    case DenseVectorInsertAll(a,i,x) => Nil
    case DenseVectorRepmat(a,i,j) => Nil
    case DenseVectorClone(a) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case DenseVectorApply(a,i) => Nil
    case DenseVectorUpdate(a,i,x) => syms(a)
    case DenseVectorInsert(a,i,x) => syms(a)
    case DenseVectorInsertAll(a,i,x) => syms(a) ++ syms(x)
    case DenseVectorRepmat(a,i,j) => syms(a)
    case DenseVectorClone(a) => syms(a)
    case _ => super.copySyms(e)
  }  
}

/**
 * Optimizations for composite DenseVectorOps operations.
 */

// have to extend DeliteCollectionOps to override dc_apply...
trait DenseVectorOpsExpOpt extends DenseVectorOpsExp with DeliteCollectionOpsExp {
  this: DenseVectorImplOps with OptiLAExp =>

  override def densevector_equals[A:Manifest](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = (x, y) match {
    case (a,b) if (a == b) => unit(true) // same symbol
    case _ => super.densevector_equals(x,y)
  }

  override def densevector_plus[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = (x, y) match {
    // (TB + TD) == T(B + D)
    case (Def(DenseVectorTimes(a, b)), Def(DenseVectorTimes(c, d))) if (a == c) => densevector_times[A](a.asInstanceOf[Exp[DenseVector[A]]], densevector_plus[A](b.asInstanceOf[Exp[DenseVector[A]]],d.asInstanceOf[Exp[DenseVector[A]]]))
    // ...
    case _ => super.densevector_plus(x, y)
  }

  override def densevector_plusequals[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = (x, y) match {
    // remove runtime check on zero densevector being same length as argument
    case (a, Def(DenseVectorObjectZeros(len))) => ()
    //case (Def(DenseVectorObjectZeros(len)), b) => b  // this is unsafe because we lose the effectful operation (e.g. accumulation)
    case _ => super.densevector_plusequals(x,y)
  }

  override def densevector_times[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) = (x, y) match {
    case _ => super.densevector_times(x, y)
  }

  override def densevector_mutable_clone[A:Manifest](x: Exp[DenseVector[A]]) = x match {
    // these are unsafe in general.. we can only short-circuit the clone if we know the allocation is dead
    // except for the .mutable call
    // e.g., val x = DenseVector(10, true)
    //       val y = x.mutable // should clone!
    //       val z = x + 5
    // val x = DenseVector(10, true).mutable // should not clone!
    case Def(d@DenseVectorNew(len, isRow)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]])
    case Def(d@DenseVectorObjectFromSeq(xs)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]])   
    case Def(d@DenseVectorObjectZeros(len)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]])
    case Def(d@DenseVectorObjectZerosF(len)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]])
    //case Def(d@DenseVectorObjectOnes(len)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]]) <--- actually a problem in testSumIf!
    //case Def(d@DenseVectorObjectOnesF(len)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]])
    case Def(d@DenseVectorObjectRand(len)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]])
    case Def(d@DenseVectorObjectRandF(len)) => reflectMutable(d.asInstanceOf[Def[DenseVector[A]]])
    case _ => super.densevector_mutable_clone(x)
  }

  override def densevector_length[A:Manifest](x: Exp[DenseVector[A]]) = x match {
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
    case Def(DenseVectorClone(a)) => densevector_length(a)
    //case Def(DenseVectorObjectRange(s,e,d,r)) => (e - s + d - 1) / d

    /* propagate output size information */
    // some of this could be handled in DeliteCollectionOps, but we need a way to link length (for single tasks)
    // and size (for data parallel tasks) together. DenseVector can override dc_size, but has to deal with erasure.    
    case Def(e: DeliteOpMap[_,_,_]) => e.size
    case Def(e: DeliteOpZipWith[_,_,_,_]) => e.size
    //case Def(Reflect(e: DeliteOpMap[_,_,_], _,_)) => e.size // reasonable? mutable things may change size...
    //case Def(Reflect(e: DeliteOpZipWith[_,_,_,_], _,_)) => e.size // reasonable?
    //    case Def(e: DeliteOpDenseVectorLoop[A]) => e.size
    
    case Def(DenseVectorSlice(a, start, end)) => end - start
    case Def(DenseVectorMutableTrans(a)) => a.length
    case Def(DenseVectorConcatenate(a,b)) => a.length + b.length   
    case Def(DenseVectorSort(a)) => a.length
      
    case _ => 
      //printerr("could not short-circuit call to " + x.toString + ".length")
      //printerr(findDefinition(x.asInstanceOf[Sym[DenseVector[A]]]))
      super.densevector_length(x)
  }

  override def densevector_isrow[A:Manifest](x: Exp[DenseVector[A]]) = x match {
    case Def(e: DenseVectorArithmeticMap[A]) => e.in.asInstanceOf[Exp[DenseVector[A]]].isRow 
    case Def(e: DenseVectorArithmeticZipWith[A]) => e.inA.asInstanceOf[Exp[DenseVector[A]]].isRow 
    //case Def(e: DeliteOpDenseVectorLoop[A]) => e.isRow
    //case Def(e: DenseVectorDeliteOp[A] => e.isRow)
    //case Def(Reflect(DenseVectorObjectZeros(l,r), _)) => r
    case Def(DenseVectorClone(a)) => densevector_isrow(a)
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
  def densevector_optimize_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]): Option[Exp[A]] = x match {
    case Def(DenseVectorObjectZeros(l)) => Some(unit(0).asInstanceOf[Exp[A]])
    case Def(DenseVectorObjectOnes(l)) => Some(unit(1).asInstanceOf[Exp[A]])
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
  
  override def densevector_apply[A:Manifest](x: Exp[DenseVector[A]], n: Exp[Int]) = {
    densevector_optimize_apply(x.asInstanceOf[Exp[DeliteCollection[A]]],n) getOrElse super.densevector_apply(x,n)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]) = {
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
    case DenseVectorApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
    case DenseVectorUpdate(x,n,y) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))
    case DenseVectorLength(x)    => emitValDef(sym, quote(x) + ".length")
    case DenseVectorIsRow(x)     => emitValDef(sym, quote(x) + ".isRow")
    case DenseVectorMutableTrans(x) => emitValDef(sym, quote(x) + ".mtrans")
    case DenseVectorSort(x) => emitValDef(sym, quote(x) + ".sort")
    case DenseVectorToList(x) => emitValDef(sym, quote(x) + ".toList")
    case DenseVectorCopyFrom(x,pos,y) => emitValDef(sym, quote(x) + ".copyFrom(" + quote(pos) + ", " + quote(y) + ")")
    case DenseVectorInsert(x,pos,y) => emitValDef(sym, quote(x) + ".insert(" + quote(pos) + ", " + quote(y) + ")")
    case DenseVectorInsertAll(x,pos,y) => emitValDef(sym, quote(x) + ".insertAll(" + quote(pos) + ", " + quote(y) + ")")
    case DenseVectorRemoveAll(x,pos,len) => emitValDef(sym, quote(x) + ".removeAll(" + quote(pos) + ", " + quote(len) + ")")
    case DenseVectorTrim(x) => emitValDef(sym, quote(x) + ".trim")
    case DenseVectorClear(x) => emitValDef(sym, quote(x) + ".clear()")
    case DenseVectorClone(x) => emitValDef(sym, quote(x) + ".cloneL")
    case DenseVectorRawData(x) => emitValDef(sym, quote(getBlockResult(x)) + ".data")      
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenDenseVectorOps extends BaseGenDenseVectorOps with CudaGenFat with CudaGenDataStruct {
  val IR: DenseVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

    case DenseVectorApply(x, n) =>
      emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case DenseVectorUpdate(x,n,y) =>
      stream.println(addTab() + "%s.update(%s,%s);".format(quote(x),quote(n),quote(y)))
    case DenseVectorLength(x)    =>
      emitValDef(sym, quote(x) + ".length")
    case DenseVectorIsRow(x)     =>
      emitValDef(sym, quote(x) + ".isRow")

	//case DenseVectorPlusEquals(x,y) =>
	//	throw new GenerationFailedException("CudaGen: No dimension specified for this kernel.")

    case DenseVectorRepmat(x,i,j) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->length * %s * %s".format(quote(x),quote(i),quote(j)))
      stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(sym)))
      tabWidth += 1
      stream.println(addTab()+"int i = %s / (%s.length * %s);".format(currDimStr,quote(x),quote(j)))
      stream.println(addTab()+"int j = " + currDimStr + " % " + "(%s.length * %s);".format(quote(x),quote(j)))
      stream.println(addTab()+"%s.update(i,j,%s.apply(%s));".format(quote(sym),quote(x),"j%"+quote(x)+".length"))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s".format(quote(i)),"%s.length*%s".format(quote(x),quote(j)),false)
      currDim -= 1

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

