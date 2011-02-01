package ppl.dsl.optiml.vector

import ppl.dsl.optiml.datastruct.CudaGenDataStruct
import ppl.dsl.optiml.datastruct.scala._
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.Manifest
import scala.virtualization.lms.common.{CudaGenFat, ScalaGenFat, CGenBase, CudaGenBase, ScalaGenBase}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import ppl.dsl.optiml.{OptiMLExp, OptiML}

trait VectorOps extends DSLType with Variables {
  this: OptiML =>

  object Vector {
    def apply[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]) = vector_obj_new(len, isRow)
    // this works, but generates painfully inefficient code because the Const(List(..)) symbol gets inlined each
    // time it is used, rather than stored as a value, because consts are not treated like dependencies in 'syms'.
    def apply[A:Manifest](xs: A*) = {
      // Seq gets lifted into a WrappedArray Const, which can't be instantiated from generated code
      val xs2 = unit(xs.toList)
      vector_obj_fromseq(xs2)
    }
    // this doesn't work because if we don't lift the Seq, we can't generate code for it
    // if we do lift the Seq, we have a Rep[Seq[Rep[A]], which has the problems discussed below
    //def apply[A:Manifest](xs: Rep[A]*) = {}

    // this is problematic.. should this be Rep[Vector[Rep[Vector[A]]] or Rep[Vector[Vector[A]]]?
    // we have this issue for all containers; with the current implementation only the latter makes sense, but how
    // is it ever instantiated? Vector(Vector(1,2,3)) will return a Rep[Vector[Rep[Vector[Int]]]
    // a Rep[Vector[Rep[Vector[Int]]]'s apply method would return a Rep[Rep[Vector[Int]]], which is obviously not what we want
    // one option is to make containers always contain Reps explicitly; but this is a bit uglier and
    // then we need a way of instantiating a manifest for Rep[A]
    def flatten[A:Manifest](pieces: Rep[Vector[Vector[A]]]) = vector_obj_flatten(pieces)

    def ones(len: Rep[Int]) = vector_obj_ones(len)
    def onesf(len: Rep[Int]) = vector_obj_onesf(len)
    def zeros(len: Rep[Int]) = vector_obj_zeros(len)
    def mzeros(len: Rep[Int]) = vector_obj_mzeros(len)
    def zerosf(len: Rep[Int]) = vector_obj_zerosf(len)
    def rand(len: Rep[Int]) = vector_obj_rand(len)
    def randf(len: Rep[Int]) = vector_obj_randf(len)
    def range(start: Rep[Int], end: Rep[Int], stride: Rep[Int] = 1, isRow: Rep[Boolean] = true) =
      vector_obj_range(start, end, stride, isRow)
    def uniform(start: Rep[Double], step_size: Rep[Double], end: Rep[Double], isRow: Rep[Boolean] = true) =
      vector_obj_uniform(start, step_size, end, isRow)
  }

  implicit def repVecToVecOps[A:Manifest](x: Rep[Vector[A]]) = new vecOpsCls(x)
  //implicit def vecToVecOps[A:Manifest](x: Vector[A]) = new vecOpsCls(x)
  implicit def varToVecOps[A:Manifest](x: Var[Vector[A]]) = new vecOpsCls(readVar(x))

  /**
   * This class defines the public interface for the Vector[T] class.
   * (could convert to infix, but apply doesn't work with it anyways yet)
   */
  class vecOpsCls[A:Manifest](x: Rep[Vector[A]]) {
    // TODO: how to make this work? the implicit won't kick in
    // override def toString = ..

    // TODO: scaladocify -- this is the only place, but is it the right place?

    // conversions
    def toBoolean(implicit conv: Rep[A] => Rep[Boolean]) =  map(e => conv(e))
    def toDouble(implicit conv: Rep[A] => Rep[Double]) =  map(e => conv(e))
    def toFloat(implicit conv: Rep[A] => Rep[Float]) = map(e => conv(e))
    def toInt(implicit conv: Rep[A] => Rep[Int]) = map(e => conv(e))
    def toLong(implicit conv: Rep[A] => Rep[Long]) = map(e => conv(e))

    // accessors
    def length = vector_length(x)
    def isRow = vector_isRow(x)
    def apply(n: Rep[Int]) = vector_apply(x, n)
    def isEmpty = length == 0
    def first = apply(0)
    def last = apply(repArithToArithOps(length) - 1) // TODO: why doesn't this get invoked implicitly?
    def indices = (0::length)
    def drop(count: Rep[Int]) = slice(count, length)
    def take(count: Rep[Int]) = slice(0, count)
    def slice(start: Rep[Int], end: Rep[Int]) = vector_slice(x, start, end)
    def contains(y: Rep[A]) = vector_contains(x,y)
    def distinct = vector_distinct(x)

    // general
    def t = vector_trans(x)
    def mt() = vector_mutable_trans(x)
    def cloneL() = vector_clone(x)
    def pprint() = vector_pprint(x)
    def replicate(i: Rep[Int], j: Rep[Int]) = vector_repmat(x,i,j)
    def toList = vector_tolist(x)

    // data operations
    def ++(y: Rep[Vector[A]]) = vector_concatenate(x,y)
    def update(n: Rep[Int], y: Rep[A]) = vector_update(x,n,y)
    def +=(y: Rep[A]) = vector_insert(x,x.length,y)
    def ++=(y: Rep[Vector[A]]) = insertAll(length,y)
    def copyFrom(pos: Rep[Int], y: Rep[Vector[A]]) = vector_copyfrom(x,pos,y)
    def insert(pos: Rep[Int], y: Rep[A]) = vector_insert(x,pos,y)
    def insertAll(pos: Rep[Int], y: Rep[Vector[A]]) = vector_insertall(x,pos,y)
    def remove(pos: Rep[Int]) = removeAll(pos,1)
    def removeAll(pos: Rep[Int], len: Rep[Int]) = vector_removeall(x,pos,len)
    def trim() = vector_trim(x)
    def clear() = vector_clear(x)

    // arithmetic operations
    def +(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_plus(x,y)
    def +(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = vector_plus_scalar(x,y)
    def +=(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_plusequals(x,y)
    def -(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_minus(x,y)
    def -(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = vector_minus_scalar(x,y)
    def -=(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_minusequals(x,y)
    def *(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_times(x,y)
    def *[B](y: Rep[Vector[B]])(implicit mB: Manifest[B], a: Arith[A], conv: Rep[B] => Rep[A]) = vector_times_withconvert(x,y,conv)
    //def *[B](y: Rep[Vector[B]])(implicit mB: Manifest[B], aB: Arith[B], conv: Rep[A] => Rep[B], o: Overloaded1) = vector_times_withconvertright(x,y,conv)
    def *(y: Rep[A])(implicit a: Arith[A],o: Overloaded1) = vector_times_scalar(x,y)
    def *(y: Rep[Matrix[A]])(implicit a: Arith[A],o: Overloaded2) = vector_times_matrix(x,y)
    def **(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_outer(x,y)
    def *:*(y: Rep[Vector[A]])(implicit a: Arith[A]) = {val v = x*y; v.sum} //TODO: this is less efficient (space-wise) than: //vector_dot_product(x,y)
    def dot(y: Rep[Vector[A]])(implicit a: Arith[A]) = x *:* y
    def /(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_divide(x,y)
    def /(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = vector_divide_scalar(x,y)
    def /[B](y: Rep[B])(implicit a: Arith[A], conv: Rep[B] => Rep[A]) = vector_divide_scalar(x,conv(y))
    def sum(implicit a: Arith[A]) = vector_sum(x)
    def abs(implicit a: Arith[A]) = vector_abs(x)
    def exp(implicit a: Arith[A]) = vector_exp(x)

    // ordering operations
    def sort(implicit o: Ordering[A]) = vector_sort(x)
    def min(implicit o: Ordering[A]) = vector_min(x)
    //def minIndex(implicit o: Ordering[A]) = vector_minindex(x)
    def max(implicit o: Ordering[A]) = vector_max(x)
    def max[B](key: Rep[A] => Rep[B])(implicit o: Ordering[B], mB: Manifest[B]) = vector_max_key(x, key)
    //def maxIndex(implicit o: Ordering[A]) = vector_maxIndex(x)
    def median(implicit o: Ordering[A]) = vector_median(x)
    def :>(y: Rep[Vector[A]])(implicit o: Ordering[A]) = zip(y) { (a,b) => a > b }
    def :<(y: Rep[Vector[A]])(implicit o: Ordering[A]) = zip(y) { (a,b) => a < b }

    // bulk operations
    def map[B:Manifest](f: Rep[A] => Rep[B]) = vector_map(x,f)
    def mmap(f: Rep[A] => Rep[A]) = vector_mmap(x,f)
    def foreach(block: Rep[A] => Rep[Unit]) = vector_foreach(x, block)
    def zip[B:Manifest,R:Manifest](y: Rep[Vector[B]])(f: (Rep[A],Rep[B]) => Rep[R]) = vector_zipwith(x,y,f)
    def reduce(f: (Rep[A],Rep[A]) => Rep[A]) = vector_reduce(x,f)
    def filter(pred: Rep[A] => Rep[Boolean]) = vector_filter(x,pred)
    def flatMap[B:Manifest](f: Rep[A] => Rep[Vector[B]]) = vector_flatmap(x,f)
    def partition(pred: Rep[A] => Rep[Boolean]) = vector_partition(x,pred)
  }
  
/*
  class vecOpsClsMutable[A:Manifest](vx: Var[Vector[A]]) extends vecOpsCls[A](readVar(vx)) {
    // ...
  }
*/  



  def NilV[A:Manifest] = vector_nil

  // object defs
  def vector_obj_new[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]): Rep[Vector[A]]
  def vector_obj_fromseq[A:Manifest](xs: Rep[Seq[A]]): Rep[Vector[A]]
  def vector_obj_ones(len: Rep[Int]): Rep[Vector[Double]]
  def vector_obj_onesf(len: Rep[Int]): Rep[Vector[Float]]
  def vector_obj_zeros(len: Rep[Int]): Rep[Vector[Double]]
  def vector_obj_mzeros(len: Rep[Int]): Rep[Vector[Double]]
  def vector_obj_zerosf(len: Rep[Int]): Rep[Vector[Float]]
  def vector_obj_rand(len: Rep[Int]): Rep[Vector[Double]]
  def vector_obj_randf(len: Rep[Int]): Rep[Vector[Float]]
  def vector_obj_range(start: Rep[Int], end: Rep[Int], stride: Rep[Int], isRow: Rep[Boolean]): Rep[RangeVector]
  def vector_obj_uniform(start: Rep[Double], step_size: Rep[Double], end: Rep[Double], isRow: Rep[Boolean]): Rep[Vector[Double]]
  def vector_obj_flatten[A:Manifest](pieces: Rep[Vector[Vector[A]]]): Rep[Vector[A]]

  // class defs
  def vector_length[A:Manifest](x: Rep[Vector[A]]): Rep[Int]
  def vector_isRow[A:Manifest](x: Rep[Vector[A]]): Rep[Boolean]
  def vector_apply[A:Manifest](x: Rep[Vector[A]], n: Rep[Int]): Rep[A]
  def vector_slice[A:Manifest](x: Rep[Vector[A]], start: Rep[Int], end: Rep[Int]): Rep[Vector[A]]
  def vector_contains[A:Manifest](x: Rep[Vector[A]], y: Rep[A]): Rep[Boolean]
  def vector_distinct[A:Manifest](x: Rep[Vector[A]]): Rep[Vector[A]]

  def vector_trans[A:Manifest](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_mutable_trans[A:Manifest](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_clone[A:Manifest](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_pprint[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
  def vector_repmat[A:Manifest](x: Rep[Vector[A]], i: Rep[Int], j: Rep[Int]): Rep[Matrix[A]]
  def vector_tolist[A:Manifest](x: Rep[Vector[A]]): Rep[List[A]]

  def vector_concatenate[A:Manifest](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_update[A:Manifest](x: Rep[Vector[A]], n: Rep[Int], y: Rep[A]): Rep[Unit]
  def vector_copyfrom[A:Manifest](x: Rep[Vector[A]], pos: Rep[Int], y: Rep[Vector[A]]): Rep[Unit]
  def vector_insert[A:Manifest](x: Rep[Vector[A]], pos: Rep[Int], y: Rep[A]): Rep[Unit]
  def vector_insertall[A:Manifest](x: Rep[Vector[A]], pos: Rep[Int], y: Rep[Vector[A]]): Rep[Unit]
  def vector_removeall[A:Manifest](x: Rep[Vector[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit]
  def vector_trim[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
  def vector_clear[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]

  def vector_plus[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_plus_scalar[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vector_plusequals[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_minus[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_minus_scalar[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vector_minusequals[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_times[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_times_withconvert[A:Manifest:Arith,B:Manifest](x: Rep[Vector[A]], y: Rep[Vector[B]],  conv: Rep[B] => Rep[A]): Rep[Vector[A]]
  def vector_times_withconvertright[A:Manifest,B:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[B]], conv: Rep[A] => Rep[B]): Rep[Vector[B]]
  def vector_times_scalar[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vector_times_matrix[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Matrix[A]]): Rep[Vector[A]]
  def vector_outer[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Matrix[A]]
  def vector_dot_product[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[A]
  def vector_divide[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_divide_scalar[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vector_sum[A:Manifest:Arith](x: Rep[Vector[A]]): Rep[A]
  def vector_abs[A:Manifest:Arith](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_exp[A:Manifest:Arith](x: Rep[Vector[A]]): Rep[Vector[A]]

  def vector_sort[A:Manifest:Ordering](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_min[A:Manifest:Ordering](x: Rep[Vector[A]]): Rep[A]
  //def vector_minIndex[A:Manifest:Ordering](x: Rep[Vector[A]]): Rep[Int]
  def vector_max[A:Manifest:Ordering](x: Rep[Vector[A]]): Rep[A]
  def vector_max_key[A:Manifest,B:Manifest:Ordering](x: Rep[Vector[A]], key: Rep[A] => Rep[B]): Rep[A]
  //def vector_maxIndex[A:Manifest:Ordering](x: Rep[Vector[A]]): Rep[Int]
  def vector_median[A:Manifest:Ordering](x: Rep[Vector[A]]): Rep[A]

  def vector_map[A:Manifest,B:Manifest](x: Rep[Vector[A]], f: Rep[A] => Rep[B]): Rep[Vector[B]]
  def vector_mmap[A:Manifest](x: Rep[Vector[A]], f: Rep[A] => Rep[A]): Rep[Vector[A]]
  def vector_foreach[A:Manifest](x: Rep[Vector[A]], block: Rep[A] => Rep[Unit]): Rep[Unit]
  def vector_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Rep[Vector[A]], y: Rep[Vector[B]], f: (Rep[A],Rep[B]) => Rep[R]): Rep[Vector[R]]
  def vector_reduce[A:Manifest](x: Rep[Vector[A]], f: (Rep[A],Rep[A]) => Rep[A]): Rep[A]
  def vector_filter[A:Manifest](x: Rep[Vector[A]], pred: Rep[A] => Rep[Boolean]): Rep[Vector[A]]
  def vector_flatmap[A:Manifest,B:Manifest](x: Rep[Vector[A]], f: Rep[A] => Rep[Vector[B]]): Rep[Vector[B]]
  def vector_partition[A:Manifest](x: Rep[Vector[A]], pred: Rep[A] => Rep[Boolean]): (Rep[Vector[A]], Rep[Vector[A]])

  // other defs
  def vector_nil[A:Manifest] : Rep[Vector[A]]
}

trait CleanRoom {
  var c = 0

  def foobar(x: Any): Unit = {
    Predef.println("readVar: " + x)
    if (c > 10) throw new Exception
    c += 1
  }
}


trait VectorOpsExp extends VectorOps with VariablesExp with BaseFatExp with CleanRoom {

  this: VectorImplOps with OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class VectorObjectRange(start: Exp[Int], end: Exp[Int], stride: Exp[Int], isRow: Exp[Boolean])
    extends Def[RangeVector]

  case class VectorObjectNew[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) //TR GRAPH

  case class VectorNew[A](len: Exp[Int], isRow: Exp[Boolean])(val mV: Manifest[VectorImpl[A]]) extends Def[Vector[A]]
/*
  case class VectorNew[A:Manifest](len: Exp[Int], isRow: Exp[Boolean])
    extends Def[Vector[A]] {
    val mV = manifest[VectorImpl[A]]
  }
*/
  case class VectorNil[A](implicit mA: Manifest[A]) extends Def[Vector[A]]
  case class VectorApply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) extends Def[A]
  case class VectorLength[A:Manifest](x: Exp[Vector[A]]) extends Def[Int]
  case class VectorIsRow[A:Manifest](x: Exp[Vector[A]]) extends Def[Boolean]
  case class VectorUpdate[A:Manifest](x: Exp[Vector[A]], n: Exp[Int], y: Exp[A]) extends Def[Unit]
  case class VectorCopyFrom[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[Vector[A]]) extends Def[Unit]
  case class VectorInsert[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[A]) extends Def[Unit]
  case class VectorInsertAll[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[Vector[A]]) extends Def[Unit]
  case class VectorRemoveAll[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], len: Exp[Int]) extends Def[Unit]
  case class VectorTrim[A:Manifest](x: Exp[Vector[A]]) extends Def[Unit]
  case class VectorClear[A:Manifest](x: Exp[Vector[A]]) extends Def[Unit]
  case class VectorMutableTrans[A:Manifest](x: Exp[Vector[A]]) extends Def[Vector[A]]
  case class VectorClone[A:Manifest](x: Exp[Vector[A]]) extends Def[Vector[A]]
  // TODO: right now we just use the underlying data structure sort, but we should implement our own fast parallel sort
  // with delite ops
  case class VectorSort[A:Manifest:Ordering](x: Exp[Vector[A]]) extends Def[Vector[A]]


  /////////////////////////////////////////////////
  // implemented via kernel embedding (sequential)

  case class VectorObjectFromSeq[A:Manifest](xs: Exp[Seq[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_obj_fromseq_impl(xs)))

  case class VectorObjectOnes(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_obj_ones_impl(len)))

  case class VectorObjectOnesF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_obj_onesf_impl(len)))

  case class VectorObjectZeros(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_obj_zeros_impl(len)))

  case class VectorObjectZerosF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_obj_zerosf_impl(len)))

  case class VectorObjectRand(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_obj_rand_impl(len)))

  case class VectorObjectRandF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_obj_randf_impl(len)))

  case class VectorObjectUniform(start: Exp[Double], step_size: Exp[Double], end: Exp[Double], isRow: Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_obj_uniform_impl(start, step_size, end, isRow)))

//  case class VectorObjectFlatten[A:Manifest](pieces: Exp[Vector[Vector[A]]])
//    extends DeliteOpSingleTask(reifyEffectsHere(vector_obj_flatten_impl(pieces)))

  case class VectorConcatenate[A:Manifest](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_concatenate_impl(x,y)))
  
  case class VectorSlice[A:Manifest](x: Exp[Vector[A]], start: Exp[Int], end: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_slice_impl(x,start,end)))

  case class VectorTimesMatrix[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_times_matrix_impl[A](x,y)))

  case class VectorOuter[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_outer_impl[A](x,y)))

  case class VectorPPrint[A](x: Exp[Vector[A]])(block: Exp[Unit]) // stupid limitation...
    extends DeliteOpSingleTask(block)
    // reifyEffects(vector_pprint_impl[A](x))

/*
  case class VectorTrans[A:Manifest](x: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_trans_impl[A](x)))
*/

  case class VectorRepmat[A:Manifest](x: Exp[Vector[A]], i: Exp[Int], j: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_repmat_impl[A](x,i,j)))

  case class VectorToList[A:Manifest](x: Exp[Vector[A]]) extends Def[List[A]]

  case class VectorMedian[A:Manifest:Ordering](x: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_median_impl[A](x)))

  case class VectorFilter[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_filter_impl(x, pred)))

  case class VectorPartition[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_partition_impl(x, pred)))

  case class VectorContains[A:Manifest](x: Exp[Vector[A]], y: Exp[A])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_contains_impl[A](x, y)))

  case class VectorDistinct[A:Manifest](x: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_distinct_impl[A](x)))



  ////////////////////////////////
  // implemented via delite ops

/*
  case class VectorTrans[A:Manifest](in: Exp[Vector[A]])
    extends DeliteOpMap[A,A,Vector] {
    
    val alloc = reifyEffects(Vector[A](in.length, !in.isRow))
    val v = fresh[A]
    val func = v 
  }

  case class VectorPlus[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector] {

    val alloc = reifyEffects(Vector[A](inA.length, inA.isRow))
    val v = (fresh[A],fresh[A])
    val func = v._1 + v._2
  }
*/

  abstract class DeliteOpVectorLoop[A] extends DeliteOpLoop[Vector[A]] {
    val size: Exp[Int] //inherited
    val isRow: Exp[Boolean] //inherited
  }
  
  case class VectorTrans[A:Manifest](in: Exp[Vector[A]])
    extends DeliteOpVectorLoop[A] {

    val size = in.length
    val isRow = !in.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = DeliteCollectElem[A,Vector](
      alloc = reifyEffects(Vector[A](size, isRow)),
      func = reifyEffects(in(v))
    )
  }

  case class VectorPlus[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]]) 
    extends DeliteOpVectorLoop[A] {

    val size = inA.length
    val isRow = inA.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = DeliteCollectElem[A,Vector](
      alloc = reifyEffects(Vector[A](size, isRow)),
      func = reifyEffects(inA(v) + inB(v))
    )
  }

  //TR TODO
  case class VectorPlusScalar[A:Manifest:Arith](in: Exp[Vector[A]], y: Exp[A])
    extends DeliteOpMap[A,A,Vector] {

    val alloc = reifyEffects(Vector[A](in.length, in.isRow))
    val v = fresh[A]
    val func = reifyEffects(v + y)
  }

  case class VectorPlusEquals[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector] { // stick to ZipWith for the moment TODO: try multi loop

    val alloc = inA
    val v = (fresh[A],fresh[A])
    val func = reifyEffects(v._1 + v._2)
  }

  case class VectorMinus[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpVectorLoop[A] {

    val size = inA.length
    val isRow = inA.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = DeliteCollectElem[A,Vector](
      alloc = reifyEffects(Vector[A](size, isRow)),
      func = reifyEffects(inA(v) - inB(v))
    )
  }

  //TR TODO
  case class VectorMinusScalar[A:Manifest:Arith](in: Exp[Vector[A]], y: Exp[A])
    extends DeliteOpMap[A,A,Vector] {

    val alloc = reifyEffects(Vector[A](in.length, in.isRow))
    val v = fresh[A]
    val func = reifyEffects(v - y)
  }
  
  case class VectorMinusEquals[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector] {

    val alloc = inA
    val v = (fresh[A],fresh[A])
    val func = v._1 - v._2
  }

  abstract case class VectorTimes[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]]) extends DeliteOpVectorLoop[A] {
    def mev = manifest[A]
    def aev = implicitly[Arith[A]]
  }
  
  class VectorTimesFresh[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]]) extends VectorTimes(inA, inB) {
    val size = inA.length
    val isRow = inA.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = DeliteCollectElem[A,Vector](
      alloc = reifyEffects(Vector[A](size, isRow)),
      func = reifyEffects(inA(v) * inB(v))
    )
  }

  //TR TODO ?
  case class VectorTimesWithConvert[A:Manifest:Arith,B:Manifest](inA: Exp[Vector[A]], inB: Exp[Vector[B]], conv: Exp[B] => Exp[A])
    extends DeliteOpZipWith[A,B,A,Vector] {

    val alloc = reifyEffects(Vector[A](inA.length, inA.isRow))
    val v = (fresh[A],fresh[B])
    val func = reifyEffects(v._1 * conv(v._2))
  }

  case class VectorTimesWithConvertRight[A:Manifest,B:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[B]], conv: Exp[A] => Exp[B])
    extends DeliteOpZipWith[A,B,B,Vector] {

    val alloc = reifyEffects(Vector[B](inB.length, inB.isRow))
    val v = (fresh[A],fresh[B])
    val func = conv(v._1) * v._2
  }

  abstract case class VectorTimesScalar[A:Manifest:Arith](in: Exp[Vector[A]], y: Exp[A]) extends DeliteOpVectorLoop[A] {
    def mev = manifest[A]
    def aev = implicitly[Arith[A]]
  }
  class VectorTimesScalarFresh[A:Manifest:Arith](in: Exp[Vector[A]], y: Exp[A]) extends VectorTimesScalar[A](in,y) {
    val size = in.length
    val isRow = in.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = DeliteCollectElem[A,Vector](
      alloc = reifyEffects(Vector[A](size, isRow)),
      func = reifyEffects(in(v) * y)
    )
  }

  //TR TODO
  case class VectorDotProduct[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWithReduce[A,A,A,Vector] {

    val zV = (fresh[A],fresh[A])
    val zip = reifyEffects(zV._1 * zV._2)
    val rV = (fresh[A],fresh[A])
    val reduce = reifyEffects(rV._1 += rV._2)
  }

  case class VectorDivide[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpVectorLoop[A] {

    val size = inA.length
    val isRow = inA.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = new DeliteCollectElem[A,Vector](
      alloc = reifyEffects(Vector[A](size, isRow)),
      func = inA(v) / inB(v)
    )
  }

  case class VectorDivideScalar[A:Manifest:Arith](in: Exp[Vector[A]], y: Exp[A])
    extends DeliteOpVectorLoop[A] {

    val size = in.length
    val isRow = in.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = new DeliteCollectElem[A,Vector](
      alloc = reifyEffects(Vector[A](size, isRow)),
      func = in(v) / y
    )
  }

  abstract case class VectorSum[A:Manifest:Arith](in: Exp[Vector[A]]) extends DeliteOpLoop[A] {
    def mev = manifest[A]
    def aev = implicitly[Arith[A]]
  }


  class VectorSumFresh[A:Manifest:Arith](in: Exp[Vector[A]]) extends VectorSum[A](in) {

    val size = in.length
    val v = fresh[Int]
    private[this] val rV = (fresh[A],fresh[A])
    val body: Def[A] = DeliteReduceElem[A]( //TODO might need explicit zero?
      func = reifyEffects(in(v)),
      rV = rV,
      rFunc = reifyEffects(rV._1 + rV._2)
    )
  }

  //TR TODO
  case class VectorAbs[A:Manifest:Arith](in: Exp[Vector[A]])
    extends DeliteOpMap[A,A,Vector] {

    val alloc = reifyEffects(Vector[A](in.length, in.isRow))
    val v = fresh[A]
    val func = reifyEffects(v.abs)
  }

  //TR TODO
  case class VectorExp[A:Manifest:Arith](in: Exp[Vector[A]])
    extends DeliteOpMap[A,A,Vector] {

    val alloc = reifyEffects(Vector[A](in.length, in.isRow))
    val v = fresh[A]
    val func = reifyEffects(v.exp)
  }

  //TR TODO
  case class VectorMin[A:Manifest:Ordering](in: Exp[Vector[A]])
    extends DeliteOpReduce[A] {

    val v = (fresh[A],fresh[A])
    val func = reifyEffects(if (v._1 < v._2) v._1 else v._2)
  }

  // TODO: can't yet express this with DeliteOpReduce
  //case class VectorMinIndex[A:Manifest:Ordering](in: Exp[Vector[A]])
  //  extends DeliteOpReduce[A] {
  //
  //  val v = (fresh[A],fresh[A])
  //  val func = if (v._1 < v._2) index(v._1) else index(v._2)
  //}

  //TR TODO
  case class VectorMax[A:Manifest:Ordering](in: Exp[Vector[A]])
    extends DeliteOpReduce[A] {

    val v = (fresh[A],fresh[A])
    val func = reifyEffects(if (v._1 > v._2) v._1 else v._2)
  }

  case class VectorMaxKey[A:Manifest,B:Manifest:Ordering](in: Exp[Vector[A]], key: Exp[A] => Exp[B])
    extends DeliteOpReduce[A] {

    val v = (fresh[A],fresh[A])
    val func = if (key(v._1) > key(v._2)) v._1 else v._2
  }

  //case class VectorMaxIndex[A:Manifest:Ordering](in: Exp[Vector[A]])
  //  extends DeliteOpReduce[A] {
  //
  //  val v = (fresh[A],fresh[A])
  //  val func = if (v._1 > v._2) v._1.index else v._2.index
  //}

  case class VectorMap[A:Manifest,B:Manifest](in: Exp[Vector[A]], v: Sym[A], func: Exp[B])
    extends DeliteOpMap[A,B,Vector] {

    val alloc = reifyEffects(Vector[B](in.length, in.isRow))
  }

  case class VectorMutableMap[A:Manifest](in: Exp[Vector[A]], v: Sym[A], func: Exp[A])
    extends DeliteOpMap[A,A,Vector] {

    val alloc = in
  }

  case class VectorForeach[A:Manifest](in: Exp[Vector[A]], v: Sym[A], func: Exp[Unit])
    extends DeliteOpForeach[A,Vector] {

    val i = fresh[Int]
    val sync = reifyEffects(List())
    //val sync = reifyEffects(if ((i > 0) && (i < in.length)) List(in(i-1),in(i),in(i+1)) else List(in(i)))
  }


  case class VectorZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[Vector[A]], inB: Exp[Vector[B]],
                                                             v: (Sym[A],Sym[B]), func: Exp[R])
    extends DeliteOpZipWith[A,B,R,Vector] {

    val alloc = reifyEffects(Vector[R](inA.length, inA.isRow))
  }

  case class VectorReduce[A:Manifest](in: Exp[Vector[A]], v: (Sym[A],Sym[A]), func: Exp[A])
    extends DeliteOpReduce[A]


  // TODO: this is an inefficient way to compute flatten (allocates a new buffer for every intermediate output)
  // should use a scan that allocates the output once (precumulate)
  case class VectorObjectFlatten[A:Manifest](in: Exp[Vector[Vector[A]]])
    extends DeliteOpReduce[Vector[A]] {

    val v = (fresh[Vector[A]], fresh[Vector[A]])
    val func = reifyEffects(v._1 ++ v._2)
  }

  case class VectorFlatMap[A:Manifest,B:Manifest](in: Exp[Vector[A]], mV: Sym[A], map: Exp[Vector[B]])
    extends DeliteOpMapReduce[A,Vector[B],Vector] {

    val rV = (fresh[Vector[B]],fresh[Vector[B]])
    val reduce = reifyEffects(rV._1 ++ rV._2)
  }


  //////////////
  // mirroring

  override def mirrorFatDef[A:Manifest](d: Def[A], f: Transformer): Def[A] = mirrorLoopBody(d,f) // TODO: cleanup

  def mirrorLoopBody[A](d: Def[A], f: Transformer): Def[A] = {
    d match {
      case e: DeliteCollectElem[a,Vector] => 
      DeliteCollectElem[a,Vector]( // need to be a case class for equality!
        alloc = f(e.alloc),
        func = f(e.func)
      ).asInstanceOf[Def[A]]
      case e: DeliteReduceElem[a] => 
      DeliteReduceElem[a](
        func = f(e.func),
        rV = (f(e.rV._1).asInstanceOf[Sym[a]], f(e.rV._2).asInstanceOf[Sym[a]]), // need to transform bound vars ??
        rFunc = f(e.rFunc)
      ).asInstanceOf[Def[A]]
    }
  }
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case VectorApply(x, n) => vector_apply(f(x), f(n))
    case VectorLength(x) => vector_length(f(x))
    case VectorIsRow(x) => vector_isRow(f(x))
    // FIXME: VectorSum might not actually be triggered because
    case e@VectorSum(x) => toAtom(new VectorSum(f(x))(e.mev,e.aev) { val size = f(e.size); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody[A](e.body.asInstanceOf[Def[A]], f) })
    case e@VectorTimes(x,y) => toAtom(new VectorTimes(f(x),f(y))(e.mev,e.aev) { val size = f(e.size); val isRow = f(e.isRow); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case e@VectorTimesScalar(x,y) => toAtom(new VectorTimesScalar(f(x),f(y))(e.mev,e.aev) { val size = f(e.size); val isRow = f(e.isRow); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case Reflect(e@VectorPPrint(x), Global(), es) => reflectMirrored(Reflect(VectorPPrint(f(x))(f(e.block)), Global(), f(es)))
    // below are read/write effects TODO: find a general approach to treating them!!!!
    case Reflect(VectorApply(l,r), Read(rs), es) => reflectMirrored(Reflect(VectorApply(f(l),f(r)), Read(f onlySyms rs), f(es)))
    case Reflect(VectorLength(x), Read(rs), es) => reflectMirrored(Reflect(VectorLength(f(x)), Read(f onlySyms rs), f(es)))
    case Reflect(VectorIsRow(x), Read(rs), es) => reflectMirrored(Reflect(VectorIsRow(f(x)), Read(f onlySyms rs), f(es)))
    case Reflect(VectorForeach(a,b,c), Read(rs), es) => reflectMirrored(Reflect(VectorForeach(f(a),f(b).asInstanceOf[Sym[Int]],f(c)), Read(f onlySyms rs), f(es)))
    // FIXME: problem with VectorTimes: it's actually a loop and if it is reflected it means a.length will also reflect and we have no context here!!!
    case Reflect(e2@VectorTimes(a,b), Read(rs), es) => error("we'd rather not mirror " + e); //reflectMirrored(Reflect(VectorTimes(f(a),f(b))(e.mev,e.aev), Read(f onlySyms rs), f(es)))
    case Reflect(VectorUpdate(l,i,r), Write(ws), es) => reflectMirrored(Reflect(VectorUpdate(f(l),f(i),f(r)), Write(f onlySyms ws), f(es)))
    // allocations TODO: generalize
    case Reflect(VectorObjectZeros(x), Alloc(), es) => reflectMirrored(Reflect(VectorObjectZeros(f(x)), Alloc(), f(es)))
    case Reflect(VectorObjectRange(s,e,d,r), Alloc(), es) => reflectMirrored(Reflect(VectorObjectRange(f(s),f(e),f(d),f(r)), Alloc(), f(es)))
    case Reflect(e@VectorNew(l,r), Alloc(), es) => reflectMirrored(Reflect(VectorNew(f(l),f(r))(e.mV), Alloc(), f(es)))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??


  /////////////////////
  // object interface

//  def vector_obj_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) = reflectEffect(VectorObjectNew[A](len, isRow))
  def vector_obj_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) = reflectMutable(VectorNew[A](len, isRow)(manifest[VectorImpl[A]])) //XXX
  def vector_obj_fromseq[A:Manifest](xs: Exp[Seq[A]]) = reflectMutable(VectorObjectFromSeq(xs)) //XXX
  def vector_obj_ones(len: Exp[Int]) = reflectNew()(VectorObjectOnes(len))
  def vector_obj_onesf(len: Exp[Int]) = reflectNew()(VectorObjectOnesF(len))
  def vector_obj_zeros(len: Exp[Int]) = reflectNew()(VectorObjectZeros(len))
  def vector_obj_mzeros(len: Exp[Int]) = reflectMutable(VectorObjectZeros(len))
  def vector_obj_zerosf(len: Exp[Int]) = reflectNew()(VectorObjectZerosF(len))
  def vector_obj_rand(len: Exp[Int]) = reflectNew()(VectorObjectRand(len))
  def vector_obj_randf(len: Exp[Int]) = reflectNew()(VectorObjectRandF(len))
  def vector_obj_range(start: Exp[Int], end: Exp[Int], stride: Exp[Int], isRow: Exp[Boolean]) = reflectNew()(VectorObjectRange(start, end, stride, isRow))
  def vector_obj_uniform(start: Exp[Double], step_size: Exp[Double], end: Exp[Double], isRow: Exp[Boolean]) = reflectNew()(VectorObjectUniform(start, step_size, end, isRow))
  def vector_obj_flatten[A:Manifest](pieces: Exp[Vector[Vector[A]]]) = reflectNew(pieces)(VectorObjectFlatten(pieces))


  /////////////////////
  // class interface

/* FROM GRAPH-PORT-BROKEN....
  def vector_length[A:Manifest](x: Exp[Vector[A]]) = VectorLength(reflectRead(x))
  def vector_isRow[A:Manifest](x: Exp[Vector[A]]) = VectorIsRow(reflectRead(x))
  def vector_apply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) = VectorApply(reflectRead(x), n)
  def vector_slice[A:Manifest](x: Exp[Vector[A]], start: Exp[Int], end: Exp[Int]) = VectorSlice(reflectRead(x), start, end)
  def vector_contains[A:Manifest](x: Exp[Vector[A]], y: Exp[A]) = VectorContains(reflectRead(x), y)
  def vector_distinct[A:Manifest](x: Exp[Vector[A]]) = VectorDistinct(reflectRead(x))

  def vector_trans[A:Manifest](x: Exp[Vector[A]]) = VectorTrans(reflectRead(x))
  def vector_mutable_trans[A:Manifest](x: Exp[Vector[A]]) = reflectMutation(VectorMutableTrans(reflectReadWrite(x)))
  def vector_clone[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(VectorClone(reflectRead(x)))
  def vector_pprint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(VectorPPrint(reflectRead(x)))
  def vector_repmat[A:Manifest](x: Exp[Vector[A]], i: Exp[Int], j: Exp[Int]) = VectorRepmat(reflectRead(x),i,j)
  def vector_tolist[A:Manifest](x: Exp[Vector[A]]) = VectorToList(reflectRead(x))

  def vector_concatenate[A:Manifest](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorConcatenate(reflectWrite(x),reflectRead(y))
  def vector_update[A:Manifest](x: Exp[Vector[A]], n: Exp[Int], y: Exp[A]) = reflectMutation(VectorUpdate(reflectWrite(x), n, reflectRead(y)))
  def vector_copyfrom[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[Vector[A]]) = reflectMutation(VectorCopyFrom(reflectWrite(x), pos, reflectRead(y)))
  def vector_insert[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[A]) = reflectMutation(VectorInsert(reflectWrite(x), pos, reflectRead(y)))
  def vector_insertall[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[Vector[A]]) = reflectMutation(VectorInsertAll(reflectWrite(x), pos, reflectRead(y)))
  def vector_removeall[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], len: Exp[Int]) = reflectMutation(VectorRemoveAll(reflectWrite(x), pos, len))
  def vector_trim[A:Manifest](x: Exp[Vector[A]]) = reflectMutation(VectorTrim(reflectWrite(x)))
  def vector_clear[A:Manifest](x: Exp[Vector[A]]) = reflectMutation(VectorClear(reflectWrite(x)))
  
  def vector_plus[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlus(reflectWrite(x), reflectRead(y))
  def vector_plus_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = VectorPlusScalar(reflectWrite(x), reflectRead(y))
  def vector_plusequals[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectMutation(VectorPlusEquals(reflectReadWrite(x), reflectRead(y)))
  def vector_minus[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorMinus(reflectRead(x),reflectRead(y))
  def vector_minus_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = VectorMinusScalar(reflectRead(x), reflectRead(y))
  def vector_minusequals[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectMutation(VectorMinusEquals(reflectReadWrite(x), reflectRead(y)))
  def vector_times[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorTimes(reflectRead(x), reflectRead(y))
  def vector_times_withconvert[A:Manifest:Arith,B:Manifest](x: Exp[Vector[A]], y: Exp[Vector[B]], conv: Exp[B] => Exp[A]) = VectorTimesWithConvert(reflectRead(x),reflectRead(y),conv)
  def vector_times_withconvertright[A:Manifest,B:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[B]], conv: Exp[A] => Exp[B]) = VectorTimesWithConvertRight(reflectRead(x),reflectRead(y),conv)
  def vector_times_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = VectorTimesScalar(reflectRead(x), reflectRead(y))
  def vector_times_matrix[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Matrix[A]]) = VectorTimesMatrix(reflectRead(x), reflectRead(y))
  def vector_outer[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorOuter(reflectRead(x), reflectRead(y))
  def vector_dot_product[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorDotProduct(reflectRead(x), reflectRead(y))
  def vector_divide[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorDivide(reflectRead(x), reflectRead(y))
  def vector_divide_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = reflectEffect(VectorDivideScalar(reflectRead(x), reflectRead(y)))
  def vector_sum[A:Manifest:Arith](x: Exp[Vector[A]]) = VectorSum(reflectRead(x))
  def vector_abs[A:Manifest:Arith](x: Exp[Vector[A]]) = VectorAbs(reflectRead(x))
  def vector_exp[A:Manifest:Arith](x: Exp[Vector[A]]) = VectorExp(reflectRead(x))

  def vector_sort[A:Manifest:Ordering](x: Exp[Vector[A]]) = VectorSort(reflectRead(x))
  def vector_min[A:Manifest:Ordering](x: Exp[Vector[A]]) = VectorMin(reflectRead(x))
  //def vector_minIndex[A:Manifest:Ordering](x: Exp[Vector[A]]) = VectorMinIndex(reflectRead(x))
  def vector_max[A:Manifest:Ordering](x: Exp[Vector[A]]) = VectorMax(reflectRead(x))
  def vector_max_key[A:Manifest,B:Manifest:Ordering](x: Exp[Vector[A]], key: Exp[A] => Exp[B]) = VectorMaxKey(reflectRead(x), key)
*/

  def vector_length[A:Manifest](x: Exp[Vector[A]]) = reflectRead(x)(VectorLength(x))
  def vector_isRow[A:Manifest](x: Exp[Vector[A]]) = reflectRead(x)(VectorIsRow(x))
  def vector_apply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) = reflectRead(x)(VectorApply(x, n))
  def vector_slice[A:Manifest](x: Exp[Vector[A]], start: Exp[Int], end: Exp[Int]) = reflectNew(x)(VectorSlice(x, start, end))
  def vector_contains[A:Manifest](x: Exp[Vector[A]], y: Exp[A]) = reflectRead(x)(VectorContains(x, y))
  def vector_distinct[A:Manifest](x: Exp[Vector[A]]) = reflectNew(x)(VectorDistinct(x))

  def vector_trans[A:Manifest](x: Exp[Vector[A]]) = reflectNew(x)(VectorTrans(x))
  def vector_mutable_trans[A:Manifest](x: Exp[Vector[A]]) = reflectWrite(x)(x)(VectorMutableTrans(x))
  def vector_clone[A:Manifest](x: Exp[Vector[A]]) = reflectNew(x)(VectorClone(x))
  def vector_repmat[A:Manifest](x: Exp[Vector[A]], i: Exp[Int], j: Exp[Int]) = reflectNew(x)(VectorRepmat(x,i,j))
  def vector_tolist[A:Manifest](x: Exp[Vector[A]]) = reflectNew(x)(VectorToList(x))

  def vector_pprint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(VectorPPrint(/*reflectRead*/(x))(reifyEffectsHere(vector_pprint_impl[A](x))))

  def vector_concatenate[A:Manifest](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectNew(x,y)(VectorConcatenate(x,y))
  def vector_update[A:Manifest](x: Exp[Vector[A]], n: Exp[Int], y: Exp[A]) = reflectWrite(x)(x,y)(VectorUpdate(x, n, y))
  def vector_copyfrom[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[Vector[A]]) = reflectWrite(x)(x/*,y*/)(VectorCopyFrom(x, pos, y))
  def vector_insert[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[A]) = reflectWrite(x)(x/*,y*/)(VectorInsert(x, pos, y))
  def vector_insertall[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[Vector[A]]) = reflectWrite(x)(x,y)(VectorInsertAll(x, pos, y))
  def vector_removeall[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], len: Exp[Int]) = reflectWrite(x)(x)(VectorRemoveAll(x, pos, len))
  def vector_trim[A:Manifest](x: Exp[Vector[A]]) = reflectWrite(x)(x)(VectorTrim(x))
  def vector_clear[A:Manifest](x: Exp[Vector[A]]) = reflectWrite(x)()(VectorClear(x))

  def vector_plus[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectRead(x,y)(VectorPlus(x,y))
  def vector_plus_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = reflectRead(x/*,y*/)(VectorPlusScalar(x,y))
  def vector_plusequals[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectWrite(x)(x,y)(VectorPlusEquals(x,y))
  def vector_minus[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectRead(x,y)(VectorMinus(x,y))
  def vector_minus_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = reflectRead(x/*,y*/)(VectorMinusScalar(x,y))
  def vector_minusequals[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectWrite(x)(x,y)(VectorMinusEquals(x, y))
  def vector_times[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectRead(x,y)(new VectorTimesFresh(x,y))
  def vector_times_withconvert[A:Manifest:Arith,B:Manifest](x: Exp[Vector[A]], y: Exp[Vector[B]], conv: Exp[B] => Exp[A]) = reflectRead(x,y)(VectorTimesWithConvert(x,y,conv)) // TODO: de-hoas
  def vector_times_withconvertright[A:Manifest,B:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[B]], conv: Exp[A] => Exp[B]) = reflectRead(x,y)(VectorTimesWithConvertRight(x,y,conv))
  def vector_times_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = reflectRead(x/*,y*/)(new VectorTimesScalarFresh(x,y))
  def vector_times_matrix[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Matrix[A]]) = reflectRead(x,y)(VectorTimesMatrix(x,y))
  def vector_outer[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectRead(x,y)(VectorOuter(x,y))
  def vector_dot_product[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectRead(x,y)(VectorDotProduct(x,y))
  def vector_divide[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectRead(x,y)(VectorDivide(x,y))
  def vector_divide_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = reflectRead(x/*,y*/)(VectorDivideScalar(x,y))
  def vector_sum[A:Manifest:Arith](x: Exp[Vector[A]]) = reflectRead(x)(new VectorSumFresh(x))
  def vector_abs[A:Manifest:Arith](x: Exp[Vector[A]]) = reflectRead(x)(VectorAbs(x))
  def vector_exp[A:Manifest:Arith](x: Exp[Vector[A]]) = reflectRead(x)(VectorExp(x))

  def vector_sort[A:Manifest:Ordering](x: Exp[Vector[A]]) = reflectRead(x)(VectorSort(x))
  def vector_min[A:Manifest:Ordering](x: Exp[Vector[A]]) = reflectRead(x)(VectorMin(x))
  //def vector_minIndex[A:Manifest:Ordering](x: Exp[Vector[A]]) = VectorMinIndex(reflectRead(x))
  def vector_max[A:Manifest:Ordering](x: Exp[Vector[A]]) = reflectRead(x)(VectorMax(x))
  //def vector_maxIndex[A:Manifest:Ordering](x: Exp[Vector[A]]) = VectorMaxIndex(reflectRead(x))
  def vector_median[A:Manifest:Ordering](x: Exp[Vector[A]]) = reflectRead(x)(VectorMedian(x))
  def vector_max_key[A:Manifest,B:Manifest:Ordering](x: Exp[Vector[A]], key: Exp[A] => Exp[B]) = reflectRead(x)(VectorMaxKey(x, key))

  def vector_map[A:Manifest,B:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[B]) = {
    val v = fresh[A]
    val func = reifyEffects(f(v))
    reflectRead(x)(VectorMap(x, v, func)) // TODO: effect if func effectful!
  }
  def vector_mmap[A:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[A]) = {
    val v = fresh[A]
    val func = reifyEffects(f(v))
    reflectWrite(x)(x)(VectorMutableMap(x, v, func))  // TODO: effect if func effectful!
  }
  def vector_foreach[A:Manifest](x: Exp[Vector[A]], block: Exp[A] => Exp[Unit]) = {
    val v = fresh[A]
    val func = reifyEffects(block(v))
    reflectEffect(VectorForeach(x, v, func))  // TODO: read sym??
  }
  def vector_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Exp[Vector[A]], y: Exp[Vector[B]], f: (Exp[A],Exp[B]) => Exp[R]) = {
    val v = (fresh[A], fresh[B])
    val func = reifyEffects(f(v._1,v._2))
    reflectRead(x,y)(VectorZipWith(x, y, v, func))
  }
  def vector_reduce[A:Manifest](x: Exp[Vector[A]], f: (Exp[A],Exp[A]) => Exp[A]) = {
    val v = (fresh[A],fresh[A])
    val func = reifyEffects(f(v._1, v._2))
    reflectRead(x)(VectorReduce(x, v, func))
  }

// TODO: this is like an aggregating reduction, or flatMap reduction; the reduction function should be concatentation
// we can't do this yet because of our issues with flatMap
//
// or more efficiently expressed, this is like a reduce of (A,B) with a function from (A,B) => A;
// in this case, it's (Vector[A],A) => Vector[A]. this is a more general form of reduction than we can express currently.
// reduction function should be: if (pred(b)) a += b else a
//
//  def vector_filter[A:Manifest](x: Rep[Vector[A]], pred: Rep[A] => Rep[Boolean]) = {
//    val mV = fresh[A]
//    val map = reifyEffects(if pred(mV) Vector(mV) else NilVector[A])
//    val rV = (fresh[Vector[A]], fresh[Vector[A]])
//    val reduce = rV._1 ++= rV._2
//
//  }
  def vector_filter[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) = reflectRead(x)(VectorFilter(x, pred))

  def vector_flatmap[A:Manifest,B:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[Vector[B]]) = {
    val v = fresh[A]
    val func = reifyEffects(f(v))
    reflectRead(x)(VectorFlatMap(x, v, func))
  }

  def vector_partition[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) = t2(reflectRead(x)(VectorPartition(x, pred)))

  def vector_nil[A:Manifest] = VectorNil[A]()

}

/**
 * Optimizations for composite VectorOps operations.
 */

trait VectorOpsExpOpt extends VectorOpsExp {
  this: VectorImplOps with OptiMLExp =>

  override def vector_plus[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    // (TB + TD) == T(B + D)
    case (Def(VectorTimes(a, b)), Def(VectorTimes(c, d))) if (a == c) => vector_times[A](a.asInstanceOf[Exp[Vector[A]]], vector_plus[A](b.asInstanceOf[Exp[Vector[A]]],d.asInstanceOf[Exp[Vector[A]]]))
    // ...
    case _ => super.vector_plus(x, y)
  }

  override def vector_plusequals[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    // remove runtime check on zero vector being same length as argument
    case (a, Def(VectorObjectZeros(len))) => a
    case (Def(VectorObjectZeros(len)), b) => b
    case _ => super.vector_plusequals(x,y)
  }

  override def vector_times[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    case _ => super.vector_times(x, y)
  }


  // these are essential for fusing:

  override def vector_length[A:Manifest](x: Exp[Vector[A]]) = x match {
    case Def(Reflect(e @ VectorTimes(_,_), _,_)) => e.asInstanceOf[DeliteOpVectorLoop[A]].size // FIXME: in general this is unsafe, but hey...
    case Def(Reflect(e @ VectorObjectZeros(l), _,_)) => l // FIXME: in general this is unsafe, but hey...
    case Def(Reflect(e @ VectorClone(a), _,_)) => vector_length(a) // FIXME: in general this is unsafe, but hey...
    case Def(e: DeliteOpVectorLoop[A]) => e.size
    case Def(VectorObjectZeros(l)) => l
    case Def(VectorClone(a)) => vector_length(a)
    case Def(VectorObjectRange(s,e,d,r)) => (e - s + d - 1)
    case Def(MatrixVView(x, start, stride, l, r)) => l
    case Def(MatrixGetRow(x,i)) => x.numCols
    case _ => super.vector_length(x)
  }

  override def vector_isRow[A:Manifest](x: Exp[Vector[A]]) = x match {
    case Def(e: DeliteOpVectorLoop[A]) => e.isRow
    //case Def(Reflect(VectorObjectZeros(l,r), _)) => r
    case Def(VectorClone(a)) => vector_isRow(a)
    case Def(VectorObjectRange(s,e,d,r)) => r
    case Def(MatrixVView(x, start, stride, l, r)) => r
    case Def(MatrixGetRow(x,i)) => Const(true)
    case _ => super.vector_isRow(x)
  }
  
  // and this one also helps in the example:
  
  override def vector_apply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) = x match {
    case Def(VectorObjectZeros(l)) => unit(0).asInstanceOf[Exp[A]]
    case Def(VectorObjectOnes(l)) => unit(1).asInstanceOf[Exp[A]]
    case Def(VectorObjectRange(s,e,d,r)) => (s + n*d).asInstanceOf[Exp[A]]
    case Def(VectorTrans(x)) => vector_apply(x,n)
    case _ => super.vector_apply(x,n)
  }
  
}

trait BaseGenVectorOps extends GenericFatCodegen {
  val IR: VectorOpsExp
  import IR._

  override def unapplySimpleIndex(e: Def[Any]) = e match { // TODO: move elsewhere
    case VectorApply(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }

  //override def syms(e: Any): List[Sym[Any]] = e match {
    //case VectorObjectFromSeq(xs) => List(xs)
    //case _ => super.syms(e)
  //}
}

trait ScalaGenVectorOps extends BaseGenVectorOps with ScalaGenFat {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      // these are the ops that call through to the underlying real data structure
      case VectorApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
      case VectorUpdate(x,n,y) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))
      case VectorLength(x)    => emitValDef(sym, quote(x) + ".length")
      case VectorIsRow(x)     => emitValDef(sym, quote(x) + ".isRow")
      case VectorMutableTrans(x) => emitValDef(sym, quote(x) + ".mtrans")
      case VectorSort(x) => emitValDef(sym, quote(x) + ".sort")
      case VectorToList(x) => emitValDef(sym, quote(x) + ".toList")
      case VectorCopyFrom(x,pos,y) => emitValDef(sym, quote(x) + ".copyFrom(" + quote(pos) + ", " + quote(y) + ")")
      case VectorInsert(x,pos,y) => emitValDef(sym, quote(x) + ".insert(" + quote(pos) + ", " + quote(y) + ")")
      case VectorInsertAll(x,pos,y) => emitValDef(sym, quote(x) + ".insertAll(" + quote(pos) + ", " + quote(y) + ")")
      case VectorRemoveAll(x,pos,len) => emitValDef(sym, quote(x) + ".removeAll(" + quote(pos) + ", " + quote(len) + ")")
      case VectorTrim(x) => emitValDef(sym, quote(x) + ".trim")
      case VectorClear(x) => emitValDef(sym, quote(x) + ".clear()")
      case VectorClone(x) => emitValDef(sym, quote(x) + ".cloneL")
//      case v@VectorObjectNew(length, isRow) => emitValDef(sym, "new " + remap(v.mV) + "(" + quote(length) + "," + quote(isRow) + ")")
      case v@VectorNew(length, isRow) => emitValDef(sym, "new " + remap(v.mV) + "(" + quote(length) + "," + quote(isRow) + ")")
      case VectorObjectRange(start, end, stride, isRow) => emitValDef(sym, "new " + remap(manifest[RangeVectorImpl]) + "(" + quote(start) + "," + quote(end) + "," + quote(stride) + "," + quote(isRow) + ")")
      // TODO: why!!!
      case v@VectorNil() => v.mA.toString match {
                              case "Int" => emitValDef(sym, "NilVectorIntImpl")
                              case "Double" => emitValDef(sym, "NilVectorDoubleImpl")
                              case _ => throw new UnsupportedOperationException("NilVector")
                            }

      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenVectorOps extends BaseGenVectorOps with CudaGenFat with CudaGenDataStruct {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

    case VectorApply(x, n) =>
      emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case VectorUpdate(x,n,y) =>
      stream.println(addTab() + "%s.update(%s,%s);".format(quote(x),quote(n),quote(y)))
    case VectorLength(x)    =>
      emitValDef(sym, quote(x) + ".length")
    case VectorIsRow(x)     =>
      emitValDef(sym, quote(x) + ".isRow")

    /* Specialized CUDA code generations */
    case VectorObjectRange(start, end, stride, isRow) =>
      stream.println(addTab()+"RangeVector %s;".format(quote(sym)))
      stream.println(addTab()+"%s.start = %s;".format(quote(sym),quote(start)))
      stream.println(addTab()+"%s.end = %s;".format(quote(sym),quote(end)))
      stream.println(addTab()+"%s.stride = %s;".format(quote(sym),quote(stride)))
      stream.println(addTab()+"%s.isRow = %s;".format(quote(sym),quote(isRow)))

    case VectorObjectZeros(len) =>
        currDim += 1
        val currDimStr = getCurrDimStr()
        setCurrDimLength(quote(len))
        emitVectorAlloc(sym,"%s".format(quote(len)),"true") //needs to allocate with new symbol
		//if(currDim == 2) {
		//	stream.println(addTab()+"%s %s_save = %s.data;".format(remap(sym.Type.typeArguments(0)),quote(sym),quote(sym)))
        //	stream.println(addTab()+"%s.data += %s*%s;".format(quote(sym),quote(len),getPrevDimStr()))
		//}
        stream.println(addTab()+"if(%s < %s) {".format(currDimStr,quote(len)))
        tabWidth += 1
        stream.println(addTab()+"%s.update(%s,0);".format(quote(sym),currDimStr))
        tabWidth -= 1
        stream.println(addTab()+"}")
		//if(currDim == 2) {
		//	stream.println(addTab()+"%s.data = %s_save;".format(quote(sym),quote(sym)))
		//}
        currDim -= 1
	/*
      if(currDim > 1)
        throw new GenerationFailedException("CudaGen: No more than 2 dimensions are allowed for GPU kernels.")
      else {
        currDim += 1
        val currDimStr = getCurrDimStr()
		val prevDimStr = if(currDim == 2) getPrevDimStr() else "0"
		val prevDimSize = if(currDim == 2) xDimList(0) else "1"
        setCurrDimLength(quote(len))
        emitVectorAlloc(sym,"%s*%s".format(quote(len),prevDimSize),"true") //needs to allocate with new symbol
        stream.println(addTab()+"%s.length = %s;".format(quote(sym),quote(len)))
        stream.println(addTab()+"%s.isRow = true;".format(quote(sym)))
        stream.println(addTab()+"%s.data += %s*%s;".format(quote(sym),quote(len),prevDimStr))
        stream.println(addTab()+"if(%s < %s) {".format(currDimStr,quote(len)))
        tabWidth += 1
        stream.println(addTab()+"%s.update(%s,0);".format(quote(sym),currDimStr))
        tabWidth -= 1
        stream.println(addTab()+"}")
        currDim -= 1
      }
	*/

    case VectorTrans(x) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength("%s->length".format(quote(x)))
      stream.println(addTab()+"if( %s < %s.size() ) {".format(currDimStr,quote(x)))
      tabWidth += 1
      stream.println(addTab()+"%s.update(%s,%s.apply(%s));".format(quote(sym),currDimStr,quote(x),currDimStr))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitVectorAlloc(sym,"%s->length".format(quote(x)),"!%s->isRow".format(quote(x)))
      currDim -= 1

    case VectorRepmat(x,i,j) =>
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
      emitMatrixAlloc(sym,"%s".format(quote(i)),"%s->length*%s".format(quote(x),quote(j)))
      currDim -= 1

    case VectorOuter(x,y) =>
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
      emitMatrixAlloc(sym,"%s->length".format(quote(x)),"%s->length".format(quote(x)))
      currDim -= 1

    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenVectorOps extends BaseGenVectorOps with CGenFat {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case VectorObjectZeros(len) =>
      stream.println("%s *%s_data = malloc(sizeof(%s)*%s);".format(remap(sym.Type.typeArguments(0)),quote(sym),remap(sym.Type.typeArguments(0)),quote(len)))
      stream.println("memset(%s_data,0,sizeof(%s)*%s);".format(quote(sym),remap(sym.Type.typeArguments(0)),quote(len)))
      stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
      stream.println("%s.length = %s;".format(quote(sym),quote(len)))
      stream.println("%s.isRow = true;".format(quote(sym)))
      stream.println("%s.data = %s_data;".format(quote(sym),quote(sym)))
    case VectorNew(len,isRow) =>
//    case VectorObjectNew(len,isRow) =>
      stream.println("%s *%s_data = malloc(sizeof(%s)*%s);".format(remap(sym.Type.typeArguments(0)),quote(sym),remap(sym.Type.typeArguments(0)),quote(len)))
      stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
      stream.println("%s.length = %s;".format(quote(sym),quote(len)))
      stream.println("%s.isRow = %s;".format(quote(sym),quote(isRow)))
      stream.println("%s.data = %s_data;".format(quote(sym),quote(sym)))
    case VectorApply(x, n) =>
      emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case VectorUpdate(x,n,y) =>
      stream.println("%s.update(%s,%s);".format(quote(x),quote(n),quote(y)))
    case VectorLength(x)    =>
      emitValDef(sym, quote(x) + ".length")
    case VectorIsRow(x)     =>
      emitValDef(sym, quote(x) + ".isRow")

    case _ => super.emitNode(sym, rhs)
  }
}

