package ppl.dsl.optiml

import datastruct.CudaGenDataStruct
import datastruct.scala._
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.Manifest
import scala.virtualization.lms.common.{CudaGenFat, ScalaGenFat}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}

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

    // arithmetic operations
    def +(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_plus(x,y)
    def +(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = vector_plus_scalar(x,y)
    def +=(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_plusequals(x,y)
    def -(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_minus(x,y)
    def -(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = vector_minus_scalar(x,y)
    def *(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_times(x,y)
    def *[B](y: Rep[Vector[B]])(implicit mB: Manifest[B], a: Arith[A], conv: Rep[B] => Rep[A]) = vector_times_withconvert(x,y,conv)
    //def *[B](y: Rep[Vector[B]])(implicit mB: Manifest[B], a: Arith[A], conv: Rep[A] => Rep[B], o: Overloaded1) = vector_times_withconvertright(x,y,conv)
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

  def NilV[A:Manifest] = vector_nil

  // object defs
  def vector_obj_new[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]): Rep[Vector[A]]
  def vector_obj_fromseq[A:Manifest](xs: Rep[Seq[A]]): Rep[Vector[A]]
  def vector_obj_ones(len: Rep[Int]): Rep[Vector[Double]]
  def vector_obj_onesf(len: Rep[Int]): Rep[Vector[Float]]
  def vector_obj_zeros(len: Rep[Int]): Rep[Vector[Double]]
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

  def vector_concatenate[A:Manifest](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_update[A:Manifest](x: Rep[Vector[A]], n: Rep[Int], y: Rep[A]): Rep[Unit]
  def vector_copyfrom[A:Manifest](x: Rep[Vector[A]], pos: Rep[Int], y: Rep[Vector[A]]): Rep[Unit]
  def vector_insert[A:Manifest](x: Rep[Vector[A]], pos: Rep[Int], y: Rep[A]): Rep[Unit]
  def vector_insertall[A:Manifest](x: Rep[Vector[A]], pos: Rep[Int], y: Rep[Vector[A]]): Rep[Unit]
  def vector_removeall[A:Manifest](x: Rep[Vector[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit]
  def vector_trim[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]

  def vector_plus[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_plus_scalar[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vector_plusequals[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_minus[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_minus_scalar[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vector_times[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_times_withconvert[A:Manifest:Arith,B:Manifest](x: Rep[Vector[A]], y: Rep[Vector[B]],  conv: Rep[B] => Rep[A]): Rep[Vector[A]]
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

trait VectorOpsExp extends VectorOps with VariablesExp with BaseFatExp {

  this: VectorImplOps with OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class VectorObjectRange(start: Exp[Int], end: Exp[Int], stride: Exp[Int], isRow: Exp[Boolean])
    extends Def[RangeVector]

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
  case class VectorMutableTrans[A:Manifest](x: Exp[Vector[A]]) extends Def[Vector[A]]
  case class VectorClone[A:Manifest](x: Exp[Vector[A]]) extends Def[Vector[A]]
  // TODO: right now we just use the underlying data structure sort, but we should implement our own fast parallel sort
  // with delite ops
  case class VectorSort[A:Manifest:Ordering](x: Exp[Vector[A]]) extends Def[Vector[A]]


  /////////////////////////////////////////////////
  // implemented via kernel embedding (sequential)

  case class VectorObjectFromSeq[A:Manifest](xs: Exp[Seq[A]])
    extends DeliteOpSingleTask(reifyEffects(vector_obj_fromseq_impl(xs)))

  case class VectorObjectOnes(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(vector_obj_ones_impl(len)))

  case class VectorObjectOnesF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(vector_obj_onesf_impl(len)))

  case class VectorObjectZeros(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(vector_obj_zeros_impl(len)))

  case class VectorObjectZerosF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(vector_obj_zerosf_impl(len)))

  case class VectorObjectRand(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(vector_obj_rand_impl(len)))

  case class VectorObjectRandF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(vector_obj_randf_impl(len)))

  case class VectorObjectUniform(start: Exp[Double], step_size: Exp[Double], end: Exp[Double], isRow: Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffects(vector_obj_uniform_impl(start, step_size, end, isRow)))

//  case class VectorObjectFlatten[A:Manifest](pieces: Exp[Vector[Vector[A]]])
//    extends DeliteOpSingleTask(reifyEffects(vector_obj_flatten_impl(pieces)))

  case class VectorConcatenate[A:Manifest](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffects(vector_concatenate_impl(x,y)))
  
  case class VectorSlice[A:Manifest](x: Exp[Vector[A]], start: Exp[Int], end: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(vector_slice_impl(x,start,end)))

  case class VectorTimesMatrix[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Matrix[A]])
    extends DeliteOpSingleTask(reifyEffects(vector_times_matrix_impl[A](x,y)))

  case class VectorOuter[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffects(vector_outer_impl[A](x,y)))

  case class VectorPPrint[A](x: Exp[Vector[A]])(block: Exp[Unit]) // stupid limitation...
    extends DeliteOpSingleTask(block)
    // reifyEffects(vector_pprint_impl[A](x))

/*
  case class VectorTrans[A:Manifest](x: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffects(vector_trans_impl[A](x)))
*/

  case class VectorRepmat[A:Manifest](x: Exp[Vector[A]], i: Exp[Int], j: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(vector_repmat_impl[A](x,i,j)))

  case class VectorMedian[A:Manifest:Ordering](x: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffects(vector_median_impl[A](x)))

  case class VectorFilter[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffects(vector_filter_impl(x, pred)))

  case class VectorPartition[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffects(vector_partition_impl(x, pred)))

  case class VectorContains[A:Manifest](x: Exp[Vector[A]], y: Exp[A])
    extends DeliteOpSingleTask(reifyEffects(vector_contains_impl[A](x, y)))

  case class VectorDistinct[A:Manifest](x: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffects(vector_distinct_impl[A](x)))



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

  case class VectorTrans[A:Manifest](in: Exp[Vector[A]])
    extends DeliteOpLoop[Vector[A]] {

    val size = in.length
    val v = fresh[Int]
    val body: Def[Vector[A]] = DeliteCollectElem[A,Vector](
      alloc = reifyEffects(Vector[A](in.length, !in.isRow)),
      func = in(v)
    )
  }

  case class VectorPlus[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]]) 
    extends DeliteOpLoop[Vector[A]] {

    val size = inA.length
    val v = fresh[Int]
    val body: Def[Vector[A]] = DeliteCollectElem[A,Vector](
      alloc = reifyEffects(Vector[A](inA.length, inA.isRow)),
      func = inA(v) + inB(v)
    )
  }

  //TR TODO
  case class VectorPlusScalar[A:Manifest:Arith](in: Exp[Vector[A]], y: Exp[A])
    extends DeliteOpMap[A,A,Vector] {

    val alloc = reifyEffects(Vector[A](in.length, in.isRow))
    val v = fresh[A]
    val func = v + y
  }

  case class VectorPlusEquals[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector] { // stick to ZipWith for the moment TODO: try multi loop

    val alloc = inA
    val v = (fresh[A],fresh[A])
    val func = v._1 + v._2
  }

  case class VectorMinus[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpLoop[Vector[A]] {

    val size = inA.length
    val v = fresh[Int]
    val body: Def[Vector[A]] = DeliteCollectElem[A,Vector](
      alloc = reifyEffects(Vector[A](inA.length, inA.isRow)),
      func = inA(v) - inB(v)
    )
  }

  //TR TODO
  case class VectorMinusScalar[A:Manifest:Arith](in: Exp[Vector[A]], y: Exp[A])
    extends DeliteOpMap[A,A,Vector] {

    val alloc = reifyEffects(Vector[A](in.length, in.isRow))
    val v = fresh[A]
    val func = v - y
  }

  case class VectorTimes[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpLoop[Vector[A]] {

    val size = inA.length
    val v = fresh[Int]
    val body: Def[Vector[A]] = DeliteCollectElem[A,Vector](
      alloc = reifyEffects(Vector[A](inA.length, inA.isRow)),
      func = inA(v) * inB(v)
    )
  }

  //TR TODO ?
  case class VectorTimesWithConvert[A:Manifest:Arith,B:Manifest](inA: Exp[Vector[A]], inB: Exp[Vector[B]], conv: Exp[B] => Exp[A])
    extends DeliteOpZipWith[A,B,A,Vector] {

    val alloc = reifyEffects(Vector[A](inA.length, inA.isRow))
    val v = (fresh[A],fresh[B])
    val func = v._1 * conv(v._2)
  }

  //TR TODO
  case class VectorTimesScalar[A:Manifest:Arith](in: Exp[Vector[A]], y: Exp[A])
    extends DeliteOpMap[A,A,Vector] {

    val alloc = reifyEffects(Vector[A](in.length, in.isRow))
    val v = fresh[A]
    val func = v * y
  }

  //TR TODO
  case class VectorDotProduct[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWithReduce[A,A,A,Vector] {

    val zV = (fresh[A],fresh[A])
    val zip = (zV._1 * zV._2)
    val rV = (fresh[A],fresh[A])
    val reduce = reifyEffects(rV._1 += rV._2)
  }

  case class VectorDivide[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpLoop[Vector[A]] {

    val size = inA.length
    val v = fresh[Int]
    val body: Def[Vector[A]] = new DeliteCollectElem[A,Vector](
      alloc = reifyEffects(Vector[A](inA.length, inA.isRow)),
      func = inA(v) / inB(v)
    )
  }

  case class VectorDivideScalar[A:Manifest:Arith](in: Exp[Vector[A]], y: Exp[A])
    extends DeliteOpLoop[Vector[A]] {

    val size = in.length
    val v = fresh[Int]
    val body: Def[Vector[A]] = new DeliteCollectElem[A,Vector](
      alloc = reifyEffects(Vector[A](in.length, in.isRow)),
      func = in(v) / y
    )
  }

  case class VectorSum[A:Manifest:Arith](in: Exp[Vector[A]])
    extends DeliteOpLoop[A] {

    val size = in.length
    val v = fresh[Int]
    private[this] val rV = (fresh[A],fresh[A])
    val body: Def[A] = DeliteReduceElem[A]( //TODO might need explicit zero?
      func = in(v),
      rV = rV,
      rFunc = rV._1 + rV._2
    )
  }

  case class VectorAbs[A:Manifest:Arith](in: Exp[Vector[A]])
    extends DeliteOpMap[A,A,Vector] {

    val alloc = reifyEffects(Vector[A](in.length, in.isRow))
    val v = fresh[A]
    val func = v.abs
  }

  case class VectorExp[A:Manifest:Arith](in: Exp[Vector[A]])
    extends DeliteOpMap[A,A,Vector] {

    val alloc = reifyEffects(Vector[A](in.length, in.isRow))
    val v = fresh[A]
    val func = v.exp
  }

  //TR TODO
  case class VectorMin[A:Manifest:Ordering](in: Exp[Vector[A]])
    extends DeliteOpReduce[A] {

    val v = (fresh[A],fresh[A])
    val func = if (v._1 < v._2) v._1 else v._2
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
    val func = if (v._1 > v._2) v._1 else v._2
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

    val alloc = reifyEffects(Vector[Vector[B]](in.length, in.isRow))
    val rV = (fresh[Vector[B]],fresh[Vector[B]])
    val reduce = reifyEffects(rV._1 ++ rV._2)
  }


  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case VectorApply(x, n) => vector_apply(f(x), f(n))
    case VectorLength(x) => vector_length(f(x))
    case VectorIsRow(x) => vector_isRow(f(x))
    case Reflect(e@VectorPPrint(x), es) => toAtom(Reflect(VectorPPrint(f(x))(f(e.block)), es map (e => f(e))))
    case Reflect(VectorObjectZeros(x), es) => toAtom(Reflect(VectorObjectZeros(f(x)), es map (e => f(e))))
    case Reflect(VectorObjectRange(s,e,d,r), es) => toAtom(Reflect(VectorObjectRange(f(s),f(e),f(d),f(r)), es map (e => f(e))))
    case Reflect(e@VectorNew(l,r), es) => toAtom(Reflect(VectorNew(f(l),f(r))(e.mV), es map (e => f(e))))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??


  /////////////////////
  // object interface

  def vector_obj_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) = reflectEffect(VectorNew[A](len, isRow)(manifest[VectorImpl[A]]))
  def vector_obj_fromseq[A:Manifest](xs: Exp[Seq[A]]) = reflectEffect(VectorObjectFromSeq(xs))
  def vector_obj_ones(len: Exp[Int]) = reflectEffect(VectorObjectOnes(len))
  def vector_obj_onesf(len: Exp[Int]) = reflectEffect(VectorObjectOnesF(len))
  def vector_obj_zeros(len: Exp[Int]) = reflectEffect(VectorObjectZeros(len))
  def vector_obj_zerosf(len: Exp[Int]) = reflectEffect(VectorObjectZerosF(len))
  def vector_obj_rand(len: Exp[Int]) = reflectEffect(VectorObjectRand(len))
  def vector_obj_randf(len: Exp[Int]) = reflectEffect(VectorObjectRandF(len))
  def vector_obj_range(start: Exp[Int], end: Exp[Int], stride: Exp[Int], isRow: Exp[Boolean]) = reflectEffect(VectorObjectRange(start, end, stride, isRow))
  def vector_obj_uniform(start: Exp[Double], step_size: Exp[Double], end: Exp[Double], isRow: Exp[Boolean]) = reflectEffect(VectorObjectUniform(start, step_size, end, isRow))
  def vector_obj_flatten[A:Manifest](pieces: Exp[Vector[Vector[A]]]) = VectorObjectFlatten(reflectRead(pieces))


  /////////////////////
  // class interface

  def vector_length[A:Manifest](x: Exp[Vector[A]]) = VectorLength(reflectRead(x))
  def vector_isRow[A:Manifest](x: Exp[Vector[A]]) = VectorIsRow(reflectRead(x))
  def vector_apply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) = VectorApply(reflectRead(x), n)
  def vector_slice[A:Manifest](x: Exp[Vector[A]], start: Exp[Int], end: Exp[Int]) = VectorSlice(reflectRead(x), start, end)
  def vector_contains[A:Manifest](x: Exp[Vector[A]], y: Exp[A]) = VectorContains(reflectRead(x), y)
  def vector_distinct[A:Manifest](x: Exp[Vector[A]]) = VectorDistinct(reflectRead(x))

  def vector_trans[A:Manifest](x: Exp[Vector[A]]) = VectorTrans(reflectRead(x))
  def vector_mutable_trans[A:Manifest](x: Exp[Vector[A]]) = reflectMutation(VectorMutableTrans(reflectReadWrite(x)))
  def vector_clone[A:Manifest](x: Exp[Vector[A]]) = VectorClone(reflectRead(x))
  def vector_repmat[A:Manifest](x: Exp[Vector[A]], i: Exp[Int], j: Exp[Int]) = VectorRepmat(reflectRead(x),i,j)

  def vector_pprint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(VectorPPrint(reflectRead(x))(reifyEffects(vector_pprint_impl[A](x))))

  def vector_concatenate[A:Manifest](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorConcatenate(reflectRead(x),reflectRead(y))
  def vector_update[A:Manifest](x: Exp[Vector[A]], n: Exp[Int], y: Exp[A]) = reflectMutation(VectorUpdate(reflectWrite(x), n, reflectRead(y)))
  def vector_copyfrom[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[Vector[A]]) = reflectMutation(VectorCopyFrom(reflectRead(x), pos, reflectRead(y)))
  def vector_insert[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[A]) = reflectMutation(VectorInsert(reflectRead(x), pos, reflectRead(y)))
  def vector_insertall[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[Vector[A]]) = reflectMutation(VectorInsertAll(reflectRead(x), pos, reflectRead(y)))
  def vector_removeall[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], len: Exp[Int]) = reflectMutation(VectorRemoveAll(reflectRead(x), pos, len))
  def vector_trim[A:Manifest](x: Exp[Vector[A]]) = reflectMutation(VectorTrim(reflectRead(x)))

  def vector_plus[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlus(reflectRead(x), reflectRead(y))
  def vector_plus_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = VectorPlusScalar(reflectRead(x), reflectRead(y))
  def vector_plusequals[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectMutation(VectorPlusEquals(reflectRead(x), reflectRead(y)))
  def vector_minus[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorMinus(reflectRead(x),reflectRead(y))
  def vector_minus_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = VectorMinusScalar(reflectRead(x), reflectRead(y))
  def vector_times[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorTimes(reflectRead(x), reflectRead(y))
  def vector_times_withconvert[A:Manifest:Arith,B:Manifest](x: Exp[Vector[A]], y: Exp[Vector[B]], conv: Exp[B] => Exp[A]) = VectorTimesWithConvert(reflectRead(x),reflectRead(y),conv)
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
  //def vector_maxIndex[A:Manifest:Ordering](x: Exp[Vector[A]]) = VectorMaxIndex(reflectRead(x))
  def vector_median[A:Manifest:Ordering](x: Exp[Vector[A]]) = VectorMedian(reflectRead(x))

  def vector_map[A:Manifest,B:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[B]) = {
    val v = fresh[A]
    val func = reifyEffects(f(v))
    VectorMap(reflectRead(x), v, func)
  }
  def vector_mmap[A:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[A]) = {
    val v = fresh[A]
    val func = reifyEffects(f(v))
    reflectMutation(VectorMutableMap(reflectReadWrite(x), v, func))
  }
  def vector_foreach[A:Manifest](x: Exp[Vector[A]], block: Exp[A] => Exp[Unit]) = {
    val v = fresh[A]
    val func = reifyEffects(block(v))
    reflectEffect(VectorForeach(reflectRead(x), v, func))
  }
  def vector_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Exp[Vector[A]], y: Exp[Vector[B]], f: (Exp[A],Exp[B]) => Exp[R]) = {
    val v = (fresh[A], fresh[B])
    val func = reifyEffects(f(v._1,v._2))
    VectorZipWith(reflectRead(x), reflectRead(y), v, func)
  }
  def vector_reduce[A:Manifest](x: Exp[Vector[A]], f: (Exp[A],Exp[A]) => Exp[A]) = {
    val v = (fresh[A],fresh[A])
    val func = reifyEffects(f(v._1, v._2))
    VectorReduce(reflectRead(x), v, func)
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
  def vector_filter[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) = VectorFilter(reflectRead(x), pred)

  def vector_flatmap[A:Manifest,B:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[Vector[B]]) = {
    val v = fresh[A]
    val func = reifyEffects(f(v))
    VectorFlatMap(reflectRead(x), v, func)
  }

  def vector_partition[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) = t2(VectorPartition(reflectRead(x), pred))

  def vector_nil[A:Manifest] = VectorNil[A]()

}

/**
 * Optimizations for composite VectorOps operations.
 */

trait VectorOpsExpOpt extends VectorOpsExp {
  this: VectorImplOps with OptiMLExp =>

  override def vector_plus[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    // (TB + TD) == T(B + D)
    case (Def(VectorTimes(a, b)), Def(VectorTimes(c, d))) if (a == c) => VectorTimes[A](a.asInstanceOf[Exp[Vector[A]]], VectorPlus[A](b.asInstanceOf[Exp[Vector[A]]],d.asInstanceOf[Exp[Vector[A]]]))
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
    case Def(e: DeliteOpLoop[Vector[A]]) => 
      e.body match {
        case DeliteCollectElem(alloc, _) => vector_length(alloc)
        case _ => super.vector_length(x) // vector constructed by reduction
      }
    case Def(Reify(e, _)) => vector_length(e) // FIXME: not sure this is always safe! <--- any reflect escaping its reify?
    case Def(Reflect(VectorObjectZeros(l), _)) => l
    case Def(Reflect(VectorObjectRange(s,e,d,r), _)) => (e - s + d - 1)
    case Def(Reflect(VectorNew(l,r), _)) => l
    case _ => super.vector_length(x)
  }

  override def vector_isRow[A:Manifest](x: Exp[Vector[A]]) = x match {
    case Def(e: DeliteOpLoop[Vector[A]]) => 
      e.body match {
        case DeliteCollectElem(alloc, _) => vector_isRow(alloc)
        case _ => super.vector_isRow(x) // vector constructed by reduction
      }
    case Def(Reify(e, _)) => vector_isRow(e) // FIXME: not sure this is always safe! <--- any reflect escaping its reify?
    //case Def(Reflect(VectorObjectZeros(l,r), _)) => r
    case Def(Reflect(VectorObjectRange(s,e,d,r), es)) => r
    case Def(Reflect(VectorNew(l,r), _)) => r
    case _ => super.vector_isRow(x)
  }
  
  // and this one also helps in the example:
  
  override def vector_apply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) = x match {
    case Def(Reflect(VectorObjectRange(s,e,d,r), _)) => (s + n*d).asInstanceOf[Exp[A]]
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

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      // these are the ops that call through to the underlying real data structure
      case VectorApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
      case VectorUpdate(x,n,y) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))
      case VectorLength(x)    => emitValDef(sym, quote(x) + ".length")
      case VectorIsRow(x)     => emitValDef(sym, quote(x) + ".isRow")
      case VectorMutableTrans(x) => emitValDef(sym, quote(x) + ".mtrans")
      case VectorSort(x) => emitValDef(sym, quote(x) + ".sort")
      case VectorCopyFrom(x,pos,y) => emitValDef(sym, quote(x) + ".copyFrom(" + quote(pos) + ", " + quote(y) + ")")
      case VectorInsert(x,pos,y) => emitValDef(sym, quote(x) + ".insert(" + quote(pos) + ", " + quote(y) + ")")
      case VectorInsertAll(x,pos,y) => emitValDef(sym, quote(x) + ".insertAll(" + quote(pos) + ", " + quote(y) + ")")
      case VectorRemoveAll(x,pos,len) => emitValDef(sym, quote(x) + ".removeAll(" + quote(pos) + ", " + quote(len) + ")")
      case VectorTrim(x) => emitValDef(sym, quote(x) + ".trim")
      case VectorClone(x) => emitValDef(sym, quote(x) + ".cloneL")
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

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {

    case VectorOuter(x,y) =>
      gpuBlockSizeX = quote(x)+".length"
      stream.println(addTab()+"if( %s < %s ) {".format("idxX",quote(x)+".length"))
      tabWidth += 1
      stream.println(addTab()+"for(int i=0; i<%s.length; i++) {".format(quote(x))); tabWidth += 1
      stream.println(addTab()+"%s.update(%s, %s, %s.apply(%s)*%s.apply(%s));".format(quote(sym),"i","idxX",quote(x),"i",quote(y),"idxX"))
      if(getVarLink(sym) != null) stream.println(addTab()+"%s.update(%s, %s, %s.apply(%s,%s));".format(quote(getVarLink(sym)),"i","idxX",quote(sym),"i","idxX"))
      tabWidth -= 1; stream.println(addTab()+"}")
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s.length".format(quote(x)),"%s.length".format(quote(x)))

    case VectorObjectZeros(len) =>
      throw new GenerationFailedException("CudaGen: Not GPUable (Dynamic memory allocation is not allowed)")
    case VectorNew(len,isRow) =>
      throw new GenerationFailedException("CudaGen: Not GPUable (Dynamic memory allocation is not allowed)")
    case VectorApply(x, n) =>
      emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case VectorUpdate(x,n,y) =>
      stream.println(addTab() + "%s.update(%s,%s);".format(quote(x),quote(n),quote(y)))
    case VectorLength(x)    =>
      emitValDef(sym, quote(x) + ".length")
    case VectorIsRow(x)     =>
      emitValDef(sym, quote(x) + ".isRow")
    case VectorObjectRange(start, end, stride, isRow) =>
	  if(gpuBlockSizeX != null) {
        stream.println(addTab()+"RangeVector %s;".format(quote(sym)))
        stream.println(addTab()+"%s.start = %s;".format(quote(sym),quote(start)))
        stream.println(addTab()+"%s.end = %s;".format(quote(sym),quote(end)))
        stream.println(addTab()+"%s.stride = %s;".format(quote(sym),quote(stride)))
        stream.println(addTab()+"%s.isRow = %s;".format(quote(sym),quote(isRow)))
	  }

        /* Specialized CUDA code generations */
    case VectorTrans(x) =>
      gpuBlockSizeX = "%s.length".format(quote(x))
      stream.println(addTab()+"if( idxX < %s.length ) {".format(quote(x)))
      tabWidth += 1
      stream.println(addTab()+"%s.update(idxX,%s.apply(idxX));".format(quote(sym),quote(x)))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitVectorAlloc(sym,"%s.length".format(quote(x)),"!%s.isRow".format(quote(x)))

    case VectorRepmat(x,i,j) =>
      gpuBlockSizeX = "%s.length * %s * %s".format(quote(x),quote(i),quote(j))
      stream.println(addTab()+"if( idxX < %s.length*%s*%s ) {".format(quote(x),quote(i),quote(j)))
      //tabWidth += 1
      //stream.println(addTab()+"for(int i=0;i<%s;i++) {".format(quote(i)))
      tabWidth += 1
	  stream.println(addTab()+"int i = idxX / (%s.length * %s);".format(quote(x),quote(j)))
	  stream.println(addTab()+"int j = idxX % " + "(%s.length * %s);".format(quote(x),quote(j)))
      stream.println(addTab()+"%s.update(i,j,%s.apply(%s));".format(quote(sym),quote(x),"j%"+quote(x)+".length"))
      //tabWidth -= 1
      //stream.println(addTab()+"}")
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s.length*%s".format(quote(x),quote(i)),"%s.length*%s".format(quote(x),quote(j)))

    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenVectorOps extends BaseGenVectorOps with CGenFat {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case VectorObjectZeros(len) =>
      stream.println("%s *%s_data = malloc(sizeof(%s)*%s);".format(remap(sym.Type.typeArguments(0)),quote(sym),remap(sym.Type.typeArguments(0)),quote(len)))
      stream.println("memset(%s_data,0,sizeof(%s)*%s);".format(quote(sym),remap(sym.Type.typeArguments(0)),quote(len)))
      stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
      stream.println("%s.length = %s;".format(quote(sym),quote(len)))
      stream.println("%s.isRow = true;".format(quote(sym)))
      stream.println("%s.data = %s_data;".format(quote(sym),quote(sym)))
    case VectorNew(len,isRow) =>
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

