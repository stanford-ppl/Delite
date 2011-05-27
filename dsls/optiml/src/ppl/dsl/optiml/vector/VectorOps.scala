package ppl.dsl.optiml.vector

import ppl.dsl.optiml.datastruct.CudaGenDataStruct
import ppl.dsl.optiml.datastruct.scala._
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import ppl.dsl.optiml.{OptiMLExp, OptiML}

trait VectorOps extends DSLType with Variables {
  this: OptiML =>

  object Vector {
    def apply[A:Manifest](len: Int, isRow: Boolean) = vector_obj_new(unit(len), unit(isRow)) // needed to resolve ambiguities
    def apply[A](len: Rep[Int], isRow: Rep[Boolean])(implicit mA: Manifest[A], o: Overloaded1) = vector_obj_new(len, isRow)
//    def apply[A:Manifest](xs: A*) = {
//      val out = vector_obj_new[A](unit(0),unit(true))
//      // interpreted (not lifted)
//      xs.foreach { out += unit(_) }
//      out
//    }
    def apply[A](xs: Rep[A]*)(implicit mA: Manifest[A], o: Overloaded2) = {
      val out = vector_obj_new[A](unit(0),unit(true))
      // interpreted (not lifted)
      xs.foreach { out += _ }
      out.cloneL // return immutable object
    }

    def flatten[A:Manifest](pieces: Rep[Vector[Vector[A]]]) = vector_obj_flatten(pieces)
    def ones(len: Rep[Int]) = vector_obj_ones(len)
    def onesf(len: Rep[Int]) = vector_obj_onesf(len)
    def zeros(len: Rep[Int]) = vector_obj_zeros(len)
    def zerosf(len: Rep[Int]) = vector_obj_zerosf(len)
    def rand(len: Rep[Int]) = vector_obj_rand(len)
    def randf(len: Rep[Int]) = vector_obj_randf(len)
    def range(start: Rep[Int], end: Rep[Int], stride: Rep[Int] = 1, isRow: Rep[Boolean] = unit(true)) =
      vector_obj_range(start, end, stride, isRow)
    def uniform(start: Rep[Double], step_size: Rep[Double], end: Rep[Double], isRow: Rep[Boolean] = unit(true)) =
      vector_obj_uniform(start, step_size, end, isRow)
  }

  implicit def repVecToVecOps[A:Manifest](x: Rep[Vector[A]]) = new vecOpsCls(x)
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
    def mutable() = vector_mutable_clone(x)
    def pprint() = vector_pprint(x)
    def replicate(i: Rep[Int], j: Rep[Int]) = vector_repmat(x,i,j)
    def toList = vector_tolist(x)
    def mkString(sep: Rep[String] = unit("")) = vector_mkstring(x, sep)

    // data operations
    def ++(y: Rep[Vector[A]]) = vector_concatenate(x,y)
    def update(n: Rep[Int], y: Rep[A]) = vector_update(x,n,y)
    def update(i: Rep[IndexVector], y: Rep[A])(implicit o: Overloaded1) = vector_update_indices(x,i,y)
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
    def minIndex(implicit o: Ordering[A]) = vector_minindex(x)
    def max(implicit o: Ordering[A]) = vector_max(x)
    def max[B](key: Rep[A] => Rep[B])(implicit o: Ordering[B], mB: Manifest[B]) = vector_max_key(x, key)
    def maxIndex(implicit o: Ordering[A]) = vector_maxindex(x)
    def median(implicit o: Ordering[A]) = vector_median(x)
    def :>(y: Rep[Vector[A]])(implicit o: Ordering[A]) = zip(y) { (a,b) => a > b }
    def :<(y: Rep[Vector[A]])(implicit o: Ordering[A]) = zip(y) { (a,b) => a < b }

    // bulk operations
    def map[B:Manifest](f: Rep[A] => Rep[B]) = vector_map(x,f)
    def mmap(f: Rep[A] => Rep[A]) = vector_mmap(x,f)
    def foreach(block: Rep[A] => Rep[Unit]) = vector_foreach(x, block)
    def zip[B:Manifest,R:Manifest](y: Rep[Vector[B]])(f: (Rep[A],Rep[B]) => Rep[R]) = vector_zipwith(x,y,f)
    def mzip[B:Manifest](y: Rep[Vector[B]])(f: (Rep[A],Rep[B]) => Rep[A]) = vector_mzipwith(x,y,f)
    def reduce(f: (Rep[A],Rep[A]) => Rep[A]) = vector_reduce(x,f)
    def filter(pred: Rep[A] => Rep[Boolean]) = vector_filter(x,pred)
    def find(pred: Rep[A] => Rep[Boolean]) = vector_find(x,pred)
    def count(pred: Rep[A] => Rep[Boolean]) = vector_count(x, pred)
    def flatMap[B:Manifest](f: Rep[A] => Rep[Vector[B]]) = vector_flatmap(x,f)
    def partition(pred: Rep[A] => Rep[Boolean]) = vector_partition(x,pred)
  }

  def __equal[A](a: Rep[Vector[A]], b: Rep[Vector[A]])(implicit o: Overloaded1, mA: Manifest[A]): Rep[Boolean] = vector_equals(a,b)
  def __equal[A](a: Rep[Vector[A]], b: Var[Vector[A]])(implicit o: Overloaded2, mA: Manifest[A]): Rep[Boolean] = vector_equals(a,b)
  def __equal[A](a: Var[Vector[A]], b: Rep[Vector[A]])(implicit o: Overloaded3, mA: Manifest[A]): Rep[Boolean] = vector_equals(a,b)
  def __equal[A](a: Var[Vector[A]], b: Var[Vector[A]])(implicit o: Overloaded4, mA: Manifest[A]): Rep[Boolean] = vector_equals(a,b)

/*
  class vecOpsClsMutable[A:Manifest](vx: Var[Vector[A]]) extends vecOpsCls[A](readVar(vx)) {
    // ...
  }
*/  

  def NilV[A](implicit mA: Manifest[A]): Rep[Vector[A]] = mA match {
    case Manifest.Double => vector_nil_double.asInstanceOfL[Vector[A]]
    case Manifest.Float => vector_nil_float.asInstanceOfL[Vector[A]]
    case Manifest.Int => vector_nil_int.asInstanceOfL[Vector[A]]
    case _ => throw new IllegalArgumentException("No NilVector exists of type " + mA)
  }

  def ZeroV[A](length: Rep[Int], isRow: Rep[Boolean] = unit(true))(implicit mA: Manifest[A]): Rep[Vector[A]] = mA match {
    case Manifest.Double => vector_zero_double(length, isRow).asInstanceOfL[Vector[A]]
    case Manifest.Float => vector_zero_float(length, isRow).asInstanceOfL[Vector[A]]
    case Manifest.Int => vector_zero_int(length, isRow).asInstanceOfL[Vector[A]]
    case _ => throw new IllegalArgumentException("No ZeroVector exists of type " + mA)
  }

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

  def vector_equals[A:Manifest](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Boolean]
  def vector_trans[A:Manifest](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_mutable_trans[A:Manifest](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_clone[A:Manifest](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_mutable_clone[A:Manifest](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_pprint[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
  def vector_repmat[A:Manifest](x: Rep[Vector[A]], i: Rep[Int], j: Rep[Int]): Rep[Matrix[A]]
  def vector_tolist[A:Manifest](x: Rep[Vector[A]]): Rep[List[A]]
  def vector_mkstring[A:Manifest](x: Rep[Vector[A]], sep: Rep[String]): Rep[String]

  def vector_concatenate[A:Manifest](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_update[A:Manifest](x: Rep[Vector[A]], n: Rep[Int], y: Rep[A]): Rep[Unit]
  def vector_update_indices[A:Manifest](x: Rep[Vector[A]], i: Rep[IndexVector], y: Rep[A]): Rep[Unit]
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
  def vector_minindex[A:Manifest:Ordering](x: Rep[Vector[A]]): Rep[Int]
  def vector_max[A:Manifest:Ordering](x: Rep[Vector[A]]): Rep[A]
  def vector_max_key[A:Manifest,B:Manifest:Ordering](x: Rep[Vector[A]], key: Rep[A] => Rep[B]): Rep[A]
  def vector_maxindex[A:Manifest:Ordering](x: Rep[Vector[A]]): Rep[Int]
  def vector_median[A:Manifest:Ordering](x: Rep[Vector[A]]): Rep[A]

  def vector_map[A:Manifest,B:Manifest](x: Rep[Vector[A]], f: Rep[A] => Rep[B]): Rep[Vector[B]]
  def vector_mmap[A:Manifest](x: Rep[Vector[A]], f: Rep[A] => Rep[A]): Rep[Vector[A]]
  def vector_foreach[A:Manifest](x: Rep[Vector[A]], block: Rep[A] => Rep[Unit]): Rep[Unit]
  def vector_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Rep[Vector[A]], y: Rep[Vector[B]], f: (Rep[A],Rep[B]) => Rep[R]): Rep[Vector[R]]
  def vector_mzipwith[A:Manifest,B:Manifest](x: Rep[Vector[A]], y: Rep[Vector[B]], f: (Rep[A],Rep[B]) => Rep[A]): Rep[Vector[A]]
  def vector_reduce[A:Manifest](x: Rep[Vector[A]], f: (Rep[A],Rep[A]) => Rep[A]): Rep[A]
  def vector_filter[A:Manifest](x: Rep[Vector[A]], pred: Rep[A] => Rep[Boolean]): Rep[Vector[A]]
  def vector_find[A:Manifest](x: Rep[Vector[A]], pred: Rep[A] => Rep[Boolean]): Rep[IndexVector]
  def vector_count[A:Manifest](x: Rep[Vector[A]], pred: Rep[A] => Rep[Boolean]): Rep[Int]
  def vector_flatmap[A:Manifest,B:Manifest](x: Rep[Vector[A]], f: Rep[A] => Rep[Vector[B]]): Rep[Vector[B]]
  def vector_partition[A:Manifest](x: Rep[Vector[A]], pred: Rep[A] => Rep[Boolean]): (Rep[Vector[A]], Rep[Vector[A]])

  // other defs
  def vector_nil_double: Rep[Vector[Double]]
  def vector_nil_float: Rep[Vector[Float]]
  def vector_nil_int: Rep[Vector[Int]]
  def vector_zero_double(length: Rep[Int], isRow: Rep[Boolean]): Rep[Vector[Double]]
  def vector_zero_float(length: Rep[Int], isRow: Rep[Boolean]): Rep[Vector[Float]]
  def vector_zero_int(length: Rep[Int], isRow: Rep[Boolean]): Rep[Vector[Int]]
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

  def reflectPure[A:Manifest](x: Def[A]): Exp[A] = toAtom(x) // TODO: just to make refactoring easier in case we want to change to reflectSomething


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
  case class VectorNilDouble() extends Def[Vector[Double]]
  case class VectorNilFloat() extends Def[Vector[Float]]
  case class VectorNilInt() extends Def[Vector[Int]]
  case class VectorZeroDouble(length: Exp[Int], isRow: Exp[Boolean]) extends Def[Vector[Double]]
  case class VectorZeroFloat(length: Exp[Int], isRow: Exp[Boolean]) extends Def[Vector[Float]]
  case class VectorZeroInt(length: Exp[Int], isRow: Exp[Boolean]) extends Def[Vector[Int]]
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
  // TODO: right now we just use the underlying data structure sort, but we should implement our own
  // fast parallel sort with delite ops
  case class VectorSort[A:Manifest:Ordering](x: Exp[Vector[A]]) extends Def[Vector[A]]
  case class VectorToList[A:Manifest](x: Exp[Vector[A]]) extends Def[List[A]]


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

  // this is a single task right now because of the likely early exit. should we have a delite op for this?
  case class VectorEquals[A:Manifest](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_equals_impl[A](x,y)))

  case class VectorPPrint[A](x: Exp[Vector[A]])(block: Exp[Unit]) // stupid limitation...
    extends DeliteOpSingleTask(block)
    // reifyEffects(vector_pprint_impl[A](x))

/*
  case class VectorTrans[A:Manifest](x: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_trans_impl[A](x)))
*/

  case class VectorRepmat[A:Manifest](x: Exp[Vector[A]], i: Exp[Int], j: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_repmat_impl[A](x,i,j)))

  case class VectorMkString[A:Manifest](x: Exp[Vector[A]], sep: Exp[String])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_mkstring_impl[A](x, sep)))

  case class VectorMedian[A:Manifest:Ordering](x: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_median_impl[A](x)))

//  case class VectorFilter[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean])
//    extends DeliteOpSingleTask(reifyEffectsHere(vector_filter_impl(x, pred)))

  case class VectorPartition[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_partition_impl(x, pred)))

  case class VectorContains[A:Manifest](x: Exp[Vector[A]], y: Exp[A])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_contains_impl[A](x, y)))

  case class VectorDistinct[A:Manifest](x: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_distinct_impl[A](x)))

//  case class VectorMinIndex[A:Manifest:Ordering](x: Exp[Vector[A]])
//    extends DeliteOpSingleTask(reifyEffectsHere(vector_min_index_impl[A](x)))
//
//  case class VectorMaxIndex[A:Manifest:Ordering](x: Exp[Vector[A]])
//    extends DeliteOpSingleTask(reifyEffectsHere(vector_max_index_impl[A](x)))

//  case class VectorFind[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean])
//    extends DeliteOpSingleTask(reifyEffectsHere(vector_find_impl[A](x, pred)))


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

  // contract: resulting vector has size equal to index range
  abstract class DeliteOpVectorLoop[A] extends DeliteOpLoop[Vector[A]] {
    val size: Exp[Int] //inherited
    val isRow: Exp[Boolean]
  }
  
  // it would be nice if these were traits but we could not pass the implicits
  abstract class DeliteOpVectorLoopMA[A:Manifest:Arith] extends DeliteOpVectorLoop[A] {
    def mev = manifest[A]
    def aev = implicitly[Arith[A]]
  }

  abstract class DeliteOpLoopMA[A:Manifest:Arith] extends DeliteOpLoop[A] {
    def mev = manifest[A]
    def aev = implicitly[Arith[A]]
  }
  
/* too bad traits have no constructors...
  class DeliteOpVectorLoopMirrored[A](f: Transformer, o: DeliteOpVectorLoop[A])   // TODO!
    extends DeliteOpVectorLoop[A] with DeliteOpLoopMirrored[Vector[A]](f, o) {
    
    val isRow = f(o.size)
  }
*/  

  case class VectorUpdateIndices[A:Manifest](x: Exp[Vector[A]], in: Exp[IndexVector], y: Exp[A])
    extends DeliteOpForeach[Int,Vector] {

    val i = fresh[Int]
    val sync = reifyEffects(List())

    val v = fresh[Int]
    val func = reifyEffects { x(v) = y }
  }

  case class VectorTrans[A:Manifest](in: Exp[Vector[A]])
    extends DeliteOpVectorLoop[A] {

    val size = in.length
    val isRow = !in.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = DeliteCollectElem[A,Vector[A]](
      alloc = reifyEffects(Vector[A](size, isRow)),
      func = reifyEffects(in(v))
    )
  }

  abstract case class VectorPlus[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpVectorLoopMA[A]

  class VectorPlusFresh[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends VectorPlus(inA, inB) {

    val size = inA.length
    val isRow = inA.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = DeliteCollectElem[A,Vector[A]](
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

  abstract case class VectorMinus[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpVectorLoopMA[A] 
  
  class VectorMinusFresh[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends VectorMinus(inA, inB) {

    val size = inA.length
    val isRow = inA.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = DeliteCollectElem[A,Vector[A]](
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
    val func = reifyEffects(v._1 - v._2)
  }

  abstract case class VectorTimes[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]]) 
    extends DeliteOpVectorLoopMA[A]
  
  class VectorTimesFresh[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]]) extends VectorTimes(inA, inB) {
    val size = inA.length
    val isRow = inA.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = DeliteCollectElem[A,Vector[A]](
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
    val func = reifyEffects(conv(v._1) * v._2)
  }

  abstract case class VectorTimesScalar[A:Manifest:Arith](in: Exp[Vector[A]], y: Exp[A]) 
    extends DeliteOpVectorLoopMA[A]
    
  class VectorTimesScalarFresh[A:Manifest:Arith](in: Exp[Vector[A]], y: Exp[A]) extends VectorTimesScalar[A](in,y) {
    val size = in.length
    val isRow = in.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = DeliteCollectElem[A,Vector[A]](
      alloc = reifyEffects(Vector[A](size, isRow)),
      func = reifyEffects(in(v) * y)
    )
  }

  //TR TODO -- not used currently
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
    val body: Def[Vector[A]] = new DeliteCollectElem[A,Vector[A]](
      alloc = reifyEffects(Vector[A](size, isRow)),
      func = reifyEffects(inA(v) / inB(v))
    )
  }

  case class VectorDivideScalar[A:Manifest:Arith](in: Exp[Vector[A]], y: Exp[A])
    extends DeliteOpVectorLoop[A] {

    val size = in.length
    val isRow = in.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = new DeliteCollectElem[A,Vector[A]](
      alloc = reifyEffects(Vector[A](size, isRow)),
      func = reifyEffects(in(v) / y)
    )
  }

  abstract case class VectorSum[A:Manifest:Arith](in: Exp[Vector[A]]) 
    extends DeliteOpLoopMA[A]

  class VectorSumFresh[A:Manifest:Arith](in: Exp[Vector[A]]) 
    extends VectorSum[A](in) {

    val size = in.length
    val v = fresh[Int]
    private[this] val rV = (fresh[A],fresh[A])
    val body: Def[A] = DeliteReduceElem[A](
      func = reifyEffects(in(v)),
      zero = unit(0.asInstanceOf[A]), // should get zero from Arith!
      rV = rV,
      rFunc = reifyEffects(rV._1 + rV._2)
    )
  }

  abstract case class VectorAbs[A:Manifest:Arith](in: Exp[Vector[A]])
    extends DeliteOpVectorLoopMA[A]
  
  class VectorAbsFresh[A:Manifest:Arith](in: Exp[Vector[A]])
    extends VectorAbs[A](in) {

    val size = in.length
    val isRow = in.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = new DeliteCollectElem[A,Vector[A]](
      alloc = reifyEffects(Vector[A](size, isRow)),
      func = reifyEffects(in(v).abs)
    )
  }

  abstract case class VectorExp[A:Manifest:Arith](in: Exp[Vector[A]])
    extends DeliteOpVectorLoopMA[A]
  
  class VectorExpFresh[A:Manifest:Arith](in: Exp[Vector[A]])
    extends VectorExp[A](in) {

    val size = in.length
    val isRow = in.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = new DeliteCollectElem[A,Vector[A]](
      alloc = reifyEffects(Vector[A](size, isRow)),
      func = reifyEffects(in(v).exp)
    )
  }


  def getMaxValue[A:Manifest]: Exp[A] = manifest[A] match { // TODO: move somewhere else (Arith? Ordering?)
    case Manifest.Double => unit(scala.Double.MaxValue).asInstanceOf[Exp[A]]
    case Manifest.Int => unit(scala.Int.MaxValue).asInstanceOf[Exp[A]]
  }

  def getMinValue[A:Manifest]: Exp[A] = manifest[A] match { // TODO: move somewhere else (Arith? Ordering?)
    case Manifest.Double => unit(scala.Double.MinValue).asInstanceOf[Exp[A]]
    case Manifest.Int => unit(scala.Int.MinValue).asInstanceOf[Exp[A]]
  }


  case class VectorMin[A:Manifest:Ordering](in: Exp[Vector[A]]) 
    extends DeliteOpLoop[A] {

    val size = in.length
    val v = fresh[Int]
    private[this] val rV = (fresh[A],fresh[A])
    val body: Def[A] = DeliteReduceElem[A](
      func = reifyEffects(in(v)),
      zero = getMaxValue[A],
      rV = rV,
      rFunc = reifyEffects(if (rV._1 < rV._2) rV._1 else rV._2)
    )
  }

  case class VectorMinIndex[A:Manifest:Ordering](in: Exp[Vector[A]])
    extends DeliteOpLoop[Int] {

    val size = in.length
    val v = fresh[Int]
    private[this] val rV = (fresh[Int],fresh[Int])
    val body: Def[Int] = DeliteReduceElem[Int](
      func = reifyEffects(v),
      zero = 0, // sensible?
      rV = rV,
      rFunc = reifyEffects(if (in(rV._1) < in(rV._2)) rV._1 else rV._2)
    )
  }

  case class VectorMax[A:Manifest:Ordering](in: Exp[Vector[A]]) 
    extends DeliteOpLoop[A] {

    val size = in.length
    val v = fresh[Int]
    private[this] val rV = (fresh[A],fresh[A])
    val body: Def[A] = DeliteReduceElem[A](
      func = reifyEffects(in(v)),
      zero = getMinValue[A],
      rV = rV,
      rFunc = reifyEffects(if (rV._1 > rV._2) rV._1 else rV._2)
    )
  }

  case class VectorMaxKey[A:Manifest,B:Manifest:Ordering](in: Exp[Vector[A]], key: Exp[A] => Exp[B])
    extends DeliteOpLoop[A] {

    val size = in.length
    val v = fresh[Int]
    private[this] val rV = (fresh[A],fresh[A])
    val body: Def[A] = DeliteReduceElem[A](
      func = reifyEffects(in(v)),
      zero = getMinValue[A],
      rV = rV,
      rFunc = reifyEffects(if (key(rV._1) > key(rV._2)) rV._1 else rV._2)
    )
  }

  case class VectorMaxIndex[A:Manifest:Ordering](in: Exp[Vector[A]])
    extends DeliteOpLoop[Int] {

    val size = in.length
    val v = fresh[Int]
    private[this] val rV = (fresh[Int],fresh[Int])
    val body: Def[Int] = DeliteReduceElem[Int](
      func = reifyEffects(v),
      zero = 0, // sensible?
      rV = rV,
      rFunc = reifyEffects(if (in(rV._1) > in(rV._2)) rV._1 else rV._2)
    )
  }

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
  
  case class VectorMutableZipWith[A:Manifest,B:Manifest](inA: Exp[Vector[A]], inB: Exp[Vector[B]],
                                                             v: (Sym[A],Sym[B]), func: Exp[A])
    extends DeliteOpZipWith[A,B,A,Vector] {

    val alloc = inA
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

  // TODO: need to either use scan or express this more efficiently
  // could be expressed as a reduce of (A,B) with a function from (A,B) => A;
  // in this case, it's (Vector[A],A) => Vector[A]. this is a more general form of reduction than we can express currently.
  // reduction function should be: if (pred(b)) a += b else a
  /*
  case class VectorFilter[A:Manifest](in: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean])
    extends DeliteOpMapReduce[A,Vector[A],Vector] {

    val mV = fresh[A]
    val map = reifyEffects { if (pred(mV)) Vector[A](mV) else NilV[A] }
    val rV = (fresh[Vector[A]],fresh[Vector[A]])
    val reduce = reifyEffects(rV._1 ++ rV._2)
  }
  */
  
  /*  functional, but slow
  case class VectorFind[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean])
    extends DeliteOpMapReduce[Int,Vector[Int],Vector] {

    val in = (0::x.length)
    val mV = fresh[Int]
    val map = reifyEffects { if (pred(x(mV))) Vector[Int](mV) else NilV[Int] }
    val rV = (fresh[Vector[Int]],fresh[Vector[Int]])
    val reduce = reifyEffects(rV._1 ++ rV._2)
  }
  */
  
  /*
  case class VectorCount[A:Manifest](in: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean])
    extends DeliteOpMapReduce[A,Int,Vector] {

    val mV = fresh[A]
    val map = reifyEffects { if (pred(mV)) 1 else 0 }
    val rV = (fresh[Int],fresh[Int])
    val reduce = reifyEffects(rV._1 + rV._2)
  }
  */

  abstract case class VectorFilter[A:Manifest](in: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) extends DeliteOpLoop[Vector[A]] {
    // does not extend DeliteOpVectorLoop because result size is not known
    def mev = manifest[A]
    val isRow: Exp[Boolean]
  }
  
  class VectorFilterFresh[A:Manifest](in: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) extends VectorFilter[A](in, pred) {
    val size = in.length
    val isRow = in.isRow
    val v = fresh[Int]
    val body: Def[Vector[A]] = new DeliteCollectElem[A,Vector[A]](
      alloc = reifyEffects(Vector(0,isRow)),
      func = reifyEffects(in(v)),
      cond = reifyEffects(pred(in(v)))::Nil
    )
  }

  abstract case class VectorFind[A:Manifest](in: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) extends DeliteOpLoop[IndexVector] {
    def mev = manifest[A]
  }

  class VectorFindFresh[A:Manifest](in: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) extends VectorFind[A](in, pred) {
    val size = in.length
    val v = fresh[Int]
    val body: Def[IndexVector] = new DeliteCollectElem[Int,IndexVector](
      alloc = reifyEffects(IndexVector(0)),
      func = reifyEffects(v),
      cond = reifyEffects(pred(in(v)))::Nil
    )
  }

  abstract case class VectorCount[A:Manifest](in: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) extends DeliteOpLoop[Int] {
    def mev = manifest[A]
  }

  class VectorCountFresh[A:Manifest](in: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) extends VectorCount[A](in,pred) {
    val size = in.length
    val v = fresh[Int]
    private[this] val rV = (fresh[Int],fresh[Int])
    val body: Def[Int] = DeliteReduceElem[Int](
      func = reifyEffects(unit(1)),
      cond = reifyEffects(pred(in(v)))::Nil,
      zero = unit(0),
      rV = rV,
      rFunc = reifyEffects(rV._1 + rV._2)
    )
  }




  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case VectorApply(x, n) => vector_apply(f(x), f(n))
    case VectorLength(x) => vector_length(f(x))
    case VectorIsRow(x) => vector_isRow(f(x))
    case VectorTrans(x) => vector_trans(f(x))
    // FIXME: VectorSum might not actually be triggered <-- still true??
    case e@VectorPlus(x,y) => toAtom(new VectorPlus(f(x),f(y))(e.mev,e.aev) {
      val size = f(e.size); val isRow = f(e.isRow); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case e@VectorMinus(x,y) => toAtom(new VectorMinus(f(x),f(y))(e.mev,e.aev) { 
      val size = f(e.size); val isRow = f(e.isRow); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case e@VectorTimes(x,y) => toAtom(new VectorTimes(f(x),f(y))(e.mev,e.aev) { 
      val size = f(e.size); val isRow = f(e.isRow); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case e@VectorTimesScalar(x,y) => toAtom(new VectorTimesScalar(f(x),f(y))(e.mev,e.aev) { 
      val size = f(e.size); val isRow = f(e.isRow); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case e@VectorSum(x) => toAtom(new VectorSum(f(x))(e.mev,e.aev) { 
      val size = f(e.size); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case e@VectorAbs(x) => toAtom(new VectorAbs(f(x))(e.mev,e.aev) { 
      val size = f(e.size); val isRow = f(e.isRow); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case e@VectorExp(x) => toAtom(new VectorExp(f(x))(e.mev,e.aev) { 
      val size = f(e.size); val isRow = f(e.isRow); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case e@VectorFilter(x,p) => toAtom(new VectorFilter(f(x),p)(e.mev) { 
      val size = f(e.size); val isRow = f(e.isRow); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case e@VectorFind(x,p) => toAtom(new VectorFind(f(x),p)(e.mev) { 
      val size = f(e.size); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case e@VectorCount(x,p) => toAtom(new VectorCount(f(x),p)(e.mev) { 
      val size = f(e.size); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case Reflect(e@VectorPPrint(x), u, es) => reflectMirrored(Reflect(VectorPPrint(f(x))(f(e.block)), mapOver(f,u), f(es)))
    // below are read/write effects TODO: find a general approach to treating them!!!!
    case Reflect(VectorApply(l,r), u, es) => reflectMirrored(Reflect(VectorApply(f(l),f(r)), mapOver(f,u), f(es)))
    case Reflect(VectorLength(x), u, es) => reflectMirrored(Reflect(VectorLength(f(x)), mapOver(f,u), f(es)))
    case Reflect(VectorIsRow(x), u, es) => reflectMirrored(Reflect(VectorIsRow(f(x)), mapOver(f,u), f(es)))
    case Reflect(VectorForeach(a,b,c), u, es) => reflectMirrored(Reflect(VectorForeach(f(a),f(b).asInstanceOf[Sym[Int]],f(c)), mapOver(f,u), f(es)))
    // FIXME: problem with VectorTimes: it's actually a loop and if it is reflected it means a.length will also reflect and we have no context here!!!
    case Reflect(e2@VectorTimes(a,b), u, es) => error(unit("we'd rather not mirror ") + unit(e)); //reflectMirrored(Reflect(VectorTimes(f(a),f(b))(e.mev,e.aev), Read(f onlySyms rs), f(es)))
    case Reflect(VectorUpdate(l,i,r), u, es) => reflectMirrored(Reflect(VectorUpdate(f(l),f(i),f(r)), mapOver(f,u), f(es)))
    // allocations TODO: generalize
    case Reflect(VectorObjectZeros(x), u, es) => reflectMirrored(Reflect(VectorObjectZeros(f(x)), mapOver(f,u), f(es)))
    case Reflect(VectorObjectRange(s,e,d,r), u, es) => reflectMirrored(Reflect(VectorObjectRange(f(s),f(e),f(d),f(r)), mapOver(f,u), f(es)))
    case Reflect(e@VectorNew(l,r), u, es) => reflectMirrored(Reflect(VectorNew(f(l),f(r))(e.mV), mapOver(f,u), f(es)))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??



  /////////////////////
  // aliases and sharing

  // TODO: precise sharing info for other IR types (default is conservative)
  
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case VectorApply(a,i) => Nil
    case VectorUpdate(a,i,x) => Nil           // syms(a) <-- any use to return a?
    case VectorUpdateIndices(a,is,x) => Nil   // syms(a) <-- any use to return a?
    case VectorInsert(a,i,x) => Nil           // syms(a) <-- any use to return a?
    case VectorRepmat(a,i,j) => Nil
    case VectorClone(a) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case VectorApply(a,i) => Nil
    case VectorUpdate(a,i,x) => syms(x)
    case VectorUpdateIndices(a,is,x) => syms(x)
    case VectorInsert(a,i,x) => syms(x)
    case VectorRepmat(a,i,j) => Nil
    case VectorClone(a) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case VectorApply(a,i) => syms(a)
    case VectorUpdate(a,i,x) => Nil
    case VectorUpdateIndices(a,is,x) => Nil
    case VectorInsert(a,i,x) => Nil
    case VectorRepmat(a,i,j) => Nil
    case VectorClone(a) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case VectorApply(a,i) => Nil
    case VectorUpdate(a,i,x) => syms(a)
    case VectorUpdateIndices(a,is,x) => syms(a)
    case VectorInsert(a,i,x) => syms(a)
    case VectorRepmat(a,i,j) => syms(a)
    case VectorClone(a) => syms(a)
    case _ => super.copySyms(e)
  }



  /////////////////////
  // object interface

//  def vector_obj_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) = reflectEffect(VectorObjectNew[A](len, isRow))
  def vector_obj_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) = reflectMutable(VectorNew[A](len, isRow)(manifest[VectorImpl[A]])) //XXX
  def vector_obj_fromseq[A:Manifest](xs: Exp[Seq[A]]) = reflectPure(VectorObjectFromSeq(xs)) //XXX
  def vector_obj_ones(len: Exp[Int]) = reflectPure(VectorObjectOnes(len))
  def vector_obj_onesf(len: Exp[Int]) = reflectPure(VectorObjectOnesF(len))
  def vector_obj_zeros(len: Exp[Int]) = reflectPure(VectorObjectZeros(len))
  def vector_obj_zerosf(len: Exp[Int]) = reflectPure(VectorObjectZerosF(len))
  def vector_obj_rand(len: Exp[Int]) = reflectPure(VectorObjectRand(len))
  def vector_obj_randf(len: Exp[Int]) = reflectPure(VectorObjectRandF(len))
  def vector_obj_range(start: Exp[Int], end: Exp[Int], stride: Exp[Int], isRow: Exp[Boolean]) = reflectPure(VectorObjectRange(start, end, stride, isRow))
  def vector_obj_uniform(start: Exp[Double], step_size: Exp[Double], end: Exp[Double], isRow: Exp[Boolean]) = reflectPure(VectorObjectUniform(start, step_size, end, isRow))
  def vector_obj_flatten[A:Manifest](pieces: Exp[Vector[Vector[A]]]) = reflectPure(VectorObjectFlatten(pieces))


  /////////////////////
  // class interface

  def vector_length[A:Manifest](x: Exp[Vector[A]]) = reflectPure(VectorLength(x))
  def vector_isRow[A:Manifest](x: Exp[Vector[A]]) = reflectPure(VectorIsRow(x))
  def vector_apply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) = reflectPure(VectorApply(x, n))
  def vector_slice[A:Manifest](x: Exp[Vector[A]], start: Exp[Int], end: Exp[Int]) = reflectPure(VectorSlice(x, start, end))
  def vector_contains[A:Manifest](x: Exp[Vector[A]], y: Exp[A]) = reflectPure(VectorContains(x, y))
  def vector_distinct[A:Manifest](x: Exp[Vector[A]]) = reflectPure(VectorDistinct(x))

  def vector_equals[A:Manifest](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectPure(VectorEquals(x,y))
  def vector_trans[A:Manifest](x: Exp[Vector[A]]) = reflectPure(VectorTrans(x))
  def vector_mutable_trans[A:Manifest](x: Exp[Vector[A]]) = reflectWrite(x)(VectorMutableTrans(x))
  def vector_clone[A:Manifest](x: Exp[Vector[A]]) = reflectPure(VectorClone(x))
  def vector_mutable_clone[A:Manifest](x: Exp[Vector[A]]) = reflectMutable(VectorClone(x))
  def vector_repmat[A:Manifest](x: Exp[Vector[A]], i: Exp[Int], j: Exp[Int]) = reflectPure(VectorRepmat(x,i,j))
  def vector_tolist[A:Manifest](x: Exp[Vector[A]]) = reflectPure(VectorToList(x))
  def vector_mkstring[A:Manifest](x: Exp[Vector[A]], sep: Exp[String]) = reflectPure(VectorMkString(x, sep))
  def vector_pprint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(VectorPPrint(x)(reifyEffectsHere(vector_pprint_impl[A](x))))

  def vector_concatenate[A:Manifest](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectPure(VectorConcatenate(x,y))
  def vector_update[A:Manifest](x: Exp[Vector[A]], n: Exp[Int], y: Exp[A]) = reflectWrite(x)(VectorUpdate(x, n, y))
  def vector_update_indices[A:Manifest](x: Exp[Vector[A]], i: Exp[IndexVector], y: Exp[A]) = reflectWrite(x)(VectorUpdateIndices(x,i,y))
  def vector_copyfrom[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[Vector[A]]) = reflectWrite(x)(VectorCopyFrom(x, pos, y))
  def vector_insert[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[A]) = reflectWrite(x)(VectorInsert(x, pos, y))
  def vector_insertall[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], y: Exp[Vector[A]]) = reflectWrite(x)(VectorInsertAll(x, pos, y))
  def vector_removeall[A:Manifest](x: Exp[Vector[A]], pos: Exp[Int], len: Exp[Int]) = reflectWrite(x)(VectorRemoveAll(x, pos, len))
  def vector_trim[A:Manifest](x: Exp[Vector[A]]) = reflectWrite(x)(VectorTrim(x))
  def vector_clear[A:Manifest](x: Exp[Vector[A]]) = reflectWrite(x)(VectorClear(x))

  def vector_plus[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectPure(new VectorPlusFresh(x,y))
  def vector_plus_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = reflectPure(VectorPlusScalar(x,y))
  def vector_plusequals[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectWrite(x)(VectorPlusEquals(x,y))
  def vector_minus[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectPure(new VectorMinusFresh(x,y))
  def vector_minus_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = reflectPure(VectorMinusScalar(x,y))
  def vector_minusequals[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectWrite(x)(VectorMinusEquals(x, y))
  def vector_times[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectPure(new VectorTimesFresh(x,y))
  def vector_times_withconvert[A:Manifest:Arith,B:Manifest](x: Exp[Vector[A]], y: Exp[Vector[B]], conv: Exp[B] => Exp[A]) = reflectPure(VectorTimesWithConvert(x,y,conv)) // TODO: de-hoas
  def vector_times_withconvertright[A:Manifest,B:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[B]], conv: Exp[A] => Exp[B]) = reflectPure(VectorTimesWithConvertRight(x,y,conv))
  def vector_times_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = reflectPure(new VectorTimesScalarFresh(x,y))
  def vector_times_matrix[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Matrix[A]]) = reflectPure(VectorTimesMatrix(x,y))
  def vector_outer[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectPure(VectorOuter(x,y))
  def vector_dot_product[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectPure(VectorDotProduct(x,y))
  def vector_divide[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectPure(VectorDivide(x,y))
  def vector_divide_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = reflectPure(VectorDivideScalar(x,y))
  def vector_sum[A:Manifest:Arith](x: Exp[Vector[A]]) = reflectPure(new VectorSumFresh(x))
  def vector_abs[A:Manifest:Arith](x: Exp[Vector[A]]) = reflectPure(new VectorAbsFresh(x))
  def vector_exp[A:Manifest:Arith](x: Exp[Vector[A]]) = reflectPure(new VectorExpFresh(x))

  def vector_sort[A:Manifest:Ordering](x: Exp[Vector[A]]) = reflectPure(VectorSort(x))
  def vector_min[A:Manifest:Ordering](x: Exp[Vector[A]]) = reflectPure(VectorMin(x))
  def vector_minindex[A:Manifest:Ordering](x: Exp[Vector[A]]) = reflectPure(VectorMinIndex(x))
  def vector_max[A:Manifest:Ordering](x: Exp[Vector[A]]) = reflectPure(VectorMax(x))
  def vector_maxindex[A:Manifest:Ordering](x: Exp[Vector[A]]) = reflectPure(VectorMaxIndex(x))
  def vector_median[A:Manifest:Ordering](x: Exp[Vector[A]]) = reflectPure(VectorMedian(x))
  def vector_max_key[A:Manifest,B:Manifest:Ordering](x: Exp[Vector[A]], key: Exp[A] => Exp[B]) = reflectPure(VectorMaxKey(x, key))

  def vector_map[A:Manifest,B:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[B]) = {
    val v = fresh[A]
    val func = reifyEffects(f(v))
    reflectPure(VectorMap(x, v, func)) // TODO: effect if func effectful!
  }
  def vector_mmap[A:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[A]) = {
    val v = fresh[A]
    val func = reifyEffects(f(v))
    reflectWrite(x)(VectorMutableMap(x, v, func))  // TODO: effect if func effectful!
  }
  def vector_foreach[A:Manifest](x: Exp[Vector[A]], block: Exp[A] => Exp[Unit]) = {
    val v = fresh[A]
    val func = reifyEffects(block(v))
    reflectEffect(VectorForeach(x, v, func))  // TODO: read sym??
  }
  def vector_zipwith[A:Manifest,B:Manifest,R:Manifest](x: Exp[Vector[A]], y: Exp[Vector[B]], f: (Exp[A],Exp[B]) => Exp[R]) = {
    val v = (fresh[A], fresh[B])
    val func = reifyEffects(f(v._1,v._2))
    reflectPure(VectorZipWith(x, y, v, func))
  }
  def vector_mzipwith[A:Manifest,B:Manifest](x: Exp[Vector[A]], y: Exp[Vector[B]], f: (Exp[A],Exp[B]) => Exp[A]) = {
    val v = (fresh[A], fresh[B])
    val func = reifyEffects(f(v._1,v._2))
    reflectWrite(x)(VectorMutableZipWith(x, y, v, func))
  }
  def vector_reduce[A:Manifest](x: Exp[Vector[A]], f: (Exp[A],Exp[A]) => Exp[A]) = {
    val v = (fresh[A],fresh[A])
    val func = reifyEffects(f(v._1, v._2))
    reflectPure(VectorReduce(x, v, func))
  }

  def vector_filter[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) = reflectPure(new VectorFilterFresh(x, pred))
  def vector_find[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) = reflectPure(new VectorFindFresh(x, pred))//IndexVector(reflectPure(VectorFind(x, pred)))
  def vector_count[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) = reflectPure(new VectorCountFresh(x, pred))

  def vector_flatmap[A:Manifest,B:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[Vector[B]]) = {
    val v = fresh[A]
    val func = reifyEffects(f(v))
    reflectPure(VectorFlatMap(x, v, func))
  }

  def vector_partition[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) = t2(reflectPure(VectorPartition(x, pred)))

  def vector_nil_double = VectorNilDouble()
  def vector_nil_float = VectorNilFloat()
  def vector_nil_int = VectorNilInt()
  def vector_zero_double(length: Exp[Int], isRow: Exp[Boolean]) = VectorZeroDouble(length, isRow)
  def vector_zero_float(length: Exp[Int], isRow: Exp[Boolean]) = VectorZeroFloat(length, isRow)
  def vector_zero_int(length: Exp[Int], isRow: Exp[Boolean]) = VectorZeroInt(length, isRow)
}

/**
 * Optimizations for composite VectorOps operations.
 */

trait VectorOpsExpOpt extends VectorOpsExp {
  this: VectorImplOps with OptiMLExp =>

  override def vector_equals[A:Manifest](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    case (a,b) if (a == b) => unit(true) // same symbol
    case _ => super.vector_equals(x,y)
  }

  override def vector_plus[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    // (TB + TD) == T(B + D)
    case (Def(VectorTimes(a, b)), Def(VectorTimes(c, d))) if (a == c) => vector_times[A](a.asInstanceOf[Exp[Vector[A]]], vector_plus[A](b.asInstanceOf[Exp[Vector[A]]],d.asInstanceOf[Exp[Vector[A]]]))
    // ...
    case _ => super.vector_plus(x, y)
  }

  override def vector_plusequals[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    // remove runtime check on zero vector being same length as argument
    case (a, Def(VectorObjectZeros(len))) => a
    //case (Def(VectorObjectZeros(len)), b) => b  // this is unsafe because we lose the effectful operation (e.g. accumulation)
    case _ => super.vector_plusequals(x,y)
  }

  override def vector_times[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    case _ => super.vector_times(x, y)
  }

  override def vector_mutable_clone[A:Manifest](x: Exp[Vector[A]]) = x match {
		case Def(d@VectorNew(len, isRow)) => reflectMutable(d.asInstanceOf[Def[Vector[A]]])
    case Def(d@VectorObjectFromSeq(xs)) => reflectMutable(d.asInstanceOf[Def[Vector[A]]])		
    case Def(d@VectorObjectZeros(len)) => reflectMutable(d.asInstanceOf[Def[Vector[A]]])
    case Def(d@VectorObjectZerosF(len)) => reflectMutable(d.asInstanceOf[Def[Vector[A]]])
    case Def(d@VectorObjectOnes(len)) => reflectMutable(d.asInstanceOf[Def[Vector[A]]])
    case Def(d@VectorObjectOnesF(len)) => reflectMutable(d.asInstanceOf[Def[Vector[A]]])
    case Def(d@VectorObjectRand(len)) => reflectMutable(d.asInstanceOf[Def[Vector[A]]])
    case Def(d@VectorObjectRandF(len)) => reflectMutable(d.asInstanceOf[Def[Vector[A]]])
    case _ => super.vector_mutable_clone(x)
  }

  // these are essential for fusing:

  override def vector_length[A:Manifest](x: Exp[Vector[A]]) = x match {
    case Def(Reflect(e @ VectorTimes(_,_), _,_)) => e.asInstanceOf[DeliteOpVectorLoop[A]].size // FIXME: in general this is unsafe, but hey...
    case Def(Reflect(e @ VectorObjectZeros(l), _,_)) => l // FIXME: in general this is unsafe, but hey...
    case Def(Reflect(e @ VectorClone(a), _,_)) => vector_length(a) // FIXME: in general this is unsafe, but hey...
    case Def(e: DeliteOpVectorLoop[A]) => e.size
    case Def(VectorObjectZeros(l)) => l
    case Def(VectorClone(a)) => vector_length(a)
    case Def(VectorObjectRange(s,e,d,r)) => (e - s + d - 1) / d
    case Def(MatrixVView(x, start, stride, l, r)) => l
    case Def(MatrixGetRow(x,i)) => x.numCols
    case Def(StreamChunkRow(x, i, offset)) => x.numCols
    case Def(StreamChunkRowFusable(x, i, offset)) => x.numCols // necessary, it's not a DeliteVectorLoop
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
    case Def(MatrixGetRow(x, i)) => matrix_apply(x,i,n)
    case Def(StreamChunkRow(x, i, offset)) => stream_chunk_elem(x,i,n)
    //case Def(StreamChunkRowFusable(x, i, offset)) => stream_chunk_elem(x,i,n) <-- enabling this will remove the computation altogether
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

}

trait ScalaGenVectorOps extends BaseGenVectorOps with ScalaGenFat {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
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
    case VectorZeroDouble(length, isRow) => emitValDef(sym, "new generated.scala.ZeroVectorDoubleImpl(" + quote(length) + ", " + quote(isRow) + ")")
    case VectorZeroFloat(length, isRow) => emitValDef(sym, "new generated.scala.ZeroVectorFloatImpl(" + quote(length) + ", " + quote(isRow) + ")")
    case VectorZeroInt(length, isRow) => emitValDef(sym, "new generated.scala.ZeroVectorIntImpl(" + quote(length) + ", " + quote(isRow) + ")")
    case VectorNilDouble() => emitValDef(sym, "generated.scala.NilVectorDoubleImpl")
    case VectorNilFloat() => emitValDef(sym, "generated.scala.NilVectorFloatImpl")
    case VectorNilInt() => emitValDef(sym, "generated.scala.NilVectorIntImpl")
    case _ => super.emitNode(sym, rhs)
  }
}


trait CudaGenVectorOps extends BaseGenVectorOps with CudaGenFat with CudaGenDataStruct {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

    // Only allow allocating primitive type Vectors
    case VectorNew(length, isRow) => {
      stream.println(addTab()+"%s *devPtr;".format(remap(sym.Type.typeArguments(0))))
      stream.println(addTab()+"DeliteCudaMalloc((void**)&devPtr,%s*sizeof(%s));".format(quote(length),remap(sym.Type.typeArguments(0))))
      stream.println(addTab()+"%s *%s = new %s(%s,%s,devPtr);".format(remap(sym.Type),quote(sym),remap(sym.Type),quote(length),quote(isRow)))
    }

    case VectorApply(x, n) =>
      emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case VectorUpdate(x,n,y) =>
      stream.println(addTab() + "%s.update(%s,%s);".format(quote(x),quote(n),quote(y)))
    case VectorLength(x)    =>
      emitValDef(sym, quote(x) + ".length")
    case VectorIsRow(x)     =>
      emitValDef(sym, quote(x) + ".isRow")

    /* Specialized CUDA code generations for DeliteOpSingleTasks */
    /*
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
        emitVectorAlloc(sym,"%s".format(quote(len)),"true",false) //needs to allocate with new symbol
        stream.println(addTab()+"if(%s < %s) {".format(currDimStr,quote(len)))
        tabWidth += 1
        stream.println(addTab()+"%s.update(%s,0);".format(quote(sym),currDimStr))
        tabWidth -= 1
        stream.println(addTab()+"}")
        currDim -= 1
    */

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
      emitMatrixAlloc(sym,"%s".format(quote(i)),"%s->length*%s".format(quote(x),quote(j)),false)
      currDim -= 1
    /*
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
      emitMatrixAlloc(sym,"%s->length".format(quote(x)),"%s->length".format(quote(x)),false)
      currDim -= 1
    */

    /* Test for using local variables */
    case VectorMinus(x,y) if(useLocalVar) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength(quote(x)+"->length")
      val outLocalVar = getNewLocalVar()
      val outIndex = if(indexMap.contains(sym)) indexMap.get(sym).get else currDimStr+"%"+quote(sym)+".size()"
      val inIndex = outIndex.replace(quote(sym),quote(x))
      //TODO: Check whether inputs are all from kernel inputs (otherwise, the recalculations need to percolate up
      stream.println(addTab()+"%s %s = %s.apply(%s) - %s.apply(%s);".format(remap(sym.Type.typeArguments(0)),outLocalVar,quote(x),inIndex,quote(y),inIndex))
      saveLocalVar(sym,outIndex,outLocalVar)
      currDim -= 1
      emitVectorAlloc(sym,"%s->length".format(quote(x)),"true",false)

    case VectorTrans(x) if(useLocalVar) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength(quote(x)+"->length")
      val outLocalVar = getNewLocalVar()
      val outIndex = if(indexMap.contains(sym)) indexMap.get(sym).get else currDimStr+"%"+quote(sym)+".size()"
      val inIndex = outIndex.replace(quote(sym),quote(x))
      if(hasLocalVar(x,inIndex)) {
        val inLocalVar = getLocalVar(x,inIndex)
        stream.println(addTab()+"%s %s = %s;".format(remap(sym.Type.typeArguments(0)),outLocalVar,inLocalVar))
      }
      else {
        val tp=findDefinition(x.asInstanceOf[Sym[_]]).get
        currDim -= 1
        indexMap.put(x,inIndex)
        emitNode(tp.sym,tp.rhs)
        indexMap.remove(x)
        currDim += 1
        val inLocalVar = getLocalVar(x,inIndex)
        stream.println(addTab()+"%s %s = %s;".format(remap(sym.Type.typeArguments(0)),outLocalVar,inLocalVar))
      }
      saveLocalVar(sym,outIndex,outLocalVar)
      currDim -= 1
      emitVectorAlloc(sym,"%s->length".format(quote(x)),"true",false)

    case VectorOuter(x,y) if(useLocalVar) =>
      currDim += 1
      val currDimStr = getCurrDimStr()
      setCurrDimLength(quote(x)+"->length*"+quote(x)+"->length")
      val outLocalVar = getNewLocalVar()
      val varX = if(hasLocalVar(x,currDimStr+"/"+quote(x)+".size()")) getLocalVar(x,currDimStr+"/"+quote(x)+".size()")
                 else {
                   val tp=findDefinition(x.asInstanceOf[Sym[_]]).get
                   indexMap.put(x,currDimStr+"/"+quote(x)+".size()")
                   currDim -= 1
                   emitNode(tp.sym,tp.rhs)
                   currDim += 1
                   indexMap.remove(x)
                   getLocalVar(x,currDimStr+"/"+quote(x)+".size()")
                 }
      val varY = if(hasLocalVar(y,currDimStr+"%"+quote(y)+".size()")) getLocalVar(y,currDimStr+"%"+quote(y)+".size()")
                 else {
                   val tp=findDefinition(y.asInstanceOf[Sym[_]]).get
                   indexMap.put(y,currDimStr+"%"+quote(y)+".size()")
                   currDim -= 1
                   emitNode(tp.sym,tp.rhs)
                   currDim += 1
                   indexMap.remove(y)
                   getLocalVar(y,currDimStr+"%"+quote(y)+".size()")
                 }
      stream.println(addTab()+"%s %s = %s * %s;".format(remap(sym.Type.typeArguments(0)),outLocalVar,varX,varY))
      saveLocalVar(sym,currDimStr,outLocalVar)
      currDim -= 1
      emitMatrixAlloc(sym,"%s->length".format(quote(x)),"%s->length".format(quote(x)),false)

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

