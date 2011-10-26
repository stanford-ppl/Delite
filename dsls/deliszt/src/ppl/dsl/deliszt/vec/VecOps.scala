package ppl.dsl.deliszt.vec

import _root_.scala.reflect.Manifest
import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
import java.io.PrintWriter
import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.datastruct.scala.MetaInteger._

import ppl.delite.framework.DSLType
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}

trait VecOps extends DSLType with Variables {
  this: DeLiszt =>

  object Vec {
    def apply[N<:IntM:Manifest:MVal,A:Manifest](i : Rep[Int])(implicit o: Overloaded1) = vec_obj_n_new[N,A](i)
    def apply[N<:IntM:Manifest:MVal,A:Manifest]() = vec_obj_n_new[N,A](MIntDepth[N])
    def apply[A:Manifest](a : Rep[A]) = vec_obj_new[_1,A](a)
		def apply[A:Manifest](a : Rep[A], b : Rep[A]) = vec_obj_new[_2,A](a,b)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A]) = vec_obj_new[_3,A](a,b,c)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A]) = vec_obj_new[_4,A](a,b,c,d)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A]) = vec_obj_new[_5,A](a,b,c,d,e)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A]) = vec_obj_new[_6,A](a,b,c,d,e,f)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A]) = vec_obj_new[_7,A](a,b,c,d,e,f,g)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A]) = vec_obj_new[_8,A](a,b,c,d,e,f,g,h)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A]) = vec_obj_new[_9,A](a,b,c,d,e,f,g,h,i)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A], j: Rep[A]) = vec_obj_new[_10,A](a,b,c,d,e,f,g,h,i,j)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A], j: Rep[A], k: Rep[A]) = vec_obj_new[_11,A](a,b,c,d,e,f,g,h,i,j,k)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A], j: Rep[A], k: Rep[A], l: Rep[A]) = vec_obj_new[_12,A](a,b,c,d,e,f,g,h,i,j,k,l)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A], j: Rep[A], k: Rep[A], l: Rep[A], m: Rep[A]) = vec_obj_new[_13,A](a,b,c,d,e,f,g,h,i,j,k,l,m)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A], j: Rep[A], k: Rep[A], l: Rep[A], m: Rep[A], n: Rep[A]) = vec_obj_new[_14,A](a,b,c,d,e,f,g,h,i,j,k,l,m,n)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A], j: Rep[A], k: Rep[A], l: Rep[A], m: Rep[A], n: Rep[A], o: Rep[A]) = vec_obj_new[_15,A](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A], j: Rep[A], k: Rep[A], l: Rep[A], m: Rep[A], n: Rep[A], o: Rep[A], p: Rep[A]) = vec_obj_new[_16,A](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A], j: Rep[A], k: Rep[A], l: Rep[A], m: Rep[A], n: Rep[A], o: Rep[A], p: Rep[A], q: Rep[A]) = vec_obj_new[_17,A](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A], j: Rep[A], k: Rep[A], l: Rep[A], m: Rep[A], n: Rep[A], o: Rep[A], p: Rep[A], q: Rep[A], r: Rep[A]) = vec_obj_new[_18,A](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A], j: Rep[A], k: Rep[A], l: Rep[A], m: Rep[A], n: Rep[A], o: Rep[A], p: Rep[A], q: Rep[A], r: Rep[A], s: Rep[A]) = vec_obj_new[_19,A](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A], j: Rep[A], k: Rep[A], l: Rep[A], m: Rep[A], n: Rep[A], o: Rep[A], p: Rep[A], q: Rep[A], r: Rep[A], s: Rep[A], t: Rep[A]) = vec_obj_new[_20,A](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A], j: Rep[A], k: Rep[A], l: Rep[A], m: Rep[A], n: Rep[A], o: Rep[A], p: Rep[A], q: Rep[A], r: Rep[A], s: Rep[A], t: Rep[A], u: Rep[A]) = vec_obj_new[_21,A](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A], j: Rep[A], k: Rep[A], l: Rep[A], m: Rep[A], n: Rep[A], o: Rep[A], p: Rep[A], q: Rep[A], r: Rep[A], s: Rep[A], t: Rep[A], u: Rep[A], v: Rep[A]) = vec_obj_new[_22,A](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)
  }

  implicit def repVecToVecOps[N<:IntM:Manifest:MVal, A:Manifest](x: Rep[Vec[N, A]]) = new vecOpsCls(x)
  implicit def varToVecOps[N<:IntM:Manifest:MVal, A:Manifest](x: Var[Vec[N, A]]) = new vecOpsCls(readVar(x))

  // This class defines the public interface for the Vec[T] class.
  class vecOpsCls[N<:IntM:Manifest:MVal, A:Manifest](u: Rep[Vec[N, A]]) {
    type Self = Vec[N,A]
    def x(implicit f : EnsureSize[_0,N]) = vec_apply(u, 0)
    def y(implicit f : EnsureSize[_1,N]) = vec_apply(u, 1)
    def z(implicit f : EnsureSize[_2,N]) = vec_apply(u, 2)
    def w(implicit f : EnsureSize[_3,N]) = vec_apply(u, 3)

    def apply(n: Rep[Int]) = vec_apply(u, n)
    def update(n: Rep[Int], c: Rep[A]) = vec_update(u,n,c)
    
    def apply[TT<:IntM:Manifest:MVal](n:TT)(implicit f:EnsureSize[TT,N]) : Rep[A] = vec_apply(u,MIntDepth[TT])
    def update[TT<:IntM:Manifest:MVal](n:TT, v:Rep[A])(implicit f:EnsureSize[TT,N]) : Rep[Unit] = vec_update(u,MIntDepth[TT],v)

    def +(vt : Rep[Self])(implicit n: Arith[A]) = vec_plus(u,vt)
    def -(vt : Rep[Self])(implicit n: Arith[A]) = vec_minus(u,vt)
    def *(vt : Rep[Self])(implicit n: Arith[A]) = vec_times(u,vt)
    def /(vt : Rep[Self])(implicit n: Arith[A]) = vec_divide(u,vt)

    def *(vt : Rep[A])(implicit n: Arith[A], o: Overloaded1) = vec_times_scalar(u,vt)
    def /(vt : Rep[A])(implicit n: Arith[A], o: Overloaded1) = vec_divide_scalar(u,vt)

    def unary_-(implicit o : Arith[A]) : Rep[Vec[N,A]] = vec_negate(u)
    
    def size() : Rep[Int] = MIntDepth[N]
    def length(implicit v: MVal[N] = null) : Rep[Int] = size()
    def sum(implicit a: Arith[A]) = vec_sum(u)
    def abs(implicit a: Arith[A]) = vec_abs(u)

    def min(vt: Rep[Self])(implicit n: Ordering[A]) = vec_zip_min(u, vt)
    def max(vt: Rep[Self])(implicit n: Ordering[A]) = vec_zip_max(u, vt)
    
    def *<*(vt: Rep[Self])(implicit n: Ordering[A]) = vec_zip_min(u, vt)
    def *>*(vt: Rep[Self])(implicit n: Ordering[A]) = vec_zip_max(u, vt)
    
    //def map[B:Manifest](f: Rep[A] => Rep[B]) = vec_map(x,f)

    //def &[M<:IntM:Manifest:MVal](rhs : Rep[Vec[M,A]]) = vec_concat[N,M,N+M,A](u, rhs)
    
    def mutable() = vec_mutable_clone(u)
  }

  // Language ops
  def cross[A:Manifest:Arith](a: Rep[Vec[_3,A]], b: Rep[Vec[_3,A]]) : Rep[Vec[_3,A]]
  def dot[N<:IntM:Manifest:MVal, A:Manifest:Arith](a: Rep[Vec[N,A]],b: Rep[Vec[N,A]]) = {val v = a * b; v.sum}
  def normalize[N<:IntM:Manifest:MVal, A:Manifest:Arith](a: Rep[Vec[N,A]]) : Rep[Vec[N,A]]
  def length[N<:IntM:Manifest:MVal, A:Manifest](a: Rep[Vec[N,A]]) = vec_size(a)
  def outer[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, A:Manifest:Arith](a: Rep[Vec[R,A]], b: Rep[Vec[C,A]]) : Rep[Mat[R,C,A]]
  
  def vec_obj_new[N<:IntM:Manifest:MVal, A:Manifest](xs: Rep[A]*): Rep[Vec[N,A]]
  def vec_obj_n_new[N<:IntM:Manifest:MVal, A:Manifest](i : Rep[Int]): Rep[Vec[N,A]]

  def vec_apply[N<:IntM:Manifest:MVal, A:Manifest](x: Rep[Vec[N, A]], i: Rep[Int]): Rep[A]
  def vec_update[N<:IntM:Manifest:MVal, A:Manifest](x: Rep[Vec[N, A]], i: Rep[Int], v: Rep[A]): Rep[Unit]
  
  def vec_plus[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]], y: Rep[Vec[N,A]]): Rep[Vec[N,A]]
  def vec_plus_scalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]], y: Rep[A]): Rep[Vec[N,A]]
  def vec_minus[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]], y: Rep[Vec[N,A]]): Rep[Vec[N,A]]
  def vec_minus_scalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]], y: Rep[A]): Rep[Vec[N,A]]
  def vec_times[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]], y: Rep[Vec[N,A]]): Rep[Vec[N,A]]
  def vec_times_scalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]], y: Rep[A]): Rep[Vec[N,A]]
  def vec_divide[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]], y: Rep[Vec[N,A]]): Rep[Vec[N,A]]
  def vec_divide_scalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]], y: Rep[A]): Rep[Vec[N,A]]

  def vec_map[N<:IntM:Manifest:MVal,A:Manifest,B:Manifest](x: Rep[Vec[N,A]], f: Rep[A] => Rep[B]): Rep[Vec[N,B]]
  
  def vec_size[N<:IntM:Manifest:MVal, A:Manifest](x: Rep[Vec[N,A]]): Rep[Int]

  def vec_sum[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]]): Rep[A]
  def vec_abs[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]]): Rep[Vec[N,A]]
  
  def vec_negate[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]]): Rep[Vec[N,A]]
  //def vec_concat[N<:IntM:Manifest:MVal, M<:IntM:Manifest:MVal, O<:IntM:Manifest:MVal,  A:Manifest](x: Rep[Vec[N,A]], rhs: Rep[Vec[M,A]]): Rep[Vec[O,A]]

  def vec_zip_min[N<:IntM:Manifest:MVal, A:Manifest:Ordering](x: Rep[Vec[N,A]], y: Rep[Vec[N,A]]): Rep[Vec[N,A]]
  def vec_zip_max[N<:IntM:Manifest:MVal, A:Manifest:Ordering](x: Rep[Vec[N,A]], y: Rep[Vec[N,A]]): Rep[Vec[N,A]]
  
  def vec_mutable_clone[N<:IntM:Manifest:MVal, A:Manifest](x: Rep[Vec[N,A]]): Rep[Vec[N,A]]
}

trait VecOpsExp extends VecOps with VariablesExp with BaseFatExp {
  this: VecImplOps with DeLisztExp =>

  override def reflectPure[A:Manifest](x: Def[A]): Exp[A] = toAtom(x) // TODO: just to make refactoring easier in case we want to change to reflectSomething

  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  case class VecObjNew[N<:IntM:Manifest:MVal,A:Manifest](xs: Exp[A]*)(implicit val mV : Manifest[VecImpl[N,A]]) extends Def[Vec[N,A]] {
    def n = manifest[N]
    def vn = implicitly[MVal[N]]
    def a = manifest[A]
  }
  
  case class VecObjNNew[N<:IntM:Manifest:MVal,A:Manifest](i: Exp[Int])(implicit val mV : Manifest[VecImpl[N,A]]) extends Def[Vec[N,A]] {
    def n = manifest[N]
    def vn = implicitly[MVal[N]]
    def a = manifest[A]
  }

  case class VecApply[N<:IntM:Manifest:MVal,A:Manifest](x: Exp[Vec[N,A]], i: Exp[Int]) extends Def[A] {
    def n = manifest[N]
    def vn = implicitly[MVal[N]]
    def a = manifest[A]
  }
  
  case class VecUpdate[N<:IntM:Manifest:MVal,A:Manifest](x: Exp[Vec[N,A]], i: Exp[Int], y: Exp[A]) extends Def[Unit] {
    def n = manifest[N]
    def vn = implicitly[MVal[N]]
    def a = manifest[A]
  }
  
  case class VecSize[N<:IntM:Manifest:MVal,A:Manifest](x: Exp[Vec[N,A]]) extends Def[Int] {
    def n = manifest[N]
    def vn = implicitly[MVal[N]]
    def a = manifest[A]
  }

  // case class VecConcat[N<:IntM:Manifest:MVal, M<:IntM:Manifest:MVal, O<:IntM:Manifest:MVal, A:Manifest](x: Exp[Vec[N,A]], y: Exp[Vec[M,A]]) extends Def[Vec[O,A]]
  
  case class VecOuter[R<:IntM:Manifest:MVal,C<:IntM:Manifest:MVal,A:Manifest:Arith](x: Exp[Vec[R,A]], y: Exp[Vec[C,A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vec_outer_impl[R,C,A](x,y))) {
      def r = manifest[R]
      def vr = implicitly[MVal[R]]
      def c = manifest[C]
      def vc = implicitly[MVal[C]]
      def m = manifest[A]
      def a = implicitly[Arith[A]]
  }
  
  case class VecNormalize[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vec_normalize_impl[N,A](x))) {
      def n = manifest[N]
      def vn = implicitly[MVal[N]]
      def m = manifest[A]
      def a = implicitly[Arith[A]]
  }
  
  case class VecCross[A:Manifest:Arith](x: Exp[Vec[_3,A]], y: Exp[Vec[_3,A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vec_cross_impl[A](x,y))) {
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  ////////////////////////////////
  // implemented via delite ops

  case class VecMap[N<:IntM:Manifest:MVal,A:Manifest,B:Manifest](in: Exp[Vec[N,A]], func: Exp[A] => Exp[B])
    extends DeliteOpMap[A,B,Vec[N,B]] {

    def alloc = reifyEffects(Vec[N,B](in.size))
    val size = in.size
    
    def n = manifest[N]
    def vn = implicitly[MVal[N]]
    def m = manifest[A]
  }
  
  abstract class VecArithmeticMap[N<:IntM:Manifest:MVal,A:Manifest:Arith](in: Exp[Vec[N,A]]) extends DeliteOpMap[A,A,Vec[N,A]] {
    def alloc = Vec[N,A](in.size)
    val size = in.size
    
    def n = manifest[N]
    def vn = implicitly[MVal[N]]
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }
  
  abstract class VecArithmeticZipWith[N<:IntM:Manifest:MVal,A:Manifest:Arith](inA: Exp[Vec[N,A]], inB: Exp[Vec[N,A]]) extends DeliteOpZipWith[A,A,A,Vec[N,A]] {
    def alloc = Vec[N,A](inA.size)
    val size = inA.size
    
    def n = manifest[N]
    def vn = implicitly[MVal[N]]
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  abstract class VecOrderingZipWith[N<:IntM:Manifest:MVal,A:Manifest:Ordering](inA: Exp[Vec[N,A]], inB: Exp[Vec[N,A]]) extends DeliteOpZipWith[A,A,A,Vec[N,A]] {
    def alloc = Vec[N,A](inA.size)
    val size = inA.size
    
    def n = manifest[N]
    def vn = implicitly[MVal[N]]
    def m = manifest[A]
    def o = implicitly[Ordering[A]]
  }
  
  abstract class VecReduce[N<:IntM:Manifest:MVal,A:Manifest](in: Exp[Vec[N,A]]) 
    extends DeliteOpReduce[A] {
    val size = in.size
    def n = manifest[N]
    def vn = implicitly[MVal[N]]
    def m = manifest[A]
  }
  
  abstract class VecArithReduce[N<:IntM:Manifest:MVal,A:Manifest:Arith](in: Exp[Vec[N,A]]) 
    extends DeliteOpReduce[A] {
    val size = in.size
    def n = manifest[N]
    def vn = implicitly[MVal[N]]
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  case class VecNegate[N<:IntM:Manifest:MVal,A:Manifest:Arith](in: Exp[Vec[N,A]])
    extends VecArithmeticMap(in) {

    def func = e => -e
  }

  case class VecPlus[N<:IntM:Manifest:MVal, A:Manifest:Arith](inA: Exp[Vec[N,A]], inB: Exp[Vec[N,A]]) 
    extends VecArithmeticZipWith(inA, inB) {

    def func = (a,b) => a + b
  }

  case class VecPlusScalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](in: Exp[Vec[N,A]], y: Exp[A])
    extends VecArithmeticMap(in) {

    def func = e => e + y
  }

  case class VecMinus[N<:IntM:Manifest:MVal, A:Manifest:Arith](inA: Exp[Vec[N,A]], inB: Exp[Vec[N,A]])
    extends VecArithmeticZipWith(inA, inB) {

    def func = (a,b) => a - b
  }

  case class VecMinusScalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](in: Exp[Vec[N,A]], y: Exp[A])
    extends VecArithmeticMap(in) {

    def func = e => e - y
  }

  case class VecTimes[N<:IntM:Manifest:MVal, A:Manifest:Arith](inA: Exp[Vec[N,A]], inB: Exp[Vec[N,A]])
    extends VecArithmeticZipWith(inA, inB) {

    def func = (a,b) => a * b
  }

  case class VecTimesScalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](in: Exp[Vec[N,A]], y: Exp[A])
    extends VecArithmeticMap(in) {

    def func = e => e * y
  }

  case class VecDivide[N<:IntM:Manifest:MVal, A:Manifest:Arith](inA: Exp[Vec[N,A]], inB: Exp[Vec[N,A]])
    extends VecArithmeticZipWith(inA, inB) {

    def func = (a,b) => a * b
  }

  case class VecDivideScalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](in: Exp[Vec[N,A]], y: Exp[A])
    extends VecArithmeticMap(in) {

    def func = e => e / y
  }
  
  case class VecSum[N<:IntM:Manifest:MVal, A:Manifest:Arith](in: Exp[Vec[N,A]]) 
    extends VecArithReduce(in) {
    
    val zero = a.empty 
    def func = (a,b) => a + b
  }
  
  case class VecAbs[N<:IntM:Manifest:MVal, A:Manifest:Arith](in: Exp[Vec[N,A]])
    extends VecArithmeticMap[N,A](in) {
    
    def func = e => e.abs
  }

  case class VecMin[N<:IntM:Manifest:MVal, A:Manifest:Ordering:HasMinMax](in: Exp[Vec[N,A]])
    extends VecReduce(in) {
    
    val zero = implicitly[HasMinMax[A]].maxValue
    def func = (a,b) => if (a < b) a else b
    
    val o = implicitly[Ordering[A]]
    val p = implicitly[HasMinMax[A]]
  }

  case class VecMax[N<:IntM:Manifest:MVal, A:Manifest:Ordering:HasMinMax](in: Exp[Vec[N,A]])
    extends VecReduce(in) {
    
    val zero = implicitly[HasMinMax[A]].minValue
    def func = (a,b) => if (a > b) a else b
    
    val o = implicitly[Ordering[A]]
    val p = implicitly[HasMinMax[A]]
  }
  
  case class VecZipMin[N<:IntM:Manifest:MVal, A:Manifest:Ordering](inA: Exp[Vec[N,A]], inB: Exp[Vec[N,A]])
    extends VecOrderingZipWith(inA, inB) {

    def func = (a,b) => a min b
  }
  
  case class VecZipMax[N<:IntM:Manifest:MVal, A:Manifest:Ordering](inA: Exp[Vec[N,A]], inB: Exp[Vec[N,A]])
    extends VecOrderingZipWith(inA, inB) {

    def func = (a,b) => a max b
  }
  
  case class VecClone[N<:IntM:Manifest:MVal, A:Manifest](x: Exp[Vec[N,A]]) extends Def[Vec[N,A]] {
    def n = manifest[N]
    def vn = implicitly[MVal[N]]
    def m = manifest[A]
  }

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case e@VecApply(x, i) => vec_apply(f(x), f(i))(e.n, e.vn, e.a)
    case e@VecSize(x) => vec_size(f(x))(e.n, e.vn, e.a)
    // DeliteOpSingleTask and DeliteOpLoop
    case e@VecNegate(x) => reflectPure(new { override val original = Some(f,e) } with VecNegate(f(x))(e.n, e.vn, e.m, e.a))(mtype(manifest[A]))
    case e@VecPlus(x,y) => reflectPure(new { override val original = Some(f,e) } with VecPlus(f(x),f(y))(e.n, e.vn, e.m, e.a))(mtype(manifest[A]))
    case e@VecPlusScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with VecPlusScalar(f(x),f(y))(e.n, e.vn, e.m, e.a))(mtype(manifest[A]))
    case e@VecMinus(x,y) => reflectPure(new { override val original = Some(f,e) } with VecMinus(f(x),f(y))(e.n, e.vn, e.m, e.a))(mtype(manifest[A]))
    case e@VecMinusScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with VecMinusScalar(f(x),f(y))(e.n, e.vn, e.m, e.a))(mtype(manifest[A]))
    case e@VecTimes(x,y) => reflectPure(new { override val original = Some(f,e) } with VecTimes(f(x),f(y))(e.n, e.vn, e.m, e.a))(mtype(manifest[A]))
    case e@VecTimesScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with VecTimesScalar(f(x),f(y))(e.n, e.vn, e.m, e.a))(mtype(manifest[A]))
    case e@VecDivide(x,y) => reflectPure(new { override val original = Some(f,e) } with VecDivide(f(x),f(y))(e.n, e.vn, e.m, e.a))(mtype(manifest[A]))
    case e@VecDivideScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with VecDivideScalar(f(x),f(y))(e.n, e.vn, e.m, e.a))(mtype(manifest[A]))
    case e@VecCross(x,y) => reflectPure(new { override val original = Some(f,e) } with VecCross(f(x),f(y))(e.m, e.a))(mtype(manifest[A]))
    case e@VecNormalize(x) => reflectPure(new { override val original = Some(f,e) } with VecNormalize(f(x))(e.n, e.vn, e.m, e.a))(mtype(manifest[A]))
    case e@VecSum(x) => reflectPure(new { override val original = Some(f,e) } with VecSum(f(x))(e.n, e.vn, e.m, e.a))(mtype(manifest[A]))
    case e@VecAbs(x) => reflectPure(new { override val original = Some(f,e) } with VecAbs(f(x))(e.n, e.vn, e.m, e.a))(mtype(manifest[A]))
    case e@VecMin(x) => reflectPure(new { override val original = Some(f,e) } with VecMin(f(x))(e.n, e.vn, e.m, e.o, e.p))(mtype(manifest[A]))
    case e@VecMax(x) => reflectPure(new { override val original = Some(f,e) } with VecMax(f(x))(e.n, e.vn, e.m, e.o, e.p))(mtype(manifest[A]))
    case e@VecZipMin(x,y) => reflectPure(new { override val original = Some(f,e) } with VecZipMin(f(x),f(y))(e.n, e.vn, e.m, e.o))(mtype(manifest[A]))
    case e@VecZipMax(x,y) => reflectPure(new { override val original = Some(f,e) } with VecZipMax(f(x),f(y))(e.n, e.vn, e.m, e.o))(mtype(manifest[A]))
    // Read/write effects
    case Reflect(e@VecApply(l,r), u, es) => reflectMirrored(Reflect(VecApply(f(l),f(r))(e.n, e.vn, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VecUpdate(l,i,r), u, es) => reflectMirrored(Reflect(VecUpdate(f(l),f(i),f(r))(e.n, e.vn, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    // Effect with SingleTask and DeliteOpLoop
    // Allocation
    case Reflect(e@VecObjNew(xs @ _*), u, es) => reflectMirrored(Reflect(VecObjNew(f(xs) : _*)(e.n, e.vn, e.a, e.mV), mapOver(f,u), f(es)))
    case Reflect(e@VecObjNNew(n), u, es) => reflectMirrored(Reflect(VecObjNNew(f(n))(e.n, e.vn, e.a, e.mV), mapOver(f,u), f(es)))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case VecApply(a,i) => Nil
    case VecUpdate(a,i,x) => Nil
    case VecClone(a) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case VecApply(a,i) => Nil
    case VecUpdate(a,i,x) => syms(x)
    case VecClone(a) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case VecApply(a,i) => syms(a)
    case VecUpdate(a,i,x) => Nil
    case VecClone(a) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case VecApply(a,i) => Nil
    case VecUpdate(a,i,x) => syms(a)
    case VecClone(a) => syms(a)
    case _ => super.copySyms(e)
  }
  
  /////////////////////
  // object interface
  def vec_obj_new[N<:IntM:Manifest:MVal, A:Manifest](xs: Exp[A]*) = {
    reflectMutable(VecObjNew[N,A](xs:_*)).unsafeImmutable
  }
  
  def vec_obj_n_new[N<:IntM:Manifest:MVal, A:Manifest](i: Exp[Int]) = {
    reflectMutable(VecObjNNew[N,A](i)).unsafeImmutable
  }

  /////////////////////
  // class interface
  def vec_apply[N<:IntM:Manifest:MVal, A:Manifest](x: Exp[Vec[N,A]], i: Exp[Int]) = reflectPure(VecApply(x, i))
  
  def vec_update[N<:IntM:Manifest:MVal, A:Manifest](x: Exp[Vec[N, A]], i: Exp[Int], v: Exp[A]) = reflectWrite(x)(VecUpdate(x, i, v))

  def vec_plus[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[Vec[N,A]]) = reflectPure(VecPlus(x,y))
  def vec_plus_scalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[A]) = reflectPure(VecPlusScalar(x,y))
  def vec_minus[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[Vec[N,A]]) = reflectPure(VecMinus(x,y))
  def vec_minus_scalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[A]) = reflectPure(VecMinusScalar(x,y))
  def vec_times[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[Vec[N,A]]) = reflectPure(VecTimes(x,y))
  def vec_times_scalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[A]) = reflectPure(VecTimesScalar(x,y))

  def vec_divide[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[Vec[N,A]]) = reflectPure(VecDivide(x,y))
  def vec_divide_scalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[A]) = reflectPure(VecDivideScalar(x,y))
  
  def vec_negate[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]]) = reflectPure(VecNegate(x))

  def vec_sum[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]]) = reflectPure(VecSum(x))
  def vec_abs[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]]) = reflectPure(VecAbs(x))
  
  def vec_size[N<:IntM:Manifest:MVal, A:Manifest](x: Exp[Vec[N,A]]) = reflectPure(VecSize(x))
  //def vec_concat[N<:IntM:Manifest:MVal, M<:IntM:Manifest:MVal, O<:IntM, A:Manifest](x: Exp[Vec[N,A]], y: Exp[Vec[M,A]]) = reflectPure(VecConcat[N,M,O,A](x,y))

  def vec_map[N<:IntM:Manifest:MVal,A:Manifest,B:Manifest](x: Exp[Vec[N,A]], f: Exp[A] => Exp[B]) = reflectPure(VecMap(x, f))
  
  def vec_zip_min[N<:IntM:Manifest:MVal, A:Manifest:Ordering](x: Exp[Vec[N,A]], y: Exp[Vec[N,A]]) = reflectPure(VecZipMin(x,y))
  def vec_zip_max[N<:IntM:Manifest:MVal, A:Manifest:Ordering](x: Exp[Vec[N,A]], y: Exp[Vec[N,A]]) = reflectPure(VecZipMax(x,y))
  
  /* Language ops */
  def cross[A:Manifest:Arith](a: Exp[Vec[_3,A]], b: Exp[Vec[_3,A]]) = reflectPure(VecCross(a,b))
  def normalize[N<:IntM:Manifest:MVal, A:Manifest:Arith](a: Exp[Vec[N,A]]) = reflectPure(VecNormalize(a))
  def outer[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, A:Manifest:Arith](a: Exp[Vec[R,A]], b: Exp[Vec[C,A]]) = reflectPure(VecOuter(a,b))
  
  def vec_mutable_clone[N<:IntM:Manifest:MVal, A:Manifest](x: Exp[Vec[N,A]]) = reflectMutable(VecClone(x))
}

trait VecOpsExpOpt extends VecOpsExp {
  this: VecImplOps with DeLisztExp =>
}

trait BaseGenVecOps extends GenericFatCodegen {
  val IR: VecOpsExp
  import IR._

  override def unapplySimpleIndex(e: Def[Any]) = e match { // TODO: move elsewhere
    case VecApply(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }
}

trait ScalaGenVecOps extends BaseGenVecOps with ScalaGenFat {
  val IR: VecOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      case v@VecObjNew(xs @ _*) => emitValDef(sym, remap(v.mV) + "(" + xs.map(quote).reduceLeft(_+","+_) + ")")
      case v@VecObjNNew(i) => emitValDef(sym, "new " + remap(v.mV) + "(" + quote(i) + ")")
      // these are the ops that call through to the underlying real data structure
      case VecApply(x,n) => emitValDef(sym, quote(x) + ".dcApply(" + quote(n) + ")")
      case VecUpdate(x,n,y) => emitValDef(sym, quote(x) + ".dcUpdate(" + quote(n) + ") = " + quote(y))
      case VecSize(x) => emitValDef(sym, quote(x) + ".size")
      case VecClone(x) => emitValDef(sym, quote(x) + ".cloneL")
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenVecOps extends BaseGenVecOps with CudaGenFat with CudaGenDataStruct {
  val IR: VecOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    //case v@VecObjNew(xs @ _*) => emitValDef(sym, remap(v.mV) + "(" + xs.map(quote).reduceLeft(_+","+_) + ")")
    //case v@VecObjNNew(i) => emitValDef(sym, "new " + remap(v.mV) + "(" + quote(i) + ")")
    // these are the ops that call through to the underlying real data structure
    case VecApply(x,n) => emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case VecUpdate(x,n,y) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))
    case VecSize(x) => emitValDef(sym, quote(x) + ".size")
    //case VecClone(x) => emitValDef(sym, quote(x) + ".cloneL")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenVecOps extends BaseGenVecOps with CGenFat {
  val IR: VecOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

