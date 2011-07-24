package ppl.dsl.deliszt.vec

import _root_.scala.reflect.Manifest
import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
import java.io.PrintWriter
import ppl.dsl.deliszt.datastruct.scala._

import ppl.delite.framework.DSLType
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}

trait VecOps extends DSLType with Variables with MetaInteger {
  this: DeLiszt =>

  object Vec {
    def apply[N<:IntM:Manifest:MVal,A]() = vec_obj_n_new[N,A]()
    def apply[A:Manifest](a : Rep[A]) = vec_obj_new[_1,A](a)
		def apply[A:Manifest](a : Rep[A], b : Rep[A]) = vec_obj_new[_2,A](a,b)
		/*def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A]) = vec_obj_new[_3,A](a,b,c)
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
		def apply[A:Manifest](a : Rep[A], b : Rep[A], c : Rep[A], d : Rep[A], e : Rep[A], f: Rep[A], g: Rep[A], h: Rep[A], i: Rep[A], j: Rep[A], k: Rep[A], l: Rep[A], m: Rep[A], n: Rep[A], o: Rep[A], p: Rep[A], q: Rep[A], r: Rep[A], s: Rep[A], t: Rep[A], u: Rep[A], v: Rep[A]) = vec_obj_new[_22,A](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)*/
  }

  implicit def repVecToVecOps[N<:IntM:Manifest:MVal, A : Manifest](x: Rep[Vec[N, A]]) = new vecOpsCls(x)
  implicit def varToVecOps[N<:IntM:Manifest:MVal, A:Manifest](x: Var[Vec[N, A]]) = new vecOpsCls(readVar(x))

  /**
   * This class defines the public interface for the Vec[T] class.
   */
  class vecOpsCls[N<:IntM:Manifest:MVal, A:Manifest](u: Rep[Vec[N, A]]) {
    type Self = Vec[N,A]

    def x(implicit f : EnsureSize[_0,N]) = vec_apply(u, 0)
    def y(implicit f : EnsureSize[_1,N]) = vec_apply(u, 1)
    def z(implicit f : EnsureSize[_2,N]) = vec_apply(u, 2)
    def w(implicit f : EnsureSize[_3,N]) = vec_apply(u, 3)

    def apply(n: Rep[Int]) = vec_apply(u, n)
    def update(n: Rep[Int], c: Rep[A]) = vec_update(u,n,c)
    
    def apply[TT<:IntM](n:TT)(implicit mv: MVal[TT], mm: Manifest[TT], f:EnsureSize[TT,N]) : Rep[A] = vec_apply(u,MIntDepth[TT])
    def update[TT<:IntM](n:TT, v:Rep[A])(implicit mv: MVal[TT], mm: Manifest[TT], f:EnsureSize[TT,N]) : Rep[Unit] = vec_update(u,MIntDepth[TT],v)

    def +(vt : Rep[Self])(implicit n: Arith[A]) = vec_plus(u,vt)
    def -(vt : Rep[Self])(implicit n: Arith[A]) = vec_minus(u,vt)
    def *(vt : Rep[Self])(implicit n: Arith[A]) = vec_times(u,vt)
    def /(vt : Rep[Self])(implicit n: Arith[A]) = vec_divide(u,vt)

    def *(vt : Rep[A])(implicit n: Arith[A], o: Overloaded1) = vec_times_scalar(u,vt)
    def /(vt : Rep[A])(implicit n: Arith[A], o: Overloaded1) = vec_divide_scalar(u,vt)

    def unary_-(implicit o : Arith[A]) : Rep[Vec[N,A]] = vec_negate(u)
    
    def size : Rep[Int]
    def sum(implicit a: Arith[A]) = vec_sum(u)
    def abs(implicit a: Arith[A]) = vec_abs(u)

    def &[N<:IntM:Manifest:MVal, M<:IntM:Manifest:MVal](rhs : Rep[Vec[M,A]]) : Vec[N#Add[M],A] = vec_concat(u, rhs)
  }

  /* Language ops */
  def cross[A:Manifest:Arith](a: Rep[Vec[_3,A]], b: Rep[Vec[_3,A]]) : Rep[Vec[_3,A]]
  def dot[N<:IntM:Manifest:MVal, A:Manifest:Arith](a: Rep[Vec[N,A]],b: Rep[Vec[N,A]]) = {val v = a * b; v.sum}
  def normalize[N<:IntM:Manifest:MVal, A:Manifest:Arith](a: Rep[Vec[N,A]]) : Rep[Vec[N,A]]
  def length[N<:IntM:Manifest:MVal, A:Manifest](a: Rep[Vec[N,A]]) = vec_size(a)
  def outer[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, A:Manifest:Arith](a: Rep[Vec[R,A]], b: Rep[Vec[C,A]]) : Rep[Mat[R,C,A]]
  
/*
  class vecOpsClsMutable[A:Manifest](vx: Var[Vec[N,A]]) extends vecOpsCls[A](readVar(vx)) {
    // ...
  }
*/
  def vec_obj_new[N<:IntM:Manifest:MVal, A](xs: Rep[A]*): Rep[Vec[N,A]]
  def vec_obj_n_new[N<:IntM:Manifest:MVal, A](): Rep[Vec[N,A]]

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

  def vec_size[N<:IntM:Manifest:MVal, A:Manifest](x: Rep[Vec[N,A]]): Rep[Int]

  def vec_sum[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]]): Rep[A]
  def vec_abs[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]]): Rep[Vec[N,A]]
  
  def vec_negate[N<:IntM:Manifest:MVal, A:Manifest](x: Rep[Vec[N,A]]): Rep[Vec[N,A]]
  def vec_concat[N<:IntM:Manifest:MVal, M<:IntM:Manifest:MVal, A:Manifest](x: Rep[Vec[N,A]], rhs: Rep[Vec[M,A]]): Rep[Vec[N#Add[M],A]]
}

trait CleanRoom {
}

trait VecOpsExp extends VecOps with VariablesExp with BaseFatExp with CleanRoom {
  this: VecImplOps with DeLisztExp =>

  def reflectPure[A:Manifest](x: Def[A]): Exp[A] = toAtom(x) // TODO: just to make refactoring easier in case we want to change to reflectSomething

  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  case class VecObjNew[N<:IntM:Manifest:MVal,A:Manifest](xs: Exp[A]*) extends Def[Vec[N,A]] {
    val mV = manifest[Vec[N,A]]
    val mN = manifest[N]
  }

  case class VecObjNNew[N<:IntM:Manifest:MVal,A:Manifest]() extends Def[Vec[N,A]] {
    val mV = manifest[Vec[N,A]]
    val mN = manifest[N]
  }

  case class VecApply[N<:IntM:Manifest:MVal,A:Manifest](x: Exp[Vec[N,A]], n: Exp[Int]) extends Def[A]
  case class VecSize[N<:IntM:Manifest:MVal,A:Manifest](x: Exp[Vec[N,A]]) extends Def[Int]
  case class VecUpdate[N<:IntM:Manifest:MVal,A:Manifest](x: Exp[Vec[N,A]], n: Exp[Int], y: Exp[A]) extends Def[Unit]

  case class VecConcat[N<:IntM:Manifest:MVal, M<:IntM:Manifest:MVal, A:Manifest](x: Exp[Vec[N,A]], y: Exp[Vec[M,A]]) extends Def[Vec[N#Add[M],A]]

  ////////////////////////////////
  // implemented via delite ops

  abstract class DeliteOpVecLoop[N<:IntM:Manifest:MVal, A:Manifest] extends DeliteOpLoop[Vec[N,A]] {
    val size: Exp[Int] //inherited
  }
  
  abstract class VecMap[N<:IntM:Manifest:MVal,A:Manifest,B:Manifest](in: Exp[Vec[N,A]], func: Exp[A] => Exp[B])
    extends DeliteOpMap[A,B,Vec[N,B]] {

    def alloc = reifyEffects(Vec[N,B]())
    val size = in.size
  }
  
  abstract class VecArithmeticMap[N<:IntM:Manifest:MVal,A:Manifest:Arith](in: Exp[Vec[N,A]]) extends DeliteOpMap[A,A,Vec[N,A]] {
    def alloc = Vec[N,A]()
    val size = in.size
    
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  case class VecNegate[N<:IntM:Manifest:MVal,A:Manifest:Arith](in: Exp[Vec[N,A]])
    extends VecMap[N,A,A](in, (v:Exp[A]) => -v)

  case class VecPlus[N<:IntM:Manifest:MVal, A:Manifest:Arith](inA: Exp[Vec[N,A]], inB: Exp[Vec[N,A]]) 
    extends DeliteOpVecLoop[N,A] {

    val size = inA.size
    val v = fresh[Int]
    val body: Def[Vec[N,A]] = DeliteCollectElem[A,Vec[N,A]](
      alloc = reifyEffects(Vec[N,A]()),
      func = reifyEffects(inA(v) + inB(v))
    )
  }

  case class VecPlusScalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](in: Exp[Vec[N,A]], y: Exp[A])
    extends DeliteOpMap[A,A,Vec[N,A]] {

    val alloc = reifyEffects(Vec[N,A]())
    val v = fresh[A]
    val func = reifyEffects(v + y)
  }

  case class VecMinus[N<:IntM:Manifest:MVal, A:Manifest:Arith](inA: Exp[Vec[N,A]], inB: Exp[Vec[N,A]])
    extends DeliteOpVecLoop[N,A] {

    val size = inA.size
    val v = fresh[Int]
    val body: Def[Vec[N,A]] = DeliteCollectElem[A,Vec[N,A]](
      alloc = reifyEffects(Vec[N,A]()),
      func = reifyEffects(inA(v) - inB(v))
    )
  }

  case class VecMinusScalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](in: Exp[Vec[N,A]], y: Exp[A])
    extends DeliteOpMap[A,A,Vec[N,A]] {

    val alloc = reifyEffects(Vec[N,A]())
    val v = fresh[A]
    val func = reifyEffects(v - y)
  }

  abstract case class VecTimes[N<:IntM:Manifest:MVal, A:Manifest:Arith](inA: Exp[Vec[N,A]], inB: Exp[Vec[N,A]]) extends DeliteOpVecLoop[N,A] {
    def mev = manifest[A]
    def aev = implicitly[Arith[A]]
  }
  
  class VecTimesFresh[N<:IntM:Manifest:MVal, A:Manifest:Arith](inA: Exp[Vec[N,A]], inB: Exp[Vec[N,A]]) extends VecTimes(inA, inB) {
    val size = inA.size
    val v = fresh[Int]
    val body: Def[Vec[N,A]] = DeliteCollectElem[A,Vec[N,A]](
      alloc = reifyEffects(Vec[N,A]()),
      func = reifyEffects(inA(v) * inB(v))
    )
  }

  abstract case class VecTimesScalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](in: Exp[Vec[N,A]], y: Exp[A]) extends DeliteOpVecLoop[N,A] {
    def mev = manifest[A]
    def aev = implicitly[Arith[A]]
  }

  class VecTimesScalarFresh[N<:IntM:Manifest:MVal, A:Manifest:Arith](in: Exp[Vec[N,A]], y: Exp[A]) extends VecTimesScalar[N,A](in,y) {
    val size = in.size
    val v = fresh[Int]
    val body: Def[Vec[N,A]] = DeliteCollectElem[A,Vec[N,A]](
      alloc = reifyEffects(Vec[N,A]()),
      func = reifyEffects(in(v) * y)
    )
  }

  case class VecDivide[N<:IntM:Manifest:MVal, A:Manifest:Arith](inA: Exp[Vec[N,A]], inB: Exp[Vec[N,A]])
    extends DeliteOpVecLoop[N,A] {

    val size = inA.size
    val v = fresh[Int]
    val body: Def[Vec[N,A]] = new DeliteCollectElem[A,Vec[N,A]](
      alloc = reifyEffects(Vec[N,A]()),
      func = inA(v) / inB(v)
    )
  }

  case class VecDivideScalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](in: Exp[Vec[N,A]], y: Exp[A])
    extends DeliteOpVecLoop[N,A] {

    val size = in.size
    val v = fresh[Int]
    val body: Def[Vec[N,A]] = new DeliteCollectElem[A,Vec[N,A]](
      alloc = reifyEffects(Vec[N,A]()),
      func = in(v) / y
    )
  }
  
  case class VecSum[N<:IntM:Manifest:MVal, A:Manifest:Arith](in: Exp[Vec[N,A]]) 
    extends DeliteOpReduce[A] {

    val size = in.size
    val zero = a.empty 
    def func = (a,b) => a + b
    
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }
  
  case class VecAbs[N<:IntM:Manifest:MVal, A:Manifest:Arith](in: Exp[Vec[N,A]])
    extends VecArithmeticMap[N,A](in) {

    def func = e => e.abs
  }

  case class VecMin[N<:IntM:Manifest:MVal, A:Manifest:Ordering](in: Exp[Vec[N,A]])
    extends DeliteOpReduce[A] {

    val v = (fresh[A],fresh[A])
    val func = reifyEffects(if (v._1 < v._2) v._1 else v._2)
  }

  case class VecMax[N<:IntM:Manifest:MVal, A:Manifest:Ordering](in: Exp[Vec[N,A]])
    extends DeliteOpReduce[A] {

    val v = (fresh[A],fresh[A])
    val func = reifyEffects(if (v._1 > v._2) v._1 else v._2)
  }

  //////////////
  // mirroring

  /* override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case VecApply(x, n) => vec_apply(f(x), f(n))
    case VecSize(x) => vec_size(f(x))
    // FIXME: VecSum might not actually be triggered because
    case e@VecSum(x) => toAtom(new VecSum(f(x))(e.mev,e.aev) { val size = f(e.size); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody[A](e.body.asInstanceOf[Def[A]], f) })
    case e@VecTimes(x,y) => toAtom(new VecTimes(f(x),f(y))(e.mev,e.aev) { val size = f(e.size); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case e@VecTimesScalar(x,y) => toAtom(new VecTimesScalar(f(x),f(y))(e.mev,e.aev) { val size = f(e.size); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    // case Reflect(e@VecPPrint(x), u, es) => reflectMirrored(Reflect(VecPPrint(f(x))(f(e.block)), mapOver(f,u), f(es)))
    // below are read/write effects TODO: find a general approach to treating them!!!!
    case Reflect(VecApply(l,r), u, es) => reflectMirrored(Reflect(VecApply(f(l),f(r)), mapOver(f,u), f(es)))
    case Reflect(VecSize(x), u, es) => reflectMirrored(Reflect(VecSize(f(x)), mapOver(f,u), f(es)))
    case Reflect(VecForeach(a,b,c), u, es) => reflectMirrored(Reflect(VecForeach(f(a),f(b).asInstanceOf[Sym[Int]],f(c)), mapOver(f,u), f(es)))
    // FIXME: problem with VecTimes: it's actually a loop and if it is reflected it means a.size will also reflect and we have no context here!!!
    case Reflect(e2@VecTimes(a,b), u, es) => error("we'd rather not mirror " + e); //reflectMirrored(Reflect(VecTimes(f(a),f(b))(e.mev,e.aev), Read(f onlySyms rs), f(es)))
    case Reflect(VecUpdate(l,i,r), u, es) => reflectMirrored(Reflect(VecUpdate(f(l),f(i),f(r)), mapOver(f,u), f(es)))
    case Reflect(e@VecNew(l,r), u, es) => reflectMirrored(Reflect(VecNew(f(l),f(r))(e.mV), mapOver(f,u), f(es)))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why?? */


  /////////////////////
  // object interface
  def vec_obj_new[N<:IntM:Manifest:MVal, A:Manifest](xs: Exp[A]*) = reflectMutable(VecObjNew[N,A](xs:_*))
  def vec_obj_n_new[N<:IntM:Manifest:MVal, A:Manifest]() = reflectMutable(VecObjNNew[N,A]())

  /////////////////////
  // class interface
  def vec_apply[N<:IntM:Manifest:MVal, A:Manifest](x: Exp[Vec[N,A]], n: Exp[Int]) = reflectPure(VecApply(x, n))
  
  def vec_update[N<:IntM:Manifest:MVal, A:Manifest](x: Exp[Vec[N, A]], i: Exp[Int], v: Exp[A]) = reflectWrite(x)(VecUpdate(x, i, v))

  def vec_plus[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[Vec[N,A]]) = reflectPure(VecPlus(x,y))
  def vec_plus_scalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[A]) = reflectPure(VecPlusScalar(x,y))
  def vec_minus[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[Vec[N,A]]) = reflectPure(VecMinus(x,y))
  def vec_minus_scalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[A]) = reflectPure(VecMinusScalar(x,y))
  def vec_times[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[Vec[N,A]]) = reflectPure(new VecTimesFresh(x,y))
  def vec_times_scalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[A]) = reflectPure(new VecTimesScalarFresh(x,y))

  def vec_divide[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[Vec[N,A]]) = reflectPure(VecDivide(x,y))
  def vec_divide_scalar[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]], y: Exp[A]) = reflectPure(VecDivideScalar(x,y))
  
  def vec_negate[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Exp[Vec[N,A]]) = reflectPure(VecNegate(x))

  def vec_sum[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]]) = reflectPure(VecSum(x))
  def vec_abs[N<:IntM:Manifest:MVal, A:Manifest:Arith](x: Rep[Vec[N,A]]) = reflectPure(VecAbs(x))
  
  def vec_size[N<:IntM:Manifest:MVal, A:Manifest](x: Exp[Vec[N,A]]) = reflectPure(VecSize(x))
  def vec_concat[N<:IntM:Manifest:MVal, M<:IntM:Manifest:MVal, A:Manifest](x: Exp[Vec[N,A]], y: Exp[Vec[M,A]]) = reflectPure(VecConcat(x,y))

  /* Language ops */
  def cross[A:Arith](a: Exp[Vec[_3,A]], b: Exp[Vec[_3,A]]) : Rep[Vec[_3,A]]
  def normalize[N<:IntM:Manifest:MVal, A:Arith](a: Exp[Vec[N,A]]) : Rep[Vec[N,A]]
  def outer[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, A:Arith](a: Exp[Vec[R,A]], b: Exp[Vec[C,A]]) : Rep[Mat[R,C,A]]
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
      case v@VecObjNNew() => emitValDef(sym, remap(v.mV) + "()")
      // these are the ops that call through to the underlying real data structure
      case VecApply(x,n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
      case VecUpdate(x,n,y) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenVecOps extends BaseGenVecOps with CudaGenFat with CudaGenDataStruct {
  val IR: VecOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
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

