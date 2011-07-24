package ppl.dsl.deliszt.vec

import ppl.dsl.deliszt.datastruct.CudaGenDataStruct
import java.io.PrintWriter
import ppl.dsl.deliszt.datastruct.scala._

import ppl.delite.framework.DSLType
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}

trait VecOps extends DSLType with Variables with MetaInteger {
  this: DeLiszt =>

  object Vec {
    def apply[N<:IntM:Manifest:MVal,VT]() = vec_obj_n_new[N,VT]()
    def apply[VT:Manifest](a : Rep[VT]) = vec_obj_new[_1,VT](a)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT]) = vec_obj_new[_2,VT](a,b)
		/*def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT]) = vec_obj_new[_3,VT](a,b,c)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT]) = vec_obj_new[_4,VT](a,b,c,d)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT]) = vec_obj_new[_5,VT](a,b,c,d,e)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT]) = vec_obj_new[_6,VT](a,b,c,d,e,f)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT]) = vec_obj_new[_7,VT](a,b,c,d,e,f,g)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT]) = vec_obj_new[_8,VT](a,b,c,d,e,f,g,h)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT]) = vec_obj_new[_9,VT](a,b,c,d,e,f,g,h,i)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT]) = vec_obj_new[_10,VT](a,b,c,d,e,f,g,h,i,j)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT]) = vec_obj_new[_11,VT](a,b,c,d,e,f,g,h,i,j,k)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT]) = vec_obj_new[_12,VT](a,b,c,d,e,f,g,h,i,j,k,l)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT]) = vec_obj_new[_13,VT](a,b,c,d,e,f,g,h,i,j,k,l,m)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT]) = vec_obj_new[_14,VT](a,b,c,d,e,f,g,h,i,j,k,l,m,n)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT]) = vec_obj_new[_15,VT](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT], p: Rep[VT]) = vec_obj_new[_16,VT](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT], p: Rep[VT], q: Rep[VT]) = vec_obj_new[_17,VT](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT], p: Rep[VT], q: Rep[VT], r: Rep[VT]) = vec_obj_new[_18,VT](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT], p: Rep[VT], q: Rep[VT], r: Rep[VT], s: Rep[VT]) = vec_obj_new[_19,VT](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT], p: Rep[VT], q: Rep[VT], r: Rep[VT], s: Rep[VT], t: Rep[VT]) = vec_obj_new[_20,VT](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT], p: Rep[VT], q: Rep[VT], r: Rep[VT], s: Rep[VT], t: Rep[VT], u: Rep[VT]) = vec_obj_new[_21,VT](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)
		def apply[VT:Manifest](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT], p: Rep[VT], q: Rep[VT], r: Rep[VT], s: Rep[VT], t: Rep[VT], u: Rep[VT], v: Rep[VT]) = vec_obj_new[_22,VT](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)*/
  }

  implicit def repVecToVecOps[N<:IntM:Manifest:MVal, VT : Manifest](x: Rep[Vec[N, VT]]) = new vecOpsCls(x)
  implicit def varToVecOps[N<:IntM:Manifest:MVal, VT:Manifest](x: Var[Vec[N, VT]]) = new vecOpsCls(readVar(x))

  /**
   * This class defines the public interface for the Vec[T] class.
   */
  class vecOpsCls[N<:IntM:Manifest:MVal, VT:Manifest](u: Rep[Vec[N, VT]]) {
    type Self = Vec[N,VT]

    def x(implicit f : EnsureSize[_0,N]) = vec_apply(u, 0)
    def y(implicit f : EnsureSize[_1,N]) = vec_apply(u, 1)
    def z(implicit f : EnsureSize[_2,N]) = vec_apply(u, 2)
    def w(implicit f : EnsureSize[_3,N]) = vec_apply(u, 3)

    def apply(n: Rep[Int]) = vec_apply(u, n)
    def update(n: Rep[Int], c: Rep[VT]) = vec_update(u,n,c)
    
    def apply[TT<:IntM](n:TT)(implicit mv: MVal[TT], mm: Manifest[TT], f:EnsureSize[TT,N]) : Rep[VT] = vec_apply(u,MIntDepth[TT])
    def update[TT<:IntM](n:TT, v:Rep[VT])(implicit mv: MVal[TT], mm: Manifest[TT], f:EnsureSize[TT,N]) : Rep[Unit] = vec_update(u,MIntDepth[TT],v)

    def +(vt : Rep[Self])(implicit n: Arith[VT]) = vec_plus(u,vt)
    def -(vt : Rep[Self])(implicit n: Arith[VT]) = vec_minus(u,vt)
    def *(vt : Rep[Self])(implicit n: Arith[VT]) = vec_times(u,vt)
    def /(vt : Rep[Self])(implicit n: Arith[VT]) = vec_divide(u,vt)

    def *(vt : Rep[VT])(implicit n: Arith[VT], o: Overloaded1) = vec_times_scalar(u,vt)
    def /(vt : Rep[VT])(implicit n: Arith[VT], o: Overloaded1) = vec_divide_scalar(u,vt)

    def unary_-(implicit o : Arith[VT]) : Rep[Vec[N,VT]] = vec_negate(u)
    
    def size : Rep[Int]

    def &[N<:IntM:Manifest:MVal, M<:IntM:Manifest:MVal](rhs : Rep[Vec[M,VT]]) : Vec[N#Add[M],VT] = vec_concat(u, rhs)
  }

  /* Language ops */
  def cross[VT:Arith](a: Rep[Vec[_3,VT]], b: Rep[Vec[_3,VT]]) : Rep[Vec[_3,VT]]
  def dot[N<:IntM:Manifest:MVal, VT:Arith](a: Rep[Vec[N,VT]],b: Rep[Vec[N,VT]]) : Rep[VT]
  def normalize[N<:IntM:Manifest:MVal, VT:Arith](a: Rep[Vec[N,VT]]) : Rep[Vec[N,VT]]
  def length[N<:IntM:Manifest:MVal, VT](a: Rep[Vec[N,VT]]) : Rep[VT]
  def outer[R<:IntM:Manifest:MVal, C<:IntM:Manifest:MVal, VT:Arith](a: Rep[Vec[R,VT]], b: Rep[Vec[C,VT]]) : Rep[Mat[R,C,VT]]
  
/*
  class vecOpsClsMutable[VT:Manifest](vx: Var[Vec[N,VT]]) extends vecOpsCls[VT](readVar(vx)) {
    // ...
  }
*/
  def vec_obj_new[N<:IntM:Manifest:MVal, VT](xs: Rep[VT]*): Rep[Vec[N,VT]]
  def vec_obj_n_new[N<:IntM:Manifest:MVal, VT](): Rep[Vec[N,VT]]

  def vec_apply[N<:IntM:Manifest:MVal, VT:Manifest](x: Rep[Vec[N, VT]], i: Rep[Int]): Rep[VT]
  def vec_update[N<:IntM:Manifest:MVal, VT:Manifest](x: Rep[Vec[N, VT]], i: Rep[Int], v: Rep[VT]): Rep[Unit]
  def vec_plus[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Vec[N,VT]], y: Rep[Vec[N,VT]]): Rep[Vec[N,VT]]
  def vec_plus_scalar[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Vec[N,VT]], y: Rep[VT]): Rep[Vec[N,VT]]
  def vec_minus[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Vec[N,VT]], y: Rep[Vec[N,VT]]): Rep[Vec[N,VT]]
  def vec_minus_scalar[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Vec[N,VT]], y: Rep[VT]): Rep[Vec[N,VT]]
  def vec_times[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Vec[N,VT]], y: Rep[Vec[N,VT]]): Rep[Vec[N,VT]]
  def vec_times_scalar[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Vec[N,VT]], y: Rep[VT]): Rep[Vec[N,VT]]
  def vec_divide[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Vec[N,VT]], y: Rep[Vec[N,VT]]): Rep[Vec[N,VT]]
  def vec_divide_scalar[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Rep[Vec[N,VT]], y: Rep[VT]): Rep[Vec[N,VT]]

  def vec_size[N<:IntM:Manifest:MVal, VT:Manifest](x: Rep[Vec[N,VT]]): Rep[Int]
  
  def vec_negate[N<:IntM:Manifest:MVal, VT:Manifest](x: Rep[Vec[N,VT]]): Rep[Vec[N,VT]]
  def vec_concat[N<:IntM:Manifest:MVal, M<:IntM:Manifest:MVal, VT:Manifest](x: Rep[Vec[N,VT]], rhs: Rep[Vec[M,VT]]): Rep[Vec[N#Add[M],VT]]
}

trait CleanRoom {
}

trait VecOpsExp extends VecOps with VariablesExp with BaseFatExp with CleanRoom {
  this: VecImplOps with DeLisztExp =>

  def reflectPure[VT:Manifest](x: Def[VT]): Exp[VT] = toAtom(x) // TODO: just to make refactoring easier in case we want to change to reflectSomething

  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  case class VecObjNew[N<:IntM:Manifest:MVal,VT:Manifest](xs: Exp[VT]*) extends Def[Vec[N,VT]] {
    val mV = manifest[Vec[N,VT]]
    val mN = manifest[N]
  }

  case class VecObjNNew[N<:IntM:Manifest:MVal,VT:Manifest]() extends Def[Vec[N,VT]] {
    val mV = manifest[Vec[N,VT]]
    val mN = manifest[N]
  }

  case class VecApply[N<:IntM:Manifest:MVal,VT:Manifest](x: Exp[Vec[N,VT]], n: Exp[Int]) extends Def[VT]
  case class VecSize[N<:IntM:Manifest:MVal,VT:Manifest](x: Exp[Vec[N,VT]]) extends Def[Int]
  case class VecUpdate[N<:IntM:Manifest:MVal,VT:Manifest](x: Exp[Vec[N,VT]], n: Exp[Int], y: Exp[VT]) extends Def[Unit]

  case class VecConcat[N<:IntM:Manifest:MVal, M<:IntM:Manifest:MVal, VT:Manifest](x: Exp[Vec[N,VT]], y: Exp[Vec[M,VT]]) extends Def[Vec[N#Add[M],VT]]

  ////////////////////////////////
  // implemented via delite ops

  abstract class DeliteOpVecLoop[N<:IntM:Manifest:MVal, VT] extends DeliteOpLoop[Vec[N,VT]] {
    val size: Exp[Int] //inherited
  }
  
  abstract class VecMap[N<:IntM:Manifest:MVal,A:Manifest,B:Manifest](in: Exp[Vec[N,A]], func: Exp[A] => Exp[B])
    extends DeliteOpMap[A,B,Vec[N,B]] {

    def alloc = reifyEffects(Vec[N,B]())
    val size = in.size
  }

  case class VecNegate[N<:IntM:Manifest:MVal,VT:Manifest:Arith](in: Exp[Vec[N,VT]])
    extends VecMap[N,VT,VT](in, (v:Exp[VT]) => -v)

  case class VecPlus[N<:IntM:Manifest:MVal, VT:Manifest:Arith](inA: Exp[Vec[N,VT]], inB: Exp[Vec[N,VT]]) 
    extends DeliteOpVecLoop[N,VT] {

    val size = inA.size
    val v = fresh[Int]
    val body: Def[Vec[N,VT]] = DeliteCollectElem[VT,Vec[N,VT]](
      alloc = reifyEffects(Vec[N,VT]()),
      func = reifyEffects(inA(v) + inB(v))
    )
  }

  case class VecPlusScalar[N<:IntM:Manifest:MVal, VT:Manifest:Arith](in: Exp[Vec[N,VT]], y: Exp[VT])
    extends DeliteOpMap[VT,VT,Vec[N,VT]] {

    val alloc = reifyEffects(Vec[N,VT]())
    val v = fresh[VT]
    val func = reifyEffects(v + y)
  }

  case class VecMinus[N<:IntM:Manifest:MVal, VT:Manifest:Arith](inA: Exp[Vec[N,VT]], inB: Exp[Vec[N,VT]])
    extends DeliteOpVecLoop[N,VT] {

    val size = inA.size
    val v = fresh[Int]
    val body: Def[Vec[N,VT]] = DeliteCollectElem[VT,Vec[N,VT]](
      alloc = reifyEffects(Vec[N,VT]()),
      func = reifyEffects(inA(v) - inB(v))
    )
  }

  case class VecMinusScalar[N<:IntM:Manifest:MVal, VT:Manifest:Arith](in: Exp[Vec[N,VT]], y: Exp[VT])
    extends DeliteOpMap[VT,VT,Vec[N,VT]] {

    val alloc = reifyEffects(Vec[N,VT]())
    val v = fresh[VT]
    val func = reifyEffects(v - y)
  }

  abstract case class VecTimes[N<:IntM:Manifest:MVal, VT:Manifest:Arith](inA: Exp[Vec[N,VT]], inB: Exp[Vec[N,VT]]) extends DeliteOpVecLoop[N,VT] {
    def mev = manifest[VT]
    def aev = implicitly[Arith[VT]]
  }
  
  class VecTimesFresh[N<:IntM:Manifest:MVal, VT:Manifest:Arith](inA: Exp[Vec[N,VT]], inB: Exp[Vec[N,VT]]) extends VecTimes(inA, inB) {
    val size = inA.size
    val v = fresh[Int]
    val body: Def[Vec[N,VT]] = DeliteCollectElem[VT,Vec[N,VT]](
      alloc = reifyEffects(Vec[N,VT]()),
      func = reifyEffects(inA(v) * inB(v))
    )
  }

  abstract case class VecTimesScalar[N<:IntM:Manifest:MVal, VT:Manifest:Arith](in: Exp[Vec[N,VT]], y: Exp[VT]) extends DeliteOpVecLoop[N,VT] {
    def mev = manifest[VT]
    def aev = implicitly[Arith[VT]]
  }

  class VecTimesScalarFresh[N<:IntM:Manifest:MVal, VT:Manifest:Arith](in: Exp[Vec[N,VT]], y: Exp[VT]) extends VecTimesScalar[N,VT](in,y) {
    val size = in.size
    val v = fresh[Int]
    val body: Def[Vec[N,VT]] = DeliteCollectElem[VT,Vec[N,VT]](
      alloc = reifyEffects(Vec[N,VT]()),
      func = reifyEffects(in(v) * y)
    )
  }

  case class VecDivide[N<:IntM:Manifest:MVal, VT:Manifest:Arith](inA: Exp[Vec[N,VT]], inB: Exp[Vec[N,VT]])
    extends DeliteOpVecLoop[N,VT] {

    val size = inA.size
    val v = fresh[Int]
    val body: Def[Vec[N,VT]] = new DeliteCollectElem[VT,Vec[N,VT]](
      alloc = reifyEffects(Vec[N,VT]()),
      func = inA(v) / inB(v)
    )
  }

  case class VecDivideScalar[N<:IntM:Manifest:MVal, VT:Manifest:Arith](in: Exp[Vec[N,VT]], y: Exp[VT])
    extends DeliteOpVecLoop[N,VT] {

    val size = in.size
    val v = fresh[Int]
    val body: Def[Vec[N,VT]] = new DeliteCollectElem[VT,Vec[N,VT]](
      alloc = reifyEffects(Vec[N,VT]()),
      func = in(v) / y
    )
  }

  case class VecMin[N<:IntM:Manifest:MVal, VT:Manifest:Ordering](in: Exp[Vec[N,VT]])
    extends DeliteOpReduce[VT] {

    val v = (fresh[VT],fresh[VT])
    val func = reifyEffects(if (v._1 < v._2) v._1 else v._2)
  }

  case class VecMax[N<:IntM:Manifest:MVal, VT:Manifest:Ordering](in: Exp[Vec[N,VT]])
    extends DeliteOpReduce[VT] {

    val v = (fresh[VT],fresh[VT])
    val func = reifyEffects(if (v._1 > v._2) v._1 else v._2)
  }

  //////////////
  // mirroring

  /* override def mirror[VT:Manifest](e: Def[VT], f: Transformer): Exp[VT] = (e match {
    case VecApply(x, n) => vec_apply(f(x), f(n))
    case VecSize(x) => vec_size(f(x))
    // FIXME: VecSum might not actually be triggered because
    case e@VecSum(x) => toAtom(new VecSum(f(x))(e.mev,e.aev) { val size = f(e.size); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody[VT](e.body.asInstanceOf[Def[VT]], f) })
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
  }).asInstanceOf[Exp[VT]] // why?? */


  /////////////////////
  // object interface
  def vec_obj_new[N<:IntM:Manifest:MVal, VT:Manifest](xs: Exp[VT]*) = reflectMutable(VecObjNew[N,VT](xs:_*))
  def vec_obj_n_new[N<:IntM:Manifest:MVal, VT:Manifest]() = reflectMutable(VecObjNNew[N,VT]())

  /////////////////////
  // class interface
  def vec_apply[N<:IntM:Manifest:MVal, VT:Manifest](x: Exp[Vec[N,VT]], n: Exp[Int]) = reflectPure(VecApply(x, n))
  
  def vec_update[N<:IntM:Manifest:MVal, VT:Manifest](x: Exp[Vec[N, VT]], i: Exp[Int], v: Exp[VT]) = reflectWrite(x)(VecUpdate(x, n))

  def vec_plus[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[Vec[N,VT]]) = reflectPure(VecPlus(x,y))
  def vec_plus_scalar[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[VT]) = reflectPure(VecPlusScalar(x,y))
  def vec_minus[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[Vec[N,VT]]) = reflectPure(VecMinus(x,y))
  def vec_minus_scalar[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[VT]) = reflectPure(VecMinusScalar(x,y))
  def vec_times[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[Vec[N,VT]]) = reflectPure(new VecTimesFresh(x,y))
  def vec_times_scalar[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[VT]) = reflectPure(new VecTimesScalarFresh(x,y))

  def vec_divide[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[Vec[N,VT]]) = reflectPure(VecDivide(x,y))
  def vec_divide_scalar[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[VT]) = reflectPure(VecDivideScalar(x,y))
  
  def vec_negate[N<:IntM:Manifest:MVal, VT:Manifest:Arith](x: Exp[Vec[N,VT]]) = reflectPure(VecNegate(x))

  def vec_size[N<:IntM:Manifest:MVal, VT:Manifest](x: Exp[Vec[N,VT]]) = reflectPure(VecSize(x))
  def vec_concat[N<:IntM:Manifest:MVal, M<:IntM:Manifest:MVal, VT:Manifest](x: Exp[Vec[N,VT]], y: Exp[Vec[M,VT]]) = reflectPure(VecConcat(x,y))
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

