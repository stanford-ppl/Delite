package ppl.dsl.deliszt.vec

import ppl.dsl.delizst.datastruct.CudaGenDataStruct
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
    def apply[VT](a : Rep[VT]) = vec_obj_new[_1](a)
		def apply[VT](a : Rep[VT], b : Rep[VT]) = vec_obj_new[_2](a,b)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT]) = vec_obj_new[_3](a,b,c)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT]) = vec_obj_new[_4](a,b,c,d)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT]) = vec_obj_new[_5](a,b,c,d,e)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT]) = vec_obj_new[_6](a,b,c,d,e,f)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT]) = vec_obj_new[_7](a,b,c,d,e,f,g)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT]) = vec_obj_new[_8](a,b,c,d,e,f,g,h)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT]) = vec_obj_new[_9](a,b,c,d,e,f,g,h,i)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT]) = vec_obj_new[_10](a,b,c,d,e,f,g,h,i,j)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT]) = vec_obj_new[_11](a,b,c,d,e,f,g,h,i,j,k)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT]) = vec_obj_new[_12](a,b,c,d,e,f,g,h,i,j,k,l)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT]) = vec_obj_new[_13](a,b,c,d,e,f,g,h,i,j,k,l,m)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT]) = vec_obj_new[_14](a,b,c,d,e,f,g,h,i,j,k,l,m,n)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT]) = vec_obj_new[_15](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT], p: Rep[VT]) = vec_obj_new[_16](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT], p: Rep[VT], q: Rep[VT]) = vec_obj_new[_17](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT], p: Rep[VT], q: Rep[VT], r: Rep[VT]) = vec_obj_new[_18](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT], p: Rep[VT], q: Rep[VT], r: Rep[VT], s: Rep[VT]) = vec_obj_new[_19](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT], p: Rep[VT], q: Rep[VT], r: Rep[VT], s: Rep[VT], t: Rep[VT]) = vec_obj_new[_20](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT], p: Rep[VT], q: Rep[VT], r: Rep[VT], s: Rep[VT], t: Rep[VT], u: Rep[VT]) = vec_obj_new[_21](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)
		def apply[VT](a : Rep[VT], b : Rep[VT], c : Rep[VT], d : Rep[VT], e : Rep[VT], f: Rep[VT], g: Rep[VT], h: Rep[VT], i: Rep[VT], j: Rep[VT], k: Rep[VT], l: Rep[VT], m: Rep[VT], n: Rep[VT], o: Rep[VT], p: Rep[VT], q: Rep[VT], r: Rep[VT], s: Rep[VT], t: Rep[VT], u: Rep[VT], w: Rep[VT]) = vec_obj_new[_22](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)
  }

  implicit def repVecToVecOps[N<:IntM, VT : Manifest](x: Rep[Vec[N, VT]]) = new vecOpsCls(x)
  implicit def varToVecOps[N<:IntM, VT : Manifest](x: Var[Vec[N, VT]]) = new vecOpsCls(readVar(x))

  /**
   * This class defines the public interface for the Vec[T] class.
   */
  class vecOpsCls[N<:IntM, VT : Manifest](x: Rep[Vec[N, VT]]) {
    type Self = Vec[N,VT]

		def x(implicit f : EnsureSize[_0,N]) = vec_apply(x, 0)
		def y(implicit f : EnsureSize[_1,N]) = vec_apply(x, 1)
		def z(implicit f : EnsureSize[_2,N]) = vec_apply(x, 2)
		def w(implicit f : EnsureSize[_3,N]) = vec_apply(x, 3)
		def apply[TT<:IntM](n : TT)(implicit f : EnsureSize[TT,N]) : VT = vec_apply(x, n)
		def update[TT<:IntM](n : TT, v : VT)(implicit f : EnsureSize[TT,N]) = vec_update(x,n,c)

		def apply(n: Rep[Int]) = vec_apply(x, n)
    def update(n: Rep[Int], c: Rep[VT]) = vec_update(x,n,c)

    def +(vt : Rep[Self])(implicit n: Numeric[VT]) = vec_plus(x,vt)
    def -(vt : Rep[Self])(implicit n: Numeric[VT]) = vec_minus(x,vt)
    def *(vt : Rep[Self])(implicit n: Numeric[VT]) = vec_times(x,vt)
    def /(vt : Rep[Self])(implicit n: Numeric[VT]) = vec_divide(x,vt)

    def *(vt : Rep[VT])(implicit n: Numeric[VT], o: Overloaded1) = vec_times_scalar(x,vt)
    def /(vt : Rep[VT])(implicit n: Numeric[VT], o: Overloaded1) = vec_divide_scalar(x,vt)

		def unary_-(implicit o : Numeric[VT]) : Vec[N,VT] = vec_negate(x)
		def min(vt : Self)(implicit f : VT => Numeric[VT]) : Vec[N,VT] = __
		def max(vt : Self)(implicit f : VT => Numeric[VT]) : Vec[N,VT] = __
    def min(implicit o: Ordering[VT]) = vec_min(x)
    def max(implicit o: Ordering[VT]) = vec_max(x)

		def &[TT<:IntM](rhs : Vec[TT,VT]) : Vec[N#Add[TT],VT] = vec_concat(x, rhs)
  }

  def cross[VT:Numeric](a: Rep[Vec[_3,VT]], b: Rep[Vec[_3,VT]]) : Rep[Vec[_3,VT]]
  def dot[N<:IntM, VT:Numeric](a: Rep[Vec[N,VT]],b: Rep[Vec[N,VT]]) : Rep[VT]
  def normalize[N<:IntM, VT:Numeric](a: Rep[Vec[N,VT]]) : Rep[Vec[N,VT]]
  def length[N<:IntM, VT](a: Rep[Vec[N,VT]]) : Rep[VT]
  def outer[R<:IntM, C<:IntM, VT:Numeric](a: Rep[Vec[R,VT]], b: Rep[Vec[C,VT]]) : Rep[Mat[R,C,VT]]
  
/*
  class vecOpsClsMutable[VT:Manifest](vx: Var[Vec[N,VT]]) extends vecOpsCls[VT](readVar(vx)) {
    // ...
  }
*/
  def vec_obj_new[N<:IntM, VT](xs: Rep[VT]*): Rep[Vec[N,VT]]

  def vec_apply[N<:IntM, VT:Manifest](x: Rep[Vec[N, VT]], i: Rep[Int]): Rep[VT]
  def vec_update[N<:IntM, VT:Manifest](x: Rep[Vec[N, VT]], i: Rep[Int], v: Rep[VT]): Rep[Unit]
  def vec_plus[N<:IntM, VT:Manifest:Numeric](x: Rep[Vec[N,VT]], y: Rep[Vec[N,VT]]): Rep[Vec[N,VT]]
  def vec_plus_scalar[N<:IntM, VT:Manifest:Numeric](x: Rep[Vec[N,VT]], y: Rep[N,VT]): Rep[Vec[N,VT]]
  def vec_minus[N<:IntM, VT:Manifest:Numeric](x: Rep[Vec[N,VT]], y: Rep[Vec[N,VT]]): Rep[Vec[N,VT]]
  def vec_minus_scalar[N<:IntM, VT:Manifest:Numeric](x: Rep[Vec[N,VT]], y: Rep[N,VT]): Rep[Vec[N,VT]]
  def vec_times[N<:IntM, VT:Manifest:Numeric](x: Rep[Vec[N,VT]], y: Rep[Vec[N,VT]]): Rep[Vec[N,VT]]
  def vec_times_scalar[N<:IntM, VT:Manifest:Numeric](x: Rep[Vec[N,VT]], y: Rep[N,VT]): Rep[Vec[N,VT]]
  def vec_divide[N<:IntM, VT:Manifest:Numeric](x: Rep[Vec[N,VT]], y: Rep[Vec[N,VT]]): Rep[Vec[N,VT]]
  def vec_divide_scalar[N<:IntM, VT:Manifest:Numeric](x: Rep[Vec[N,VT]], y: Rep[N,VT]): Rep[Vec[N,VT]]

  def vec_min[VT:Manifest:Ordering](x: Rep[Vec[N,VT]]): Rep[VT]
  def vec_max[VT:Manifest:Ordering](x: Rep[Vec[N,VT]]): Rep[VT]
}

trait CleanRoom {
}

trait VecOpsExp extends VecOps with VariablesExp with BaseFatExp with CleanRoom {
  this: VecImplOps with DeLisztExp =>

  def reflectPure[VT:Manifest](x: Def[VT]): Exp[VT] = toAtom(x) // TODO: just to make refactoring easier in case we want to change to reflectSomething

  ///////////////////////////////////////////////////
  // implemented via method on real data structure
  case class VecObjectNew[N<:IntM,VT:Manifest](xs: Array[Exp[VT]]) extends Def[Vec[N,VT]]
  case class VecApply[N<:IntM,VT:Manifest](x: Exp[Vec[N,VT]], n: Exp[Int]) extends Def[VT]
  case class VecLength[N<:IntM,VT:Manifest](x: Exp[Vec[N,VT]]) extends Def[Int]
  case class VecUpdate[N<:IntM,VT:Manifest](x: Exp[Vec[N,VT]], n: Exp[Int], y: Exp[VT]) extends Def[Unit]

  case class VecConcatenate[N<:IntM, VT:Manifest](x: Exp[Vec[N,VT]], y: Exp[Vec[N,VT]])
    extends DeliteOpSingleTask(reifyEffectsHere(vec_concatenate_impl(x,y)))

  ////////////////////////////////
  // implemented via delite ops

  abstract class DeliteOpVecLoop[N<:IntM, VT] extends DeliteOpLoop[Vec[N,VT]] {
    val size: Exp[Int] //inherited
  }

  case class VecPlus[N<:IntM, VT:Manifest:Arith](inA: Exp[Vec[N,VT]], inB: Exp[Vec[N,VT]]) 
    extends DeliteOpVecLoop[VT] {

    val size = inA.length
    val v = fresh[Int]
    val body: Def[Vec[N,VT]] = DeliteCollectElem[VT,Vec](
      alloc = reifyEffects(Vec[N,VT](size, isRow)),
      func = reifyEffects(inA(v) + inB(v))
    )
  }

  case class VecPlusScalar[N<:IntM, VT:Manifest:Arith](in: Exp[Vec[N,VT]], y: Exp[VT])
    extends DeliteOpMap[VT,VT,Vec] {

    val alloc = reifyEffects(Vec[N,VT](in.length, in.isRow))
    val v = fresh[VT]
    val func = reifyEffects(v + y)
  }

  case class VecMinus[N<:IntM, VT:Manifest:Arith](inA: Exp[Vec[N,VT]], inB: Exp[Vec[N,VT]])
    extends DeliteOpVecLoop[VT] {

    val size = inA.length
    val v = fresh[Int]
    val body: Def[Vec[N,VT]] = DeliteCollectElem[VT,Vec](
      alloc = reifyEffects(Vec[N,VT](size)),
      func = reifyEffects(inA(v) - inB(v))
    )
  }

  case class VecMinusScalar[N<:IntM, VT:Manifest:Arith](in: Exp[Vec[N,VT]], y: Exp[VT])
    extends DeliteOpMap[VT,VT,Vec] {

    val alloc = reifyEffects(Vec[N,VT](in.length, in.isRow))
    val v = fresh[VT]
    val func = reifyEffects(v - y)
  }

  abstract case class VecTimes[N<:IntM, VT:Manifest:Arith](inA: Exp[Vec[N,VT]], inB: Exp[Vec[N,VT]]) extends DeliteOpVecLoop[VT] {
    def mev = manifest[VT]
    def aev = implicitly[Arith[VT]]
  }
  
  class VecTimesFresh[N<:IntM, VT:Manifest:Arith](inA: Exp[Vec[N,VT]], inB: Exp[Vec[N,VT]]) extends VecTimes(inA, inB) {
    val size = inA.length
    val v = fresh[Int]
    val body: Def[Vec[N,VT]] = DeliteCollectElem[VT,Vec](
      alloc = reifyEffects(Vec[N,VT](size, isRow)),
      func = reifyEffects(inA(v) * inB(v))
    )
  }

  abstract case class VecTimesScalar[N<:IntM, VT:Manifest:Arith](in: Exp[Vec[N,VT]], y: Exp[VT]) extends DeliteOpVecLoop[VT] {
    def mev = manifest[VT]
    def aev = implicitly[Arith[VT]]
  }

  class VecTimesScalarFresh[N<:IntM, VT:Manifest:Arith](in: Exp[Vec[N,VT]], y: Exp[VT]) extends VecTimesScalar[VT](in,y) {
    val size = in.length
    val v = fresh[Int]
    val body: Def[Vec[N,VT]] = DeliteCollectElem[VT,Vec](
      alloc = reifyEffects(Vec[N,VT](size, isRow)),
      func = reifyEffects(in(v) * y)
    )
  }

  case class VecDivide[N<:IntM, VT:Manifest:Arith](inA: Exp[Vec[N,VT]], inB: Exp[Vec[N,VT]])
    extends DeliteOpVecLoop[VT] {

    val size = inA.length
    val v = fresh[Int]
    val body: Def[Vec[N,VT]] = new DeliteCollectElem[VT,Vec](
      alloc = reifyEffects(Vec[N,VT](size, isRow)),
      func = inA(v) / inB(v)
    )
  }

  case class VecDivideScalar[N<:IntM, VT:Manifest:Arith](in: Exp[Vec[N,VT]], y: Exp[VT])
    extends DeliteOpVecLoop[VT] {

    val size = in.length
    val v = fresh[Int]
    val body: Def[Vec[N,VT]] = new DeliteCollectElem[VT,Vec](
      alloc = reifyEffects(Vec[N,VT](size, isRow)),
      func = in(v) / y
    )
  }

  case class VecMin[N<:IntM, VT:Manifest:Ordering](in: Exp[Vec[N,VT]])
    extends DeliteOpReduce[VT] {

    val v = (fresh[VT],fresh[VT])
    val func = reifyEffects(if (v._1 < v._2) v._1 else v._2)
  }

  case class VecMax[N<:IntM, VT:Manifest:Ordering](in: Exp[Vec[N,VT]])
    extends DeliteOpReduce[VT] {

    val v = (fresh[VT],fresh[VT])
    val func = reifyEffects(if (v._1 > v._2) v._1 else v._2)
  }

  //////////////
  // mirroring

  override def mirror[VT:Manifest](e: Def[VT], f: Transformer): Exp[VT] = (e match {
    case VecApply(x, n) => vec_apply(f(x), f(n))
    case VecLength(x) => vec_length(f(x))
    case VecIsRow(x) => vec_isRow(f(x))
    // FIXME: VecSum might not actually be triggered because
    case e@VecSum(x) => toAtom(new VecSum(f(x))(e.mev,e.aev) { val size = f(e.size); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody[VT](e.body.asInstanceOf[Def[VT]], f) })
    case e@VecTimes(x,y) => toAtom(new VecTimes(f(x),f(y))(e.mev,e.aev) { val size = f(e.size); val isRow = f(e.isRow); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case e@VecTimesScalar(x,y) => toAtom(new VecTimesScalar(f(x),f(y))(e.mev,e.aev) { val size = f(e.size); val isRow = f(e.isRow); val v = f(e.v).asInstanceOf[Sym[Int]]; val body = mirrorLoopBody(e.body, f) })
    case Reflect(e@VecPPrint(x), u, es) => reflectMirrored(Reflect(VecPPrint(f(x))(f(e.block)), mapOver(f,u), f(es)))
    // below are read/write effects TODO: find a general approach to treating them!!!!
    case Reflect(VecApply(l,r), u, es) => reflectMirrored(Reflect(VecApply(f(l),f(r)), mapOver(f,u), f(es)))
    case Reflect(VecLength(x), u, es) => reflectMirrored(Reflect(VecLength(f(x)), mapOver(f,u), f(es)))
    case Reflect(VecIsRow(x), u, es) => reflectMirrored(Reflect(VecIsRow(f(x)), mapOver(f,u), f(es)))
    case Reflect(VecForeach(a,b,c), u, es) => reflectMirrored(Reflect(VecForeach(f(a),f(b).asInstanceOf[Sym[Int]],f(c)), mapOver(f,u), f(es)))
    // FIXME: problem with VecTimes: it's actually a loop and if it is reflected it means a.length will also reflect and we have no context here!!!
    case Reflect(e2@VecTimes(a,b), u, es) => error("we'd rather not mirror " + e); //reflectMirrored(Reflect(VecTimes(f(a),f(b))(e.mev,e.aev), Read(f onlySyms rs), f(es)))
    case Reflect(VecUpdate(l,i,r), u, es) => reflectMirrored(Reflect(VecUpdate(f(l),f(i),f(r)), mapOver(f,u), f(es)))
    // allocations TODO: generalize
    case Reflect(VecObjectZeros(x), u, es) => reflectMirrored(Reflect(VecObjectZeros(f(x)), mapOver(f,u), f(es)))
    case Reflect(VecObjectRange(s,e,d,r), u, es) => reflectMirrored(Reflect(VecObjectRange(f(s),f(e),f(d),f(r)), mapOver(f,u), f(es)))
    case Reflect(e@VecNew(l,r), u, es) => reflectMirrored(Reflect(VecNew(f(l),f(r))(e.mV), mapOver(f,u), f(es)))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[VT]] // why??


  /////////////////////
  // object interface
  def vec_obj_new[N<:IntM, VT:Manifest](xs: Exp[VT]*) = reflectMutable(VecObjNew[N,VT](xs))

  /////////////////////
  // class interface
  def vec_apply[VT:Manifest](x: Exp[Vec[N,VT]], n: Exp[Int]) = reflectPure(VecApply(x, n))


  def vec_concatenate[VT:Manifest](x: Exp[Vec[N,VT]], y: Exp[Vec[N,VT]]) = reflectPure(VecConcatenate(x,y))

  def vec_plus[VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[Vec[N,VT]]) = reflectPure(VecPlus(x,y))
  def vec_plus_scalar[VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[VT]) = reflectPure(VecPlusScalar(x,y))
  def vec_minus[VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[Vec[N,VT]]) = reflectPure(VecMinus(x,y))
  def vec_minus_scalar[VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[VT]) = reflectPure(VecMinusScalar(x,y))
  def vec_times[VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[Vec[N,VT]]) = reflectPure(new VecTimesFresh(x,y))
  def vec_times_scalar[VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[VT]) = reflectPure(new VecTimesScalarFresh(x,y))

  def vec_divide[VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[Vec[N,VT]]) = reflectPure(VecDivide(x,y))
  def vec_divide_scalar[VT:Manifest:Arith](x: Exp[Vec[N,VT]], y: Exp[VT]) = reflectPure(VecDivideScalar(x,y))

  def vec_min[VT:Manifest:Ordering](x: Exp[Vec[N,VT]]) = reflectPure(VecMin(x))
  def vec_max[VT:Manifest:Ordering](x: Exp[Vec[N,VT]]) = reflectPure(VecMax(x))
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

