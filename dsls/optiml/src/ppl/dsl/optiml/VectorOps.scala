package ppl.dsl.optiml

import datastruct.CudaGenDataStruct
import datastruct.scala.{Vector, Matrix, VectorImpl, RangeVectorImpl}
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.Manifest
import scala.virtualization.lms.internal.{CudaGenBase, ScalaGenBase}
import scala.virtualization.lms.common._

trait VectorOps extends DSLType with Variables {
  this: ArithOps =>

  object Vector {
    def apply[A : Manifest](len: Rep[Int], isRow : Rep[Boolean] = true) : Rep[Vector[A]] = vector_new(len, isRow)
    def zeros(len: Rep[Int]) : Rep[Vector[Double]] = vector_obj_zeros(len)
    def range(start: Rep[Int], end: Rep[Int], stride: Rep[Int] = 1, isRow: Rep[Boolean] = true) =
      vector_obj_range(start, end, stride, isRow)
  }

  implicit def repVecToRepVecOps[A:Manifest](x: Rep[Vector[A]]) = new vecRepCls(x)
  implicit def vecToRepVecOps[A:Manifest](x: Vector[A]) = new vecRepCls(x)
  implicit def varToRepVecOps[A:Manifest](x: Var[Vector[A]]) : vecRepCls[A]

  // could convert to infix, but apply doesn't work with it anyways yet
  class vecRepCls[A:Manifest](x: Rep[Vector[A]]) {
    def isInstanceOfL[B](implicit mB: Manifest[B]) : Rep[Boolean] = vector_isinstanceof(x,manifest[A],mB)
    def apply(n: Rep[Int]) = vector_apply(x, n)
    def update(n: Rep[Int], y: Rep[A]) = vector_update(x,n,y)
    def length = vector_length(x)
    def toBoolean(implicit conv: Rep[A] => Rep[Boolean]) = vector_toboolean(x)
    def +(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_plus(x,y)
    def -(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_minus(x,y)
    def **(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_times(x,y)
    def /(y: Rep[Vector[A]])(implicit a: Arith[A], o: Overloaded1) = vector_divide(x,y)
    def /(y: Rep[A])(implicit a: Arith[A]) = vector_divide_scalar(x,y)    
    def *(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_outer(x,y)
    def t = vector_trans(x)
    def pprint = vector_pprint(x)
    def isRow = vector_isRow(x)
    def foreach(block: Rep[A] => Rep[Unit]) = vector_foreach(x, block)

    def +=(y: Rep[Vector[A]])(implicit a: Arith[A]) = vector_plusequals(x,y)
    def +=(y: Rep[A]) = vector_insert(x,x.length,y)

    def map[B:Manifest](f: Rep[A] => Rep[B]) = vector_map(x,f)
    def sum(implicit a: Arith[A]) : Rep[A] = vector_sum(x)
  }

  def NilV[A:Manifest] = vector_nil

  // object defs
  def vector_obj_zeros(len: Rep[Int]): Rep[Vector[Double]]
  def vector_obj_range(start: Rep[Int], end: Rep[Int], stride: Rep[Int], isRow: Rep[Boolean]): Rep[Vector[Int]]

  // class defs
  def vector_isinstanceof[A,B](x: Rep[Vector[A]], mA: Manifest[A], mB: Manifest[B]) : Rep[Boolean]
  def vector_apply[A:Manifest](x: Rep[Vector[A]], n: Rep[Int]): Rep[A]
  def vector_update[A:Manifest](x: Rep[Vector[A]], n: Rep[Int], y: Rep[A]): Rep[Unit]
  def vector_length[A:Manifest](x: Rep[Vector[A]]): Rep[Int]
  def vector_insert[A:Manifest](x: Rep[Vector[A]], pos: Rep[Int], y: Rep[A]): Rep[Vector[A]]
  def vector_isRow[A:Manifest](x: Rep[Vector[A]]): Rep[Boolean]
  def vector_toboolean[A](x: Rep[Vector[A]])(implicit conv: Rep[A] => Rep[Boolean], mA: Manifest[A]): Rep[Vector[Boolean]]
  def vector_plus[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_plusequals[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_minus[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_times[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_divide[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_divide_scalar[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vector_trans[A:Manifest](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_outer[A:Manifest:Arith](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Matrix[A]]
  def vector_pprint[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
  def vector_map[A:Manifest,B:Manifest](x: Rep[Vector[A]], f: Rep[A] => Rep[B]): Rep[Vector[B]]
  def vector_sum[A:Manifest:Arith](x: Rep[Vector[A]]) : Rep[A]
  def vector_foreach[A:Manifest](x: Rep[Vector[A]], block: Rep[A] => Rep[Unit]): Rep[Unit]

  def vector_nil[A:Manifest] : Rep[Vector[A]]

  // impl defs
  def vector_new[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]) : Rep[Vector[A]]
}

trait VectorOpsExp extends VectorOps with VariablesExp {

  this: VectorImplOps with OptiMLExp =>

  implicit def varToRepVecOps[A:Manifest](x: Var[Vector[A]]) = new vecRepCls(readVar(x))

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class VectorApply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) extends Def[A]
  case class VectorUpdate[A:Manifest](x: Exp[Vector[A]], n: Exp[Int], y: Exp[A]) extends Def[Unit]
  case class VectorLength[A:Manifest](x: Exp[Vector[A]]) extends Def[Int]
  case class VectorInsert[A:Manifest](x: Exp[Vector[A]], pos: Rep[Int], y: Exp[A]) extends Def[Vector[A]]
  case class VectorIsRow[A:Manifest](x: Exp[Vector[A]]) extends Def[Boolean]
  case class VectorNil[A](implicit mA: Manifest[A]) extends Def[Vector[A]]
  case class VectorIsInstanceOf[A,B](x: Exp[Vector[A]], mA: Manifest[A], mB: Manifest[B]) extends Def[Boolean]
  case class VectorNew[A:Manifest](len: Exp[Int], isRow: Exp[Boolean])
    extends Def[Vector[A]] {
    val mV = manifest[VectorImpl[A]]
  }
  case class VectorObjectRange(start: Exp[Int], end: Exp[Int], stride: Exp[Int], isRow: Exp[Boolean])
    extends Def[Vector[Int]]



  //////////////////////////////////////
  // implemented via kernel embedding

  case class VectorObjectZeros(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffects(vector_obj_zeros_impl(len)))

  case class VectorToBoolean[A](x: Exp[Vector[A]])(implicit conv: Exp[A] => Exp[Boolean], mA: Manifest[A])
    extends DeliteOpSingleTask(reifyEffects(vector_toboolean_impl[A](x,conv)))

  case class VectorOuter[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffects(vector_outer_impl[A](x,y)))

  case class VectorPPrint[A:Manifest](x: Exp[Vector[A]])
    extends DeliteOpSingleTask(reifyEffects(vector_pprint_impl[A](x)))

  //case class VectorTrans[A:Manifest](x: Exp[Vector[A]])
  //  extends DeliteOpSingleTask(reifyEffects(vector_trans_impl[A](x)))



  ////////////////////////////////
  // implemented via delite ops

  case class VectorTrans[A:Manifest](in: Exp[Vector[A]])
    extends DeliteOpMap[A,A,Vector] {
    
    val alloc = reifyEffects(vector_new[A](in.length, !in.isRow))
    val v = fresh[A]
    val func = v 
  }

  case class VectorPlus[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector] {

    //val out = reifyEffects(Vector[A](inA.length, inA.isRow))
    val alloc = reifyEffects(vector_new[A](inA.length, inA.isRow))
    val v = (fresh[A],fresh[A])
    val func = v._1 + v._2
  }

  case class VectorPlusEquals[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector] {

    //val out = inA
    val alloc = inA
    val v = (fresh[A],fresh[A])
    val func = v._1 + v._2
  }

  case class VectorMinus[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector] {

    //val out = reifyEffects(Vector[A](inA.length, inA.isRow))
    val alloc = reifyEffects(vector_new[A](inA.length, inA.isRow))
    val v = (fresh[A],fresh[A])
    val func = v._1 - v._2
  }

  case class VectorTimes[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector] {

    //val out = reifyEffects(Vector[A](inA.length, inA.isRow))
    val alloc = reifyEffects(vector_new[A](inA.length, inA.isRow))
    val v = (fresh[A],fresh[A])
    val func = v._1 * v._2
  }

  case class VectorDivide[A:Manifest:Arith](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector] {

    //val out = reifyEffects(Vector[A](inA.length, inA.isRow))
    val alloc = reifyEffects(vector_new[A](inA.length, inA.isRow))
    val v = (fresh[A],fresh[A])
    val func = v._1 / v._2
  }

  case class VectorDivideScalar[A:Manifest:Arith](in: Exp[Vector[A]], y: Exp[A])
    extends DeliteOpMap[A,A,Vector] {

    //val out = Vector[A](in.length, in.isRow)
    val alloc = reifyEffects(vector_new[A](in.length, in.isRow))
    val v = fresh[A]
    val func = v / y
  }

  case class VectorSum[A:Manifest:Arith](in: Exp[Vector[A]])
    extends DeliteOpReduce[A] {

    val v = (fresh[A],fresh[A])
    val func = v._1 + v._2
  }

  case class VectorMap[A:Manifest,B:Manifest](in: Exp[Vector[A]], v: Exp[A], func: Exp[B])
    extends DeliteOpMap[A,B,Vector] {

    //val out = Vector[B](in.length, in.isRow)
    val alloc = reifyEffects(vector_new[B](in.length, in.isRow))
  }

  case class VectorForeach[A:Manifest](in: Exp[Vector[A]], v: Exp[A], func: Exp[Unit])
    extends DeliteOpForeach[A,Vector] {

    val i = fresh[Int]
    val sync = reifyEffects(if ((i > 0) && (i < in.length)) List(in(i-1),in(i),in(i+1)) else List(in(i)))
  }

  //////////////
  // interface

  def vector_isinstanceof[A,B](x: Exp[Vector[A]], mA: Manifest[A], mB: Manifest[B]) = VectorIsInstanceOf(x,mA,mB)
  def vector_apply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) = VectorApply(x, n)
  def vector_update[A:Manifest](x: Exp[Vector[A]], n: Exp[Int], y: Exp[A]) = reflectMutation(VectorUpdate(x,n,y))
  def vector_length[A:Manifest](x: Exp[Vector[A]]) = VectorLength(x)
  def vector_insert[A:Manifest](x: Exp[Vector[A]], pos: Rep[Int], y: Exp[A]) = reflectMutation(VectorInsert(x, pos, y))
  def vector_isRow[A:Manifest](x: Exp[Vector[A]]) = VectorIsRow(x)

  def vector_obj_zeros(len: Exp[Int]) = reflectEffect(VectorObjectZeros(len))
  def vector_toboolean[A](x: Exp[Vector[A]])(implicit conv: Exp[A] => Exp[Boolean], mA: Manifest[A]) = VectorToBoolean(x)
  def vector_plus[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlus(x, y)
  def vector_plusequals[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectMutation(VectorPlusEquals(x, y))
  def vector_minus[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorMinus(x,y)
  def vector_times[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorTimes(x, y)
  def vector_divide[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorDivide(x, y)
  def vector_divide_scalar[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[A]) = VectorDivideScalar(x, y)
  def vector_trans[A:Manifest](x: Exp[Vector[A]]) = VectorTrans(x)
  def vector_outer[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorOuter(x, y)
  def vector_pprint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(VectorPPrint(x))

  def vector_nil[A:Manifest] = VectorNil[A]()

  def vector_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) = reflectEffect(VectorNew[A](len, isRow))

  def vector_obj_range(start: Exp[Int], end: Exp[Int], stride: Exp[Int], isRow: Exp[Boolean]) = reflectEffect(VectorObjectRange(start, end, stride, isRow))
  def vector_map[A:Manifest,B:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[B]) = {
    val v = fresh[A]
    val func = reifyEffects(f(v))
    VectorMap(x, v, func)
  }
  def vector_sum[A:Manifest:Arith](x: Exp[Vector[A]]) = VectorSum(x)
  def vector_foreach[A:Manifest](x: Rep[Vector[A]], block: Rep[A] => Rep[Unit]) = {
    val v = fresh[A]
    val func = reifyEffects(block(v))
    reflectEffect(VectorForeach(x, v, func))
  }

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
}

trait ScalaGenVectorOps extends ScalaGenBase {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      // these are the ops that call through to the underlying real data structure
      case VectorIsInstanceOf(x,mA,mB) => emitValDef(sym, quote(x) + ".isInstanceOf[" + remap(mB) + "]")
      case VectorApply(x, n) => emitValDef(sym, quote(x) + "(" + quote(n) + ")")
      case VectorUpdate(x,n,y) => emitValDef(sym, quote(x) + "(" + quote(n) + ") = " + quote(y))
      case VectorLength(x)    => emitValDef(sym, quote(x) + ".length")
      case VectorIsRow(x)     => emitValDef(sym, quote(x) + ".isRow")
      case VectorInsert(x,pos,y) => emitValDef(sym, quote(x) + ".insert(" + quote(pos) + ", " + quote(y) + ")")
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


trait CudaGenVectorOps extends CudaGenBase with CudaGenDataStruct {
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
      throw new RuntimeException("CudaGen: Not GPUable (Dynamic memory allocation is not allowed)")
    case VectorNew(len,isRow) =>
      throw new RuntimeException("CudaGen: Not GPUable (Dynamic memory allocation is not allowed)")
    case VectorApply(x, n) =>
      emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case VectorUpdate(x,n,y) =>
      stream.println(addTab() + "%s.update(%s,%s);".format(quote(x),quote(n),quote(y)))
    case VectorLength(x)    =>
      emitValDef(sym, quote(x) + ".length")
    case VectorIsRow(x)     =>
      emitValDef(sym, quote(x) + ".isRow")

    case _ => super.emitNode(sym, rhs)
  }
}

