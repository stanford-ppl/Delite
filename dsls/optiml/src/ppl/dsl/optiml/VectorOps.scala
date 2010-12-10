package ppl.dsl.optiml

import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteCollection, DeliteApplication, DSLType}
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import reflect.Manifest
import scala.virtualization.lms.internal.{CudaGenBase, ScalaGenBase}

trait NilVector[T] extends Vector[T]
trait Vector[T] extends DeliteCollection[T] {
  // fields required on real underlying data structure impl
  def length : Int
  def is_row : Boolean
  def apply(n: Int) : T
  def update[A <: T](index: Int, x: A)

  // DeliteCollection
  def size = length
}

trait VectorOps extends DSLType with Variables { this: ArithImplicits =>

  object Vector {
    def apply[A : Manifest](len: Rep[Int], is_row : Rep[Boolean] = true) : Rep[Vector[A]] = vector_new(len, is_row)
    def zeros(len: Rep[Int]) : Rep[Vector[Double]] = vector_obj_zeros(len)
    def range(start: Rep[Int], end: Rep[Int], stride: Rep[Int] = 1, is_row: Rep[Boolean] = true) =
      vector_obj_range(start, end, stride, is_row)
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
    def +(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_plus(x,y)
    def -(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_minus(x,y)
    def *(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_times(x,y)
    def /(y: Rep[A])(implicit f: Fractional[A]) = vector_divide(x,y)
    def **(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_outer(x,y)
    def t = vector_trans(x)
    def pprint = vector_pprint(x)
    def is_row = vector_is_row(x)

    def +=(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_plusequals(x,y)
    def +=(y: Rep[A]) = vector_insert(x,x.length,y)

    def map[B:Manifest](f: Rep[A] => Rep[B]) = vector_map(x,f)
    def sum(implicit ops: ArithOps[A]) : Rep[A] = vector_sum(x)
  }

  def NilV[A:Manifest] = vector_nil

  // object defs
  def vector_obj_zeros(len: Rep[Int]): Rep[Vector[Double]]
  def vector_obj_range(start: Rep[Int], end: Rep[Int], stride: Rep[Int], is_row: Rep[Boolean]): Rep[Vector[Int]]  

  // class defs
  def vector_isinstanceof[A,B](x: Rep[Vector[A]], mA: Manifest[A], mB: Manifest[B]) : Rep[Boolean]
  def vector_apply[A:Manifest](x: Rep[Vector[A]], n: Rep[Int]): Rep[A]
  def vector_update[A:Manifest](x: Rep[Vector[A]], n: Rep[Int], y: Rep[A]): Rep[Unit]
  def vector_length[A:Manifest](x: Rep[Vector[A]]): Rep[Int]
  def vector_insert[A:Manifest](x: Rep[Vector[A]], pos: Rep[Int], y: Rep[A]): Rep[Vector[A]]
  def vector_is_row[A:Manifest](x: Rep[Vector[A]]): Rep[Boolean]
  def vector_toboolean[A](x: Rep[Vector[A]])(implicit conv: Rep[A] => Rep[Boolean], mA: Manifest[A]): Rep[Vector[Boolean]]
  def vector_plus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_plusequals[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_minus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_times[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_divide[A:Manifest:Fractional](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vector_trans[A:Manifest](x: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_outer[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Matrix[A]]
  def vector_pprint[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
  def vector_map[A:Manifest,B:Manifest](x: Rep[Vector[A]], f: Rep[A] => Rep[B]): Rep[Vector[B]]
  def vector_sum[A:Manifest:ArithOps](x: Rep[Vector[A]]) : Rep[A]

  def vector_nil[A:Manifest] : Rep[Vector[A]]

  // impl defs
  def vector_new[A:Manifest](len: Rep[Int], is_row: Rep[Boolean]) : Rep[Vector[A]]
}

trait VectorOpsExp extends VectorOps with VariablesExp with DSLOpsExp with DeliteOpsExp { this: VectorImplOps with ArithImplicits =>
  implicit def varToRepVecOps[A:Manifest](x: Var[Vector[A]]) = new vecRepCls(readVar(x))

  // implemented via method on real data structure
  case class VectorApply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) extends Def[A]
  case class VectorUpdate[A:Manifest](x: Exp[Vector[A]], n: Exp[Int], y: Exp[A]) extends Def[Unit]
  case class VectorLength[A:Manifest](x: Exp[Vector[A]]) extends Def[Int]
  case class VectorInsert[A:Manifest](x: Exp[Vector[A]], pos: Rep[Int], y: Exp[A]) extends Def[Vector[A]]
  case class VectorIsRow[A:Manifest](x: Exp[Vector[A]]) extends Def[Boolean]
  case class VectorNil[A](implicit mA: Manifest[A]) extends Def[Vector[A]]
  case class VectorIsInstanceOf[A,B](x: Exp[Vector[A]], mA: Manifest[A], mB: Manifest[B]) extends Def[Boolean]

  // implemented via kernel embedding
  case class VectorObjectZeros(len: Exp[Int])
    extends DSLOp(reifyEffects(vector_obj_zeros_impl(len)))

  case class VectorToBoolean[A](x: Exp[Vector[A]])(implicit conv: Exp[A] => Exp[Boolean], mA: Manifest[A])
    extends DSLOp(reifyEffects(vector_toboolean_impl[A](x,conv)))

  case class VectorPlus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_plus_impl[A](x,y)))

  case class VectorPlusEquals[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_plusequals_impl[A](x,y)))

  case class VectorMinus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_minus_impl[A](x,y)))

  case class VectorDivide[A:Manifest:Fractional](x: Exp[Vector[A]], y: Exp[A])
    extends DSLOp(reifyEffects(vector_divide_impl[A](x,y)))

  case class VectorOuter[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_outer_impl[A](x,y)))
  
  case class VectorPPrint[A:Manifest](x: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_pprint_impl[A](x)))

  case class VectorTrans[A:Manifest](x: Exp[Vector[A]])
      extends DSLOp(reifyEffects(vector_trans_impl[A](x)))

  case class VectorTimes[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]])
      extends Def[Vector[A]]

  case class VectorNew[A:Manifest](len: Exp[Int], is_row: Exp[Boolean])
    extends Def[Vector[A]] {
    val mV = manifest[Vector[A]]
  }

  case class VectorObjectRange(start: Exp[Int], end: Exp[Int], stride: Exp[Int], is_row: Exp[Boolean])
    extends Def[Vector[Int]]

  case class VectorMap[A:Manifest,B:Manifest](in: Exp[Vector[A]], out: Exp[Vector[B]], v: Exp[A], func: Exp[B])
    extends DeliteOpMap[A,B,Vector]
    //extends DSLOp(reifyEffects(vector_map_impl(x, f)))      

  case class VectorSum[A:Manifest:ArithOps](x: Exp[Vector[A]])
    extends DSLOp(reifyEffects(vector_sum_impl(x)))

  def vector_isinstanceof[A,B](x: Exp[Vector[A]], mA: Manifest[A], mB: Manifest[B]) = VectorIsInstanceOf(x,mA,mB)
  def vector_apply[A:Manifest](x: Exp[Vector[A]], n: Exp[Int]) = VectorApply(x, n)
  def vector_update[A:Manifest](x: Exp[Vector[A]], n: Exp[Int], y: Exp[A]) = reflectMutation(VectorUpdate(x,n,y))
  def vector_length[A:Manifest](x: Exp[Vector[A]]) = VectorLength(x)
  def vector_insert[A:Manifest](x: Exp[Vector[A]], pos: Rep[Int], y: Exp[A]) = reflectMutation(VectorInsert(x, pos, y))
  def vector_is_row[A:Manifest](x: Exp[Vector[A]]) = VectorIsRow(x)

  def vector_obj_zeros(len: Exp[Int]) = reflectEffect(VectorObjectZeros(len))
  def vector_toboolean[A](x: Exp[Vector[A]])(implicit conv: Exp[A] => Exp[Boolean], mA: Manifest[A]) = VectorToBoolean(x)
  def vector_plus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlus(x, y)
  def vector_plusequals[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectEffect(VectorPlusEquals(x, y))
  def vector_minus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorMinus(x, y)
  def vector_times[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorTimes(x, y)
  def vector_divide[A:Manifest:Fractional](x: Exp[Vector[A]], y: Exp[A]) = VectorDivide(x, y)
  def vector_trans[A:Manifest](x: Exp[Vector[A]]) = VectorTrans(x)
  def vector_outer[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorOuter(x, y)
  def vector_pprint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(VectorPPrint(x))

  def vector_nil[A:Manifest] = VectorNil[A]()

  def vector_new[A:Manifest](len: Exp[Int], is_row: Exp[Boolean]) = reflectEffect(VectorNew[A](len, is_row))

  def vector_obj_range(start: Exp[Int], end: Exp[Int], stride: Exp[Int], is_row: Exp[Boolean]) = reflectEffect(VectorObjectRange(start, end, stride, is_row))
  def vector_map[A:Manifest,B:Manifest](x: Exp[Vector[A]], f: Exp[A] => Exp[B]) = {
    val out = Vector[B](x.length, x.is_row)
    val v = fresh[A]
    val func = reifyEffects(f(v))
    VectorMap(x, out, v, func)
  }
  def vector_sum[A:Manifest:ArithOps](x: Exp[Vector[A]]) = VectorSum(x)
}

/**
 * Optimizations for composite VectorOps operations.
 */

trait VectorOpsExpOpt extends VectorOpsExp { this: VectorImplOps with ArithImplicits =>
  override def vector_plus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    // (TB + TD) == T(B + D)
    case (Def(VectorTimes(a, b)), Def(VectorTimes(c, d))) if (a == c) => VectorTimes[A](a.asInstanceOf[Exp[Vector[A]]], VectorPlus[A](b.asInstanceOf[Exp[Vector[A]]],d.asInstanceOf[Exp[Vector[A]]]))
    // ...
    case _ => super.vector_plus(x, y)
  }

  // allows sum to be expressive without actually doing pointless accumulations
  override def vector_plusequals[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    // remove runtime check on zero vector being same length as argument
    case (a, Def(VectorObjectZeros(len))) => a
    case (Def(VectorObjectZeros(len)), b) => b
    case _ => super.vector_plusequals(x,y)
  }

  override def vector_times[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
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
      case VectorIsRow(x)     => emitValDef(sym, quote(x) + ".is_row")
      case VectorInsert(x,pos,y) => emitValDef(sym, quote(x) + ".insert(" + quote(pos) + ", " + quote(y) + ")")
      case v@VectorNew(length, is_row) => emitValDef(sym, "new " + remap(v.mV) + "(" + quote(length) + "," + quote(is_row) + ")")
      case VectorObjectRange(start, end, stride, is_row) => emitValDef(sym, "new " + remap(manifest[Vector[Int]]) + "(" + quote(start) + "," + quote(end) + "," + quote(stride) + "," + quote(is_row) + ")")
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


trait CudaGenVectorOps extends CudaGenBase {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure

    case VectorDivide(x,y) =>
      gpuBlockSizeX = quote(x)+".length"
      stream.println(addTab()+"if( %s < %s ) {".format("idxX",quote(x)+".length"))
      tabWidth += 1
      stream.println(addTab()+"%s.update(%s, (%s.apply(%s))/%s);".format(quote(sym),"idxX",quote(x),"idxX",quote(y)))
      //if(varLink.contains(sym)) stream.println(addTab()+"%s.update(%s, %s.apply(%s));".format(quote(varLink.get(sym).get),"idxX",quote(sym),"idxX"))
      if(getVarLink(sym) != null) stream.println(addTab()+"%s.update(%s, %s.apply(%s));".format(quote(getVarLink(sym)),"idxX",quote(sym),"idxX"))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitVectorAlloc(sym,"%s.length".format(quote(x)),"%s.is_row".format(quote(x)))
    
    case VectorMinus(x,y) =>
      gpuBlockSizeX = quote(x)+".length"
      stream.println(addTab()+"if( %s < %s ) {".format("idxX",quote(x)+".length"))
      tabWidth += 1
      stream.println(addTab()+"%s.update(%s, (%s.apply(%s))-(%s.apply(%s)));".format(quote(sym),"idxX",quote(x),"idxX",quote(y),"idxX"))
      //if(varLink.contains(sym)) stream.println(addTab()+"%s.update(%s, %s.apply(%s));".format(quote(varLink.get(sym).get),"idxX",quote(sym),"idxX"))
      if(getVarLink(sym) != null) stream.println(addTab()+"%s.update(%s, %s.apply(%s));".format(quote(getVarLink(sym)),"idxX",quote(sym),"idxX"))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitVectorAlloc(sym,"%s.length".format(quote(x)),"%s.is_row".format(quote(x)))
    
    case VectorTrans(x) =>
      gpuBlockSizeX = quote(x)+".length"
      stream.println(addTab()+"if( %s < %s ) {".format("idxX",quote(x)+".length"))
      tabWidth += 1
      stream.println(addTab()+"%s.update(%s, %s.apply(%s));".format(quote(sym),"idxX",quote(x),"idxX"))
      //if(varLink.contains(sym)) stream.println(addTab()+"%s.update(%s, %s.apply(%s));".format(quote(varLink.get(sym).get),"idxX",quote(sym),"idxX"))
      if(getVarLink(sym) != null) stream.println(addTab()+"%s.update(%s, %s.apply(%s));".format(quote(getVarLink(sym)),"idxX",quote(sym),"idxX"))
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitVectorAlloc(sym,"%s.length".format(quote(x)),"!%s.is_row".format(quote(x)))

    case VectorOuter(x,y) =>
      gpuBlockSizeX = quote(x)+".length"
      stream.println(addTab()+"if( %s < %s ) {".format("idxX",quote(x)+".length"))
      tabWidth += 1
      stream.println(addTab()+"for(int i=0; i<%s.length; i++) {".format(quote(x))); tabWidth += 1
      stream.println(addTab()+"%s.update(%s, %s, %s.apply(%s)*%s.apply(%s));".format(quote(sym),"i","idxX",quote(x),"idxX",quote(y),"i"))
      //if(varLink.contains(sym)) stream.println(addTab()+"%s.update(%s, %s, %s.apply(%s,%s));".format(quote(varLink.get(sym).get),"i","idxX",quote(sym),"i","idxX"))
      if(getVarLink(sym) != null) stream.println(addTab()+"%s.update(%s, %s, %s.apply(%s,%s));".format(quote(getVarLink(sym)),"i","idxX",quote(sym),"i","idxX"))
      tabWidth -= 1; stream.println(addTab()+"}")
      tabWidth -= 1
      stream.println(addTab()+"}")
      emitMatrixAlloc(sym,"%s.length".format(quote(x)),"%s.length".format(quote(x)))

    case VectorPlusEquals(x,y) =>
      gpuBlockSizeX = quote(x)+".length"
      stream.println(addTab()+"if( %s < %s ) {".format("idxX",quote(x)+".length"))
      tabWidth += 1
      stream.println(addTab()+"%s.update(%s, (%s.apply(%s)) + (%s.apply(%s)));".format(quote(sym),"idxX",quote(x),"idxX",quote(y),"idxX"))
      //if(varLink.contains(sym)) stream.println(addTab()+"%s.update(%s, %s.apply(%s));".format(quote(varLink.get(sym).get),"idxX",quote(sym),"idxX"))
      if(getVarLink(sym) != null) stream.println(addTab()+"%s.update(%s, %s.apply(%s));".format(quote(getVarLink(sym)),"idxX",quote(sym),"idxX"))
      tabWidth -= 1
      stream.println(addTab()+"}") 
      emitVectorAllocSym(sym,x.asInstanceOf[Sym[_]])
    
    case VectorObjectZeros(len) =>
      throw new RuntimeException("CudaGen: Not GPUable")
    case VectorNew(len,is_row) =>
      throw new RuntimeException("CudaGen: Not GPUable")
    case VectorApply(x, n) =>
      emitValDef(sym, quote(x) + ".apply(" + quote(n) + ")")
    case VectorUpdate(x,n,y) =>
      stream.println(addTab() + "%s.update(%s,%s);".format(quote(x),quote(n),quote(y)))
    case VectorLength(x)    =>
      emitValDef(sym, quote(x) + ".length")
    case VectorIsRow(x)     =>
      emitValDef(sym, quote(x) + ".is_row")

    case _ => super.emitNode(sym, rhs)
  }
}

