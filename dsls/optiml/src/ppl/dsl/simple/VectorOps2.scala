package ppl.dsl.simple

import ppl.delite.framework.{DSLType, DeliteApplication}
import java.io.PrintWriter
import scala.virtualization.lms.common.{Base, BaseExp, BaseFatExp, EffectExp, CGenBase, CGenFat, ScalaGenBase, ScalaGenFat}
import scala.virtualization.lms.internal.Effects
import ppl.delite.framework.codegen.delite.DeliteCodegen
import ppl.delite.framework.ops.DeliteOpsExp


trait Vector[T]

trait VectorOps2 extends DSLType with Base {

  object Vector {
    def doubleZeros(len: Rep[Int]) : Rep[Vector[Double]] = vector_obj_double_zeros(len)
    def intZeros(len: Rep[Int]): Rep[Vector[Int]] = vector_obj_int_zeros(len)
  }

  implicit def repVecToRepVecOps[A:Manifest](x: Rep[Vector[A]]) = new vecRepCls(x)
  implicit def vecToRepVecOps[A:Manifest](x: Vector[A]) = new vecRepCls(x)

  class vecRepCls[A:Manifest](x: Rep[Vector[A]]) {
    def +(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_plus(x,y)
    def -(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_minus(x,y)
    def apply(i: Rep[Int]) = vector_apply(x,i)
    def update(i: Rep[Int], v: Rep[A]) = vector_update(x,i,v)
    def length = vector_length(x)
    def pprint = vector_pprint(x)
  }

  // object defs
  def vector_obj_double_zeros(len: Rep[Int]): Rep[Vector[Double]]
  def vector_obj_int_zeros(len: Rep[Int]): Rep[Vector[Int]]

  // class defs
  def vector_plus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_minus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_apply[A:Manifest](x:Rep[Vector[A]], i: Rep[Int]): Rep[A]
  def vector_update[A:Manifest](x: Rep[Vector[A]], i: Rep[Int], v: Rep[A]): Rep[Unit]
  def vector_length[A:Manifest](x: Rep[Vector[A]]): Rep[Int]
  def vector_pprint[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]

}

trait VectorOpsExp2 extends VectorOps2 with DeliteOpsExp {
  case class VectorObjectDoubleZeros[A:Manifest](n: Exp[Int]) extends Def[Vector[Double]]
  case class VectorObjectIntZeros[A:Manifest](n: Exp[Int]) extends Def[Vector[Int]]
  case class VectorPlus[A:Manifest](x: Exp[Vector[A]], y: Exp[Vector[A]]) extends Def[Vector[A]]
  case class VectorMinus[A:Manifest](x: Exp[Vector[A]], y: Exp[Vector[A]]) extends Def[Vector[A]]
  case class VectorApply[A:Manifest](x: Exp[Vector[A]], i: Exp[Int]) extends Def[A]
  case class VectorUpdate[A:Manifest](x: Exp[Vector[A]], i: Exp[Int], v: Exp[A]) extends Def[Unit]
  case class VectorLength[A:Manifest](x: Exp[Vector[A]]) extends Def[Int]
  case class VectorPPrint[A:Manifest](x: Exp[Vector[A]]) extends Def[Unit]

  def vector_obj_double_zeros(len: Exp[Int]) = reflectEffect(VectorObjectDoubleZeros(len))
  def vector_obj_int_zeros(len: Exp[Int]) = reflectEffect(VectorObjectIntZeros(len))
  def vector_plus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlus(x, y)
  def vector_minus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorMinus(x, y)
  def vector_apply[A:Manifest](x: Exp[Vector[A]], i: Rep[Int]) = VectorApply(x,i)
  def vector_update[A:Manifest](x: Rep[Vector[A]], i: Rep[Int], v: Rep[A]) = reflectWrite(x)(x)(VectorUpdate(x, i, v))
  def vector_length[A:Manifest](x: Rep[Vector[A]]) = VectorLength(x)
  def vector_pprint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(VectorPPrint(x))
}

trait ScalaGenVectorOps2 extends ScalaGenFat {
  val IR: VectorOpsExp2
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case VectorObjectDoubleZeros(s) => emitValDef(sym, "Vector.doubleZeros(" + quote(s) + ")")
    case VectorObjectIntZeros(s) => emitValDef(sym, "Vector.intZeros(" + quote(s) + ")")
    case VectorPlus(x,y) => emitValDef(sym, quote(x) + " + " + quote(y))
    case VectorMinus(x,y) => emitValDef(sym, quote(x) + " - " + quote(y))
    case VectorUpdate(x,i,v) => emitValDef(sym, quote(x) + "(" + quote(i) + ") = " + quote(v))
    case VectorApply(x,i) => emitValDef(sym, quote(x) + "(" + quote(i) + ")")
    case VectorLength(x) => emitValDef(sym, quote(x) + ".length")
    case VectorPPrint(a) => emitValDef(sym, quote(a) + ".pprint")
    case _ => super.emitNode(sym, rhs)
  }
}

/*
////code generation
trait CGenVectorOps2 extends CGenFat {
  val IR: VectorOpsExp2
  import IR._

  //code generation bit
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    //todo replace the manifest with embedded types
    case VectorObjectDoubleZeros(n) => emitConstDef("vector", sym, "Vector.doubleZeros(" + quote(n) + ")")
    case VectorPlus(v1,v2) => emitConstDef("vector", sym, quote(v1) + " + " + quote (v2))
    case VectorPPrint(v) => emitConstDef("vector", sym, quote(v) + ".pprint()")
    case _ => super.emitNode(sym, rhs)    
  }
}
*/
