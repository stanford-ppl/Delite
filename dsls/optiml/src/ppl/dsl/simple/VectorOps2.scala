package ppl.dsl.simple

import ppl.delite.framework.{DSLType, DeliteApplication}
import java.io.PrintWriter
import scala.virtualization.lms.common.{Base, EffectExp, BaseExp}
import scala.virtualization.lms.internal.{CGenBase, ScalaGenBase, Effects}


trait Vector[T]

trait VectorOps2 extends DSLType with Base {

  object Vector {
    def zeros(len: Rep[Int]) : Rep[Vector[Double]] = vector_obj_zeros(len)
  }

  implicit def repVecToRepVecOps[A](x: Rep[Vector[A]]) = new vecRepCls(x)
  implicit def vecToRepVecOps[A](x: Vector[A]) = new vecRepCls(x)

  class vecRepCls[A](x: Rep[Vector[A]]) {
    def +(y: Rep[Vector[A]])(implicit mA: Manifest[A], n: Numeric[A]) = vector_plus(x,y)
    def pprint = vector_pprint(x)
  }

  // object defs
  def vector_obj_zeros(len: Rep[Int]): Rep[Vector[Double]]

  // class defs
  def vector_plus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_pprint[A](x: Rep[Vector[A]]): Rep[Unit]

}

trait VectorOpsExp2 extends VectorOps2 with EffectExp {
  case class VectorObjectZeros[A](n: Exp[Int]) extends Def[A]
  case class VectorPlus[A](x: Exp[Vector[A]], y: Exp[Vector[A]]) extends Def[Vector[A]]
  case class VectorPPrint[A](x: Exp[Vector[A]]) extends Def[Unit]

  def vector_obj_zeros(len: Exp[Int]) = reflectEffect(VectorObjectZeros(len))
  def vector_plus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlus(x, y)
  def vector_pprint[A](x: Exp[Vector[A]]) = reflectEffect(VectorPPrint(x))
}

trait ScalaGenVectorOps2 extends ScalaGenBase {
  val IR: VectorOpsExp2
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case VectorObjectZeros(s) => emitValDef(sym, "println(" + quote(s) + ")")
    case VectorPlus(x,y) => emitValDef(sym, quote(x) + " + " + quote(y))
    case VectorPPrint(a) => emitValDef(sym, "exit(" + quote(a) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

////code generation
trait CGenVectorOps2 extends CGenBase {
  val IR: VectorOpsExp2
  import IR._

  //code generation bit
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    //todo replace the manifest with embedded types
    case VectorObjectZeros(n) => emitConstDef("vector", sym, "Vector.zeros(" + quote(n) + ")")
    case VectorPlus(v1,v2) => emitConstDef("vector", sym, quote(v1) + " + " + quote (v2))
    case VectorPPrint(v) => emitConstDef("vector", sym, quote(v) + ".pprint()")
    case _ => super.emitNode(sym, rhs)    
  }
}