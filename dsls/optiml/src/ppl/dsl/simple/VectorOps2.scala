package ppl.dsl.simple

import ppl.delite.framework.{DSLType, DeliteApplication}
import java.io.PrintWriter
import scala.virtualization.lms.common.{Base, EffectExp, BaseExp}
import scala.virtualization.lms.internal.{CGenBase, ScalaGenBase, Effects}
import ppl.delite.framework.codegen.delite.DeliteCodegen


trait Vector[T]

trait VectorOps2 extends DSLType with Base {

  object Vector {
    def zeros(len: Rep[Int]) : Rep[Vector[Double]] = vector_obj_zeros(len)
  }

  implicit def repVecToRepVecOps[A:Manifest](x: Rep[Vector[A]]) = new vecRepCls(x)
  implicit def vecToRepVecOps[A:Manifest](x: Vector[A]) = new vecRepCls(x)

  class vecRepCls[A:Manifest](x: Rep[Vector[A]]) {
    def +(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vector_plus(x,y)
    def pprint = vector_pprint(x)
  }

  // object defs
  def vector_obj_zeros(len: Rep[Int]): Rep[Vector[Double]]

  // class defs
  def vector_plus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vector_pprint[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]

}

trait VectorOpsExp2 extends VectorOps2 with EffectExp {
  case class VectorObjectZeros[A:Manifest](n: Exp[Int]) extends Def[A]
  case class VectorPlus[A:Manifest](x: Exp[Vector[A]], y: Exp[Vector[A]]) extends Def[Vector[A]]
  case class VectorPPrint[A:Manifest](x: Exp[Vector[A]]) extends Def[Unit]

  def vector_obj_zeros(len: Exp[Int]) = reflectEffect(VectorObjectZeros(len))
  def vector_plus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlus(x, y)
  def vector_pprint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(VectorPPrint(x))
}

trait ScalaGenVectorOps2 extends ScalaGenBase {
  val IR: VectorOpsExp2
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case VectorObjectZeros(s) => emitValDef(sym, "Vector.Zeros(" + quote(s) + ")")
    case VectorPlus(x,y) => emitValDef(sym, quote(x) + " + " + quote(y))
    case VectorPPrint(a) => emitValDef(sym, quote(a) + ".pprint")
    case _ => super.emitNode(sym, rhs)
  }
}

/*
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
*/

trait DeliteGenVectorOps2 extends DeliteCodegen {
  val IR: VectorOpsExp2
  import IR._

  //code generation bit
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {    case VectorObjectZeros(s) => emitValDef(sym, "Vector.Zeros(" + quote(s) + ")")
    case VectorPlus(x,y) => emitValDef(sym, quote(x) + " + " + quote(y))
    case VectorPPrint(a) => emitValDef(sym, quote(a) + ".pprint")
    case _ => super.emitNode(sym, rhs)
  }
}