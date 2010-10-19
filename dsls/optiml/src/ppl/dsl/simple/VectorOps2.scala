package ppl.dsl.simple

import ppl.delite.framework.{DSLType, DeliteApplication}
import ppl.delite.framework.codegen.scala.CodeGeneratorScalaBase
import scala.virtualization.lms.internal.Effects
import java.io.PrintWriter
import ppl.delite.framework.codegen.c.CodeGeneratorCBase
import ppl.delite.framework.embedded.scala.CodeGeneratorCMisc
import scala.virtualization.lms.common.{EffectExp, BaseExp}


trait Vector[T]

trait VectorOps2 extends DSLType { this: DeliteApplication =>

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

trait VectorOpsExp2 extends VectorOps2 { this: DeliteApplication =>
  case class VectorObjectZeros[A](n: Exp[Int]) extends Def[A]
  case class VectorPlus[A](x: Exp[Vector[A]], y: Exp[Vector[A]]) extends Def[Vector[A]]
  case class VectorPPrint[A](x: Exp[Vector[A]]) extends Def[Unit]

  def vector_obj_zeros(len: Exp[Int]) = reflectEffect(VectorObjectZeros(len))
  def vector_plus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlus(x, y)
  def vector_pprint[A](x: Exp[Vector[A]]) = reflectEffect(VectorPPrint(x))

  //  //register my code generators
  //todo these should hook into some option parser for our applications,
  // also this doesn't actually work as you need to mix different generators together

  targets.get("Scala").getOrElse(
    throw new RuntimeException("Couldn't find Scala code generator")
  ) .generators += new CodeGeneratorScalaVector {
    val intermediate: VectorOpsExp2.this.type = VectorOpsExp2.this
  }

  targets.get("C").getOrElse(
    throw new RuntimeException("Couldn't find C code generator")
  ) .generators += new CodeGeneratorCVector {
    val intermediate: VectorOpsExp2.this.type = VectorOpsExp2.this
  }
}

trait CodeGeneratorScalaVector extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with VectorOpsExp2 with EffectExp
  import intermediate._

  def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Boolean = {
    rhs match {
      case VectorObjectZeros(s) => emitValDef(sym, "println(" + quote(s) + ")")
      case VectorPlus(x,y) => emitValDef(sym, quote(x) + " + " + quote(y))
      case VectorPPrint(a) => emitValDef(sym, "exit(" + quote(a) + ")")

      case _ => return false
    }
    true
  }
}

////code generation
trait CodeGeneratorCVector extends CodeGeneratorCBase {

  val intermediate: DeliteApplication with VectorOpsExp2 with EffectExp
  import intermediate._

  //code generation bit
  def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Boolean = {
    rhs match {
      //todo replace the manifest with embedded types
      case VectorObjectZeros(n) => emitConstDef("vector", sym, "Vector.zeros(" + quote(n) + ")")
      case VectorPlus(v1,v2) => emitConstDef("vector", sym, quote(v1) + " + " + quote (v2))
      case VectorPPrint(v) => emitConstDef("vector", sym, quote(v) + ".pprint()")
      case _ => return false
    }
    true
  }
}