package ppl.tests.dsls

import ppl.delite.framework.DSLType
import java.io.PrintWriter
import scala.virtualization.lms.common.{ScalaGenFunctions, ScalaGenEffect}
import ppl.delite.framework.codegen.scala.CodeGeneratorScala
import ppl.delite.framework.codegen.c.CodeGeneratorC


trait SimpleFloatVector extends DSLType with ScalaGenEffect with ScalaGenFunctions {

  case class Zeros(n: Rep[Int]) extends Def[SimpleFloatVector]
  case class VectorPlus(v1: Rep[SimpleFloatVector], v2: Rep[SimpleFloatVector]) extends Def[SimpleFloatVector]
  case class VectorApply(v: Rep[SimpleFloatVector], i: Rep[Int]) extends Def[Float]
  case class VectorUpdate(v: Rep[SimpleFloatVector], i: Rep[Int], f: Rep[Float]) extends Def[Unit]
  case class PPrint(v: Rep[SimpleFloatVector]) extends Def[String]

  def zeros(n: Rep[Int]): Rep[SimpleFloatVector] = reflectEffect(Zeros(n))
  def __ext__+(v1: Rep[SimpleFloatVector], v2: Rep[SimpleFloatVector]): Rep[SimpleFloatVector] = VectorPlus(v1, v2)

  //todo could we extend the __ext__ feature to handle this like apply
  class SimpleFloatVectorOps(v: Rep[SimpleFloatVector]) {
    def apply(i: Rep[Int]): Rep[Float] = VectorApply(v,i)
    def update(i: Rep[Int], f: Rep[Float]) = reflectEffect(VectorUpdate(v,i,f))
    def pprint: Rep[String] = PPrint(v)
  }

  //todo, need to be able to only import this stuff automatically
  implicit def injectOpsSFV(v:Rep[SimpleFloatVector]) = new SimpleFloatVectorOps(v)

  //code generation bit
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Zeros(n) => emitValDef(sym, "Vector.zeros(" + quote(n) + ")")
    case VectorPlus(v1,v2) => emitValDef(sym, quote(v1) + " + " + quote (v2))
    case VectorApply(v,i) => emitValDef(sym, quote(v) + "(" + quote(i) + ")")
    case VectorUpdate(v,i,d) => emitValDef(sym, quote(v) + "(" + quote(i) + ") = " + quote(d))
    case PPrint(v) => emitValDef(sym, quote(v) + ".pprint")
    case _ => super.emitNode(sym, rhs)
  }

}

//code generation
trait SimpleFloatVectorGeneratorScala extends CodeGeneratorScala {

  val intermediate: SimpleFloatVector
  import intermediate._
  
  //code generation bit
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Zeros(n) => emitValDef(sym, "Vector.zeros(" + quote(n) + ")")
    case VectorPlus(v1,v2) => emitValDef(sym, quote(v1) + " + " + quote (v2))
    case VectorApply(v,i) => emitValDef(sym, quote(v) + "(" + quote(i) + ")")
    case VectorUpdate(v,i,d) => emitValDef(sym, quote(v) + "(" + quote(i) + ") = " + quote(d))
    case PPrint(v) => emitValDef(sym, quote(v) + ".pprint")
    case _ => super.emitNode(sym, rhs)
  }
}

//code generation
trait SimpleFloatVectorGeneratorC extends CodeGeneratorC {

  val intermediate: SimpleFloatVector
  import intermediate._

  //code generation bit
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Zeros(n) => emitValDef(sym, "Vector.zeros(" + quote(n) + ")")
    case VectorPlus(v1,v2) => emitValDef(sym, quote(v1) + " + " + quote (v2))
    case VectorApply(v,i) => emitValDef(sym, quote(v) + "(" + quote(i) + ")")
    case VectorUpdate(v,i,d) => emitValDef(sym, quote(v) + "(" + quote(i) + ") = " + quote(d))
    case PPrint(v) => emitValDef(sym, quote(v) + ".pprint")
    case _ => super.emitNode(sym, rhs)
  }
}
