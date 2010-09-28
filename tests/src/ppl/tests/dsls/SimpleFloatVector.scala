package ppl.tests.dsls

import java.io.PrintWriter
import ppl.delite.framework.codegen.c.CodeGeneratorCBase
import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.codegen.scala.CodeGeneratorScalaBase
import ppl.delite.framework.embedded.scala.{CodeGeneratorCMisc, CodeGeneratorScalaMisc}

//todo need to add ScalaGenFunctions functionality and also remove ScalaOpsExp Stuff
trait SimpleFloatVector extends DSLType { this: DeliteApplication =>

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

  //register my code generators
  //todo these should hook into some option parser for our applications
  generators +=  new SimpleFloatVectorGeneratorScala {
    val intermediate: SimpleFloatVector.this.type = SimpleFloatVector.this
  }
  generators += new SimpleFloatVectorGeneratorC {
    val intermediate: SimpleFloatVector.this.type = SimpleFloatVector.this
  }

}

//code generation
trait SimpleFloatVectorGeneratorScala extends CodeGeneratorScalaBase with CodeGeneratorScalaMisc {

  val intermediate: SimpleFloatVector with DeliteApplication
  import intermediate._
  
  //code generation bit
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Zeros(n) => emitValDef("",sym, "Vector.zeros(" + quote(n) + ")")
    case VectorPlus(v1,v2) => emitValDef("",sym, quote(v1) + " + " + quote (v2))
    case VectorApply(v,i) => emitValDef("",sym, quote(v) + "(" + quote(i) + ")")
    case VectorUpdate(v,i,d) => emitValDef("",sym, quote(v) + "(" + quote(i) + ") = " + quote(d))
    case PPrint(v) => emitValDef("",sym, quote(v) + ".pprint")
    case _ => super.emitNode(sym, rhs)
  }
}

//code generation
trait SimpleFloatVectorGeneratorC extends CodeGeneratorCBase with CodeGeneratorCMisc {
  
  val intermediate: SimpleFloatVector with DeliteApplication
  import intermediate._

  //code generation bit
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    //todo replace the manifest with embedded types
    case Zeros(n) => emitValDef("vector", sym, "Vector.zeros(" + quote(n) + ")")
    case VectorPlus(v1,v2) => emitValDef("vector", sym, quote(v1) + " + " + quote (v2))
    case VectorApply(v,i) => emitValDef("vector", sym, quote(v) + "(" + quote(i) + ")")
    case VectorUpdate(v,i,d) => stream.println(quote(v) + "(" + quote(i) + ")" + " = " + quote(d) + ";")
    case PPrint(v) => stream.println("printf(\"%s\"," + quote(v) + ".pprint());")
    case _ => super.emitNode(sym, rhs)
  }
}
