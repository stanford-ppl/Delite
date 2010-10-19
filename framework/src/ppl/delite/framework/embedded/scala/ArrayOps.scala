package ppl.delite.framework.embedded.scala

import java.io.PrintWriter
import ppl.delite.framework.{DSLType, DeliteApplication}
import ppl.delite.framework.codegen.scala.{TargetScala, CodeGeneratorScalaBase}

trait ArrayOps extends DSLType { this: DeliteApplication with Variables =>

  // multiple definitions needed because implicits won't chain
  // not using infix here because apply doesn't work with infix methods
  implicit def varToRepArrayOps[A](x: Var[Array[A]]) = new RepArrayOpsCls(readVar(x))
  implicit def repArrayToRepArrayOps[T](a: Rep[Array[T]]) = new RepArrayOpsCls(a)
  implicit def arrayToRepArrayOps[T](a: Array[T]) = new RepArrayOpsCls(a)

  class RepArrayOpsCls[T](a: Rep[Array[T]]){
    def apply(n: Rep[Int]) = array_apply(a, n)
    def length = array_length(a)
  }
    
  def array_apply[T](x: Rep[Array[T]], n: Rep[Int]): Rep[T]
  def array_length[T](x: Rep[Array[T]]) : Rep[Int]

}

trait ArrayOpsExp extends ArrayOps { this: DeliteApplication with VariablesExp =>
  case class ArrayLength[T](a: Exp[Array[T]]) extends Def[Int]
  case class ArrayApply[T](x: Exp[Array[T]], n: Exp[Int]) extends Def[T]

  def array_apply[T](x: Exp[Array[T]], n: Exp[Int]): Rep[T] = ArrayApply(x, n)
  def array_length[T](a: Exp[Array[T]]) : Rep[Int] = ArrayLength(a)

  targets.get("Scala").getOrElse(throw new RuntimeException("Couldn't find Scala code generator"))
    .addGenerator( new CodeGeneratorScalaArray { val intermediate: ArrayOpsExp.this.type = ArrayOpsExp.this })
}

trait CodeGeneratorScalaArray extends CodeGeneratorScalaBase {

  val intermediate: DeliteApplication with ArrayOpsExp
  import intermediate._

  def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Boolean = {
    rhs match {
      case ArrayLength(x) => emitValDef(sym, "" + quote(x) + ".length")
      case ArrayApply(x,n) => emitValDef(sym, "" + quote(x) + "(" + quote(n) + ")")
      case _ => return false
    }
    true
  }
}
