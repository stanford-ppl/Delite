package ppl.dsl.CVX

import scala.virtualization.lms.common.{ScalaGenBase, Base, BaseExp, EffectExp}
import scala.virtualization.lms.util.{OverloadHack}

// to just print out type info
import java.io.PrintWriter

// this is just an IR node
// has no concrete storage
trait OptVarOps extends Base with OverloadHack with ExprOps {
  // an optvar is one of the many types
  // we're going to need "Constants" (?)
  //
  class OptVar
  object OptVar {
    // all optvars are automatically doubles
    //def apply(length: Int) = optVarNew(unit(length))
    def apply(length: Rep[Int]) = optVarNew(length)
  }
  
  // don't really want "int", want just numeric types
  // or should i have a separate thing convert Rep[Numeric] => Expr??
  // T is of type numeric, Int, Double, Float, Matrix, Vector
  def infix_+(x: Rep[OptVar], y: Rep[Int])(implicit o: Overloaded1) = optVarAdd(x, y)
  def infix_+(y: Rep[Int], x: Rep[OptVar])(implicit o: Overloaded2) = optVarAdd(x, y)
  
  // need to define +,-,*,
  
  def optVarNew(n: Rep[Int]): Rep[OptVar]
  def optVarAdd[T<:Int:Manifest](x: Rep[OptVar], y: Rep[Int]): Rep[Expr]
  
}

trait OptVarOpsExp extends OptVarOps with BaseExp with EffectExp {
  // okay, question is whether i use Def[] with
  // or Def[class with]
  case class OptVarNew(n: Exp[Int]) extends Def[OptVar] with Affine
  
  // hmm.. going to have lots of types flying around
  case class OptVarAdd(x: Exp[OptVar], y: Exp[Int]) extends Def[Expr]
  
  def optVarNew(n: Exp[Int]) = reflectEffect(OptVarNew(n))
  
  // of course, can do compiler optimizations here
  // T just has to be of type numeric
  def optVarAdd[T:Manifest](x: Exp[OptVar], y: Exp[Int]) = y match {
    case a: Exp[T] with Affine => reflectEffect(new OptVarAdd(x, y) with Affine)
    case a: Exp[T] with Convex => reflectEffect(new OptVarAdd(x, y) with Convex)
    case a: Exp[T] with Concave => reflectEffect(new OptVarAdd(x, y) with Concave)
    case _ => reflectEffect(OptVarAdd(x, y))
  }
}

trait ScalaGenOptVarOps extends ScalaGenBase {
  val IR: OptVarOpsExp // with OptVarCompilerOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    //these are the ops that call through to the underlying real data structure
    case x@OptVarNew(length) => emitValDef(sym, "println(\"new variable!\")")
    case a: OptVarAdd with Affine => emitValDef(sym, "println(\"adding things results in affine expr!\")")
    case a: OptVarAdd with Convex => emitValDef(sym, "println(\"adding things results in convex expr!\")")
    case a: OptVarAdd with Concave => emitValDef(sym, "println(\"adding things results in concave expr!\")")
    case a: OptVarAdd => emitValDef(sym, "println(\"adding things results in nonconvex expr!\")")
    case _ => super.emitNode(sym, rhs)
  }
}