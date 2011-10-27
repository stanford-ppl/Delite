package ppl.dsl.CVX

import scala.virtualization.lms.common.{ScalaGenBase, Base, BaseExp, EffectExp}
import scala.virtualization.lms.util.{OverloadHack}

// to just print out type info
import java.io.PrintWriter

// this is just an IR node
// has no concrete storage
trait OptVarOps extends Base with OverloadHack {
  // an optvar is one of the many types
  // we're going to need "Constants" (?)
  //
  class Expr
  class OptVar extends Affine
  object OptVar {
    // all optvars are automatically doubles
    def apply(length: Int) = optVarNew(unit(length))
    def apply(length: Rep[Int]) = optVarNew(length)
  }
  
  // don't really want "int", want just numeric types
  // or should i have a separate thing convert Rep[Numeric] => Expr??
  def infix_+(x: Rep[OptVar], y: Int)(implicit o: Overloaded1) = optVarAddAny(x, unit(y))
  def infix_+(x: Rep[OptVar], y: Rep[Int])(implicit o: Overloaded2) = optVarAddAny(x, y)
  def infix_+(y: Rep[Int], x: Rep[OptVar])(implicit o: Overloaded3) = optVarAddAny(x, y)
  def infix_+(y: Int, x: Rep[OptVar])(implicit o: Overloaded4) = optVarAddAny(x, unit(y))
  
  // need to define +,-,*,
  
  def optVarNew(n: Rep[Int]): Rep[OptVar]
  def optVarAddAny(x: Rep[OptVar], y: Rep[Int]): Rep[Expr]
  
}

trait OptVarOpsExp extends OptVarOps with BaseExp with EffectExp {
  case class OptVarNew(n: Exp[Int]) extends Def[OptVar] with Affine
  
  // hmm.. going to have lots of types flying around
  case class OptVarAddConvex(x: Exp[OptVar], y: Exp[Int]) extends Def[Expr] with Convex
  case class OptVarAddConcave(x: Exp[OptVar], y: Exp[Int]) extends Def[Expr] with Concave
  case class OptVarAddAffine(x: Exp[OptVar], y: Exp[Int]) extends Def[Expr] with Affine
  case class OptVarAddAny(x: Exp[OptVar], y: Exp[Int]) extends Def[Expr]
  
  def optVarNew(n: Exp[Int]) = reflectEffect(OptVarNew(n))
  
  // of course, can do compiler optimizations here
  def optVarAddAny(x: Exp[OptVar], y: Exp[Int]) = y match {
    case a: Expr with Affine => reflectEffect(OptVarAddAffine(x, y))
    case a: Expr with Convex => reflectEffect(OptVarAddConvex(x, y))
    case a: Expr with Concave => reflectEffect(OptVarAddConcave(x, y))
    case _ => reflectEffect(OptVarAddAny(x, y))
  }
}

trait ScalaGenOptVarOps extends ScalaGenBase {
  val IR: OptVarOpsExp // with OptVarCompilerOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    //these are the ops that call through to the underlying real data structure
    case x@OptVarNew(length) => emitValDef(sym, "println(\"new variable!\")")
    case OptVarAddAffine(x,y) => emitValDef(sym, "prinln(\"adding things results in affine expr!\")")
    case OptVarAddConvex(x,y) => emitValDef(sym, "prinln(\"adding things results in convex expr!\")")
    case OptVarAddConcave(x,y) => emitValDef(sym, "prinln(\"adding things results in concave expr!\")")
    case OptVarAddAny(x,y) => emitValDef(sym, "prinln(\"adding things results in nonconvex expr!\")")
    case _ => super.emitNode(sym, rhs)
  }
}