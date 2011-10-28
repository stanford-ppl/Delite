package ppl.dsl.CVX

import scala.virtualization.lms.common.{ScalaGenBase, Base, BaseExp, EffectExp}
import scala.virtualization.lms.util.{OverloadHack}

// to just print out type info
import java.io.PrintWriter


// this is just an IR node
// has no concrete storage
trait OptVarOps extends Base with OverloadHack with ExprOps with Vexity {
  // an optvar is one of the many types
  // we're going to need "Constants" (?)
  //
  class OptVar extends Expr with Affine
  object OptVar extends Expr with Affine {
    // all optvars are automatically doubles
    //def apply(length: Int) = optVarNew(unit(length))
    def apply(length: Rep[Int]) = {
      println("%s".format(this))
      optVarNew(length)
    }
  }
  
  // don't really want "int", want just numeric types
  // or should i have a separate thing convert Rep[Numeric] => Expr??
  // T is of type numeric, Int, Double, Float, Matrix, Vector
  def infix_+[T:Manifest](x: Rep[OptVar], y: Rep[T])(implicit o: Overloaded1) = optVarAdd(x, y)
  def infix_+[T:Manifest](y: Rep[T], x: Rep[OptVar])(implicit o: Overloaded2) = optVarAdd(x, y)
  
  // i guess the above is ambiguous when T is of type OptVar
  def infix_+(x: Rep[OptVar], y: Rep[OptVar])(implicit o: Overloaded3) = optVarAdd(x,y)
  
  // need to define +,-,*,
  
  def optVarNew(n: Rep[Int]): Rep[OptVar]
  // actually, after an op, it should return a rep of a Problem
  // codegen just works on the rep of type "Problem"
  def optVarAdd[T:Manifest](x: Rep[OptVar], y: Rep[T]): Rep[Expr]
  
}

trait OptVarOpsExp extends OptVarOps with BaseExp with EffectExp {
  // okay, question is whether i use Def[] with
  // i think Def[] with eventually loses its mixed in traits....
  // or Def[class with]
  case class OptVarNew(n: Exp[Int]) extends Def[OptVar] with Affine
  
  // hmm.. going to have lots of types flying around
  case class OptVarAdd[T:Manifest](x: Exp[OptVar], y: Exp[T]) extends Def[Expr]
  
  def optVarNew(n: Exp[Int]) = reflectEffect(OptVarNew(n))
  
  // of course, can do compiler optimizations here
  // T just has to be of type numeric
  def optVarAdd[T:Manifest](x: Exp[OptVar], y: Exp[T]) = {
    val m = manifest[T]

    m.erasure match {
        case a: Expr with Affine => println("Affine!")//reflectEffect(new OptVarAdd[T](x, y) with Affine)
        case a: Expr with Convex => println("Convex!")//reflectEffect(new OptVarAdd[T](x, y) with Convex)
        case a: Expr with Concave => println("Concave!")//reflectEffect(new OptVarAdd[T](x, y) with Concave)
        case _ => println("boo")
      }
    println("%s".format(m))
    println("%s".format(y))
    reflectEffect(OptVarAdd[T](x, y))
  }
}

trait ScalaGenOptVarOps extends ScalaGenBase {
  val IR: OptVarOpsExp // with OptVarCompilerOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    //these are the ops that call through to the underlying real data structure
    case x@OptVarNew(length) => emitValDef(sym, "println(\"new variable!\")")
    case a: OptVarAdd[_] with Affine => emitValDef(sym, "println(\"adding things results in affine expr!\")")
    case a: OptVarAdd[_] with Convex => emitValDef(sym, "println(\"adding things results in convex expr!\")")
    case a: OptVarAdd[_] with Concave => emitValDef(sym, "println(\"adding things results in concave expr!\")")
    case a: OptVarAdd[_] => emitValDef(sym, "println(\"adding things results in nonconvex expr!\")")
    case _ => super.emitNode(sym, rhs)
  }
}