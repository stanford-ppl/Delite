package ppl.dsl.CVX

import scala.virtualization.lms.common.{ScalaGenBase, Base, BaseExp, EffectExp}
import scala.virtualization.lms.util.{OverloadHack}

// to just print out type info
import java.io.PrintWriter

// this is just an IR node
// has no concrete storage
trait ExprOps extends Base with OverloadHack { //with Atoms
  


  // don't really want "int", want just numeric types
  // or should i have a separate thing convert Rep[Numeric] => Expr??
  // T is of type numeric, Int, Double, Float, Matrix, Vector

  def infix_+[A<:Expr:Manifest, B<:Expr:Manifest](x: Rep[A], y: Rep[B]) 
    = apply_binary_op(AffineFunc, Nondecreasing, Nondecreasing)("+", x, y)
  def infix_-[A<:Expr:Manifest, B<:Expr:Manifest](x: Rep[A], y: Rep[B])
    = apply_binary_op(AffineFunc, Nondecreasing, Nonincreasing)("-", x, y)
  
  // can't define * until i'm able to figure out if positive or not...
  
  // need to define +,-,*,
    
  // just define operations between two exprs
  // er. generated code just returns Rep[Expr] instead of Rep[subclass of Expr]
  def apply_binary_op[A:Manifest, B:Manifest](T1:FuncAttribute = Nonconvex, T2:ArgAttribute, T3:ArgAttribute)(op: String, x: Rep[A], y: Rep[B]): Rep[Expr]
  def apply_unary_op[A:Manifest](T1:FuncAttribute = Nonconvex, T2:ArgAttribute)(op: String, x: Rep[A]): Rep[Expr]
}

trait ExprOpsExp extends ExprOps with BaseExp with EffectExp {
  
    case class BinaryOp[A:Manifest, B:Manifest](op: String, x: Exp[A], y: Exp[B]) extends Def[Expr]
    case class ConvexBinaryOp[A:Manifest, B:Manifest](op: String, x: Exp[A], y: Exp[B]) extends Def[ConvexExpr]
    case class ConcaveBinaryOp[A:Manifest, B:Manifest](op: String, x: Exp[A], y: Exp[B]) extends Def[ConcaveExpr]
    case class AffineBinaryOp[A:Manifest, B:Manifest](op: String, x: Exp[A], y: Exp[B]) extends Def[AffineExpr]
    
    case class UnaryOp[A:Manifest](op: String, x: Rep[A]) extends Def[Expr]
    case class ConvexUnaryOp[A:Manifest](op: String, x: Rep[A]) extends Def[ConvexExpr]
    case class ConcaveUnaryOp[A:Manifest](op: String, x: Rep[A]) extends Def[ConcaveExpr]
    case class AffineUnaryOp[A:Manifest](op: String, x: Rep[A]) extends Def[AffineExpr]

    
    def vexity[T](vex:FuncAttribute, mono:ArgAttribute, arg:Manifest[T]) : FuncAttribute ={
      // BUG: somehow, the manifest only tracks the super class....
      val argument : FuncAttribute = {
        if(arg <:< manifest[Affine]) AffineFunc
        else if(arg <:< manifest[Convex]) ConvexFunc
        else if(arg <:< manifest[Concave]) ConcaveFunc
        else {
          println("arg %s is nonconvex".format(arg.erasure))
          Nonconvex
        }
      }
      
      
      // composition rules
      (vex, mono, argument) match {
        case (_:Affine, _, _:Affine) => AffineFunc
        case (_:Convex, _:Nondecreasing, _:Convex) => ConvexFunc
        case (_:Convex, _:Nonincreasing, _:Concave) => ConvexFunc
        case (_:Concave, _:Nondecreasing, _:Concave) => ConcaveFunc
        case (_:Concave, _:Nonincreasing, _:Convex) => ConcaveFunc
        case _ => Nonconvex
      }
    }
    
    // of course, can do compiler optimizations here
    def apply_binary_op[A:Manifest, B:Manifest](T1:FuncAttribute, T2:ArgAttribute, T3:ArgAttribute)(op: String, x: Exp[A], y: Exp[B]) = {
      val vex1 = vexity(T1, T2, manifest[A])
      val vex2 = vexity(T1, T3, manifest[B])
      
      (vex1, vex2) match {
        case (_:Affine, _:Affine) => reflectEffect(AffineBinaryOp(op, x, y))
        case (_:Convex, _:Convex) => reflectEffect(ConvexBinaryOp(op, x, y))
        case (_:Concave, _:Concave) => reflectEffect(ConcaveBinaryOp(op, x, y))
        case _ => reflectEffect(BinaryOp(op, x, y))
      }
    }
    
    def apply_unary_op[A:Manifest](T1:FuncAttribute, T2:ArgAttribute)(op: String, x: Rep[A]) = {
      
      val vex = vexity(T1, T2, manifest[A])
      
      vex match {
        case _:Affine => reflectEffect(AffineUnaryOp(op, x))
        case _:Convex => reflectEffect(ConvexUnaryOp(op, x))
        case _:Concave => reflectEffect(ConcaveUnaryOp(op, x))
        case _ => reflectEffect(UnaryOp(op, x))
      }
    }

}

trait ScalaGenExprOps extends ScalaGenBase {
  val IR: ExprOpsExp // with OptVarCompilerOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case AffineBinaryOp(op, x,y) => emitValDef(sym, "println(\"op: " + op + " results in affine expr!\")")
    case ConvexBinaryOp(op, x,y) => emitValDef(sym, "println(\"op: " + op + " results in convex expr!\")")
    case ConcaveBinaryOp(op, x,y) => emitValDef(sym, "println(\"op: " + op + " results in concave expr!\")")
    case BinaryOp(op, x,y) => emitValDef(sym, "println(\"op: " + op + " results in nonconvex expr!\")")
    
    case AffineUnaryOp(op, x) => emitValDef(sym, "println(\"op: " + op + " results in affine expr!\")")
    case ConvexUnaryOp(op, x) => emitValDef(sym, "println(\"op: " + op + " results in convex expr!\")")
    case ConcaveUnaryOp(op, x) => emitValDef(sym, "println(\"op: " + op + " results in concave expr!\")")
    case UnaryOp(op, x) => emitValDef(sym, "println(\"op: " + op + " results in nonconvex expr!\")")
    
    case _ => super.emitNode(sym, rhs)
  }
}
