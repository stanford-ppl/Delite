package ppl.dsl.CVX

import scala.virtualization.lms.common.{ScalaGenBase, Base, BaseExp, EffectExp}
import scala.virtualization.lms.util.{OverloadHack}

// to just print out type info
import java.io.PrintWriter

// this is just an IR node
// has no concrete storage
trait ExprOps extends Base with OverloadHack {
  
  class Expr
  // class ConstExpr extends Expr
  // 
  // implicit def intToExpr(x: Rep[Int]) = toConst(x)
  // implicit def floatToExpr(x: Rep[Float]) = toConst(x)
  // implicit def doubleToExpr(x: Rep[Double]) = toConst(x)
  // 
  // def toConst[T](x: Rep[T]): Rep[ConstExpr] 
}

trait ExprOpsExp extends ExprOps with BaseExp with EffectExp {
  // //case class PositiveConstant[T](x: Exp[T]) extends Def[Const]
  // case class Constant[T](x: Exp[T]) extends Def[ConstExpr]
  // 
  // // of course, can do compiler optimizations here
  // // T just has to be of type numeric
  // def toConst[T](x: Exp[T]) = Constant[T](x)
  // // TODO: how to check value of int?
  //   //if(x >= 0) PositiveConstant[Int](x)
  //   //else Constant[Int](x)
}
