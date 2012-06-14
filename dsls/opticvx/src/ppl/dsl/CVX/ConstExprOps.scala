package ppl.dsl.CVX

import scala.virtualization.lms.common.{ScalaGenBase, Base, BaseExp, EffectExp}
import scala.virtualization.lms.util.{OverloadHack}

// to just print out type info
import java.io.PrintWriter

// this is just an IR node
// has no concrete storage
trait ConstExprOps extends Base with OverloadHack {
  
  // these implicit don't get called... boooo
  implicit def intToExpr(x: Rep[Int]) = toConst(x)
  implicit def floatToExpr(x: Rep[Float]) = toConst(x)
  implicit def doubleToExpr(x: Rep[Double]) = toConst(x)
  
  def toConst[T](x: Rep[T]): Rep[ConstExpr] 
  // need to be able to check if it's positive
  
  // just define addition between two exprs
}

trait ConstExprOpsExp extends ConstExprOps with BaseExp with EffectExp {
  //case class PositiveConstant[T](x: Exp[T]) extends Def[Const]
    case class Constant[T](x: Exp[T]) extends Def[ConstExpr]
    
    // of course, can do compiler optimizations here
    // T just has to be of type numeric
    def toConst[T](x: Exp[T]) = Constant[T](x)
    // TODO: how to check value of int?
      //if(x >= 0) PositiveConstant[Int](x)
      //else Constant[Int](x)
}

trait ScalaGenConstExprOps extends ScalaGenBase {
  val IR: ConstExprOpsExp // with OptVarCompilerOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    //these are the ops that call through to the underlying real data structure
    case Constant(x) => emitValDef(sym, quote(x))
    case _ => super.emitNode(sym, rhs)
  }
}
