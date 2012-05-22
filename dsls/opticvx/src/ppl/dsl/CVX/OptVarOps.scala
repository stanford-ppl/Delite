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
  object OptVar {
    // all optvars are automatically doubles
    //def apply(length: Int) = optVarNew(unit(length))
    def apply(length: Rep[Int]) = {
      println("%s".format(this))
      optVarNew(length)
    }
  }
    
  def optVarNew(n: Rep[Int]): Rep[OptVar]
}

trait OptVarOpsExp extends OptVarOps with BaseExp with EffectExp {
  // okay, question is whether i use Def[] with
  // i think Def[] with eventually loses its mixed in traits....
  // or Def[class with]
  case class OptVarNew(n: Exp[Int]) extends Def[OptVar]
  
  
  def optVarNew(n: Exp[Int]) = reflectEffect(OptVarNew(n))
}

trait ScalaGenOptVarOps extends ScalaGenBase {
  val IR: OptVarOpsExp // with OptVarCompilerOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    //these are the ops that call through to the underlying real data structure
    case x@OptVarNew(length) => emitValDef(sym, "println(\"new variable!\")")
    case _ => super.emitNode(sym, rhs)
  }
}