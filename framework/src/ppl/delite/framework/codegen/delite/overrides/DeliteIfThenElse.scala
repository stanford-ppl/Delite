package ppl.delite.framework.codegen.delite.overrides

import scala.virtualization.lms.common._
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.internal.GenericNestedCodegen

trait DeliteIfThenElseExp extends IfThenElseExp {

  this: DeliteOpsExp =>

  case class DeliteIfThenElse[T:Manifest](c: Exp[Boolean], t: Exp[T], e: Exp[T]) extends DeliteOpCondition[T](c, t, e)

  override def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = {
    val a = reifyEffects(thenp)
    val b = reifyEffects(elsep)
    (a,b) match {
      case (Def(Reify(_,_)), _) | (_, Def(Reify(_,_))) => reflectEffect(DeliteIfThenElse(cond,a,b))
      case _ => DeliteIfThenElse(cond, thenp, elsep)
    }
  }

}

trait DeliteBaseGenIfThenElse extends GenericNestedCodegen {
  val IR: DeliteIfThenElseExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case DeliteIfThenElse(c, t, e) if shallow => syms(c) // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }

 override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case DeliteIfThenElse(c, t, e) => getFreeVarBlock(c,Nil) ::: getFreeVarBlock(t,Nil) ::: getFreeVarBlock(e,Nil)
    case _ => super.getFreeVarNode(rhs)
  }
}

trait DeliteScalaGenIfThenElse extends  DeliteBaseGenIfThenElse