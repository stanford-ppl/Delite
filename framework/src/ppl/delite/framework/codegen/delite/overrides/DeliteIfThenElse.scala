package ppl.delite.framework.codegen.delite.overrides

import scala.virtualization.lms.common._

trait DeliteIfThenElseExp extends IfThenElse with EffectExp {

  //todo change this to use DeliteConditional
  case class DeliteIfThenElse[T:Manifest](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T]

  def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = {
    val a = reifyEffects(thenp)
    val b = reifyEffects(elsep)
    (a,b) match {
      case (Def(Reify(_,_)), _) | (_, Def(Reify(_,_))) => reflectEffect(DeliteIfThenElse(cond,a,b))
      case _ => DeliteIfThenElse(cond, thenp, elsep)
    }
  }

}