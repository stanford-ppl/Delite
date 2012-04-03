package ppl.dsl.optisdr

import ppl.delite.framework.ops.DeliteOpsExp
import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericFatCodegen
import scala.virtualization.lms.common._
import scala.reflect.SourceContext

trait LanguageOps extends Base {
  this: OptiSDR =>
}

trait LanguageOpsExp extends LanguageOps with BaseFatExp with EffectExp {
  this: OptiSDRExp =>
  
  // assertWidth(x: Rep[T], width: Rep[Int])
  
  // Kernel stuff
  /*
  Kernel Datastructure
  
  KernelName (takes a kernel)
  
  add functions onto kernel?
  */
  
  // State stuff
  /*
  types:
  StateName (stores the name of the state)
  
  type StateName has apply operation that takes actions with a possible set of transitions
  
  Result of action optionally takes transitions???
  
  Transition(optional wait)
  
  takes partial function returning Strings
  */
}

trait BaseGenLanguageOps extends GenericFatCodegen {
  val IR: LanguageOpsExp
  import IR._

  case class AssertWidth[T:Manifest](x: Exp[T], width: Exp[Int]) extends Def[Unit]
  
  def assertWidth[T:Manifest](x: Rep[T], width: Rep[Int]) = reflectEffect(AssertWidth(x,width))
  
  // Kernel stuff
  // 
  
  // State stuff
  /*
  case class StateName(name: Exp[String])
  */
}
