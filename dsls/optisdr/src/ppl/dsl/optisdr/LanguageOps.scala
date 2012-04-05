package ppl.dsl.optisdr

import ppl.delite.framework.ops.DeliteOpsExp
import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericFatCodegen
import scala.virtualization.lms.common._
import scala.reflect.SourceContext

trait LanguageOps extends Base {
  this: OptiSDR =>
  
  object Range {
	def apply[T:Manifest](lo: Rep[T], hi: Rep[T]) = create_range(lo, hi)
  }
  
  def create_range[T:Manifest](lo: Rep[T], hi: Rep[T]) : Range[T]
  
  def assertWidth[T:Manifest](x: Rep[T], width: Rep[Int])(implicit ctx: SourceContext) : Rep[Unit]
  // assert multiple of? assert minimum?
  
  def belongsto[T:Manifest](x: Rep[T], r: Range[T])(implicit ctx: SourceContext) : Rep[Unit]
  
  // Just syntax sugar!
  // What we expect is a block that returns a function. Not sure how to expect a return type of a function with any number of args...
  def kernel(kernel_fn: AnyRef) = kernel_fn
}

trait LanguageOpsExp extends LanguageOps with BaseFatExp with EffectExp {
  this: OptiSDRExp =>
  
  
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
  
  case class AssertWidth[T:Manifest](x: Exp[T], width: Exp[Int]) extends Def[Unit]
  
  def assertWidth[T:Manifest](x: Exp[T], width: Exp[Int])(implicit ctx: SourceContext) = reflectEffect(AssertWidth(x,width))
  
  case class BelongsTo[T:Manifest](x: Exp[T], r: Range[T]) extends Def[Unit]
  
  def belongsto[T:Manifest](x: Exp[T], r: Range[T])(implicit ctx: SourceContext) = reflectEffect(BelongsTo(x,r))
  
  // Does this only work for integers?
  def create_range[T:Manifest](lo: Exp[T], hi: Exp[T]) : Range[T] = {
	(lo, hi) match {
		case (Const(x), Const(y)) => new Range(x, y)
		case _ => throw new IllegalArgumentException("Range needs constants!"); null
	}
  }
  
  // Kernel stuff
  // 
  
  // State stuff
  /*
  case class StateName(name: Exp[String])
  */
}

trait BaseGenLanguageOps extends GenericFatCodegen {
  val IR: LanguageOpsExp
  import IR._
}
