package ppl.dsl.optila.capabilities

import scala.virtualization.lms.common.{Variables, Base}
import scala.reflect.SourceContext
import ppl.dsl.optila.{Vector,Matrix}
import ppl.dsl.optila.{OptiLAExp, OptiLA}

trait HasMinMaxInternal[Rep[X], T] {
  def minValue(implicit ctx: SourceContext): Rep[T]
  def maxValue(implicit ctx: SourceContext): Rep[T]
}

trait HasMinMaxOps extends Variables {
  this: OptiLA =>
  
  type HasMinMax[X] = HasMinMaxInternal[Rep,X]
  
  implicit def hasMinMaxToHasMinMaxOps[T:HasMinMax:Manifest](n: T) = new HasMinMaxOpsCls(unit(n))
  implicit def repHasMinMaxToHasMinMaxOps[T:HasMinMax:Manifest](n: Rep[T]) = new HasMinMaxOpsCls(n)
  implicit def varHasMinMaxToHasMinMaxOps[T:HasMinMax:Manifest](n: Var[T]) = new HasMinMaxOpsCls(readVar(n))

  class HasMinMaxOpsCls[T](lhs: Rep[T])(implicit mT: Manifest[T], minmax: HasMinMax[T]){
    def minValue(implicit ctx: SourceContext) = minmax.minValue
    def maxValue(implicit ctx: SourceContext) = minmax.maxValue
  }
  
  implicit val doubleHasMinMax: HasMinMax[Double] = new HasMinMax[Double] {
    def minValue(implicit ctx: SourceContext) = unit(scala.Double.MinValue)
    def maxValue(implicit ctx: SourceContext) = unit(scala.Double.MaxValue)    
  }

  implicit val FloatHasMinMax: HasMinMax[Float] = new HasMinMax[Float] {
    def minValue(implicit ctx: SourceContext) = unit(scala.Float.MinValue)
    def maxValue(implicit ctx: SourceContext) = unit(scala.Float.MaxValue)   
  }
  
  implicit val intHasMinMax: HasMinMax[Int] = new HasMinMax[Int] {
    def minValue(implicit ctx: SourceContext) = unit(scala.Int.MinValue)
    def maxValue(implicit ctx: SourceContext) = unit(scala.Int.MaxValue)   
  }
}

