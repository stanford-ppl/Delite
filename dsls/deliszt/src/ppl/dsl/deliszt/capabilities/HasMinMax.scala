package ppl.dsl.deliszt.capabilities

import scala.virtualization.lms.common.{Variables, Base}
import ppl.dsl.deliszt.{DeLisztExp, DeLiszt}

trait HasMinMaxInternal[Rep[X], T] {
  def minValue: Rep[T]
  def maxValue: Rep[T]
}

trait HasMinMaxOps extends Variables {
  this: DeLiszt =>
  
  type HasMinMax[X] = HasMinMaxInternal[Rep,X]
  
  implicit def hasMinMaxToHasMinMaxOps[T:HasMinMax:Manifest](n: T) = new HasMinMaxOpsCls(unit(n))
  implicit def repHasMinMaxToHasMinMaxOps[T:HasMinMax:Manifest](n: Rep[T]) = new HasMinMaxOpsCls(n)
  implicit def varHasMinMaxToHasMinMaxOps[T:HasMinMax:Manifest](n: Var[T]) = new HasMinMaxOpsCls(readVar(n))

  class HasMinMaxOpsCls[T](lhs: Rep[T])(implicit mT: Manifest[T], minmax: HasMinMax[T]){
    def minValue = minmax.minValue
    def maxValue = minmax.maxValue
  }
  
  implicit val doubleHasMinMax: HasMinMax[Double] = new HasMinMax[Double] {
    def minValue = unit(scala.Double.MinValue)
    def maxValue = unit(scala.Double.MaxValue)    
  }

  implicit val FloatHasMinMax: HasMinMax[Float] = new HasMinMax[Float] {
    def minValue = unit(scala.Float.MinValue)
    def maxValue = unit(scala.Float.MaxValue)   
  }
  
  implicit val intHasMinMax: HasMinMax[Int] = new HasMinMax[Int] {
    def minValue = unit(scala.Int.MinValue)
    def maxValue = unit(scala.Int.MaxValue)   
  }
}

