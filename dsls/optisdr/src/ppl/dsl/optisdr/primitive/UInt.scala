package ppl.dsl.optisdr.primitive

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

import ppl.dsl.optisdr._

trait UIntOps extends Variables {
  this: OptiSDR =>
    
  // Static methods
  object UInt {
    def apply(x: Int)(implicit ctx: SourceContext) = uint_new(unit(x))
    def apply(x: Rep[Int])(implicit ctx: SourceContext) = uint_new(x)
  }
  
  // Implicit numeric conversions
  //implicit def repIntToRepUInt(x: Rep[Int]): Rep[UInt] = UInt(x)
  
  implicit def repToUIntOps(x: Rep[UInt]) = new UIntOpsCls(x)
  implicit def varToUIntOps(x: Var[UInt]) = new UIntOpsCls(readVar(x))
  
  // Objects methods
  class UIntOpsCls(x: Rep[UInt]) {
    def +(y: Rep[UInt])(implicit ctx: SourceContext) = uint_plus(x,y)
    def -(y: Rep[UInt])(implicit ctx: SourceContext) = uint_minus(x,y)
    def *(y: Rep[UInt])(implicit ctx: SourceContext) = uint_times(x,y)
    def /(y: Rep[UInt])(implicit ctx: SourceContext) = uint_divide(x,y)
    def abs(implicit ctx: SourceContext) = uint_abs(x)
    def exp(implicit ctx: SourceContext) = uint_exp(x)
    
    def intValue = uint_int_value(x)
  }
  
  // Arith implicit
  implicit def uintArith : Arith[UInt] = new Arith[UInt] {
    def +=(a: Rep[UInt], b: Rep[UInt])(implicit ctx: SourceContext) = repToUIntOps(a).+(b)
    def +(a: Rep[UInt], b: Rep[UInt])(implicit ctx: SourceContext) = repToUIntOps(a).+(b)
    def -(a: Rep[UInt], b: Rep[UInt])(implicit ctx: SourceContext) = repToUIntOps(a).-(b)
    def *(a: Rep[UInt], b: Rep[UInt])(implicit ctx: SourceContext) = repToUIntOps(a).*(b)
    def /(a: Rep[UInt], b: Rep[UInt])(implicit ctx: SourceContext) = repToUIntOps(a)./(b)
    def abs(a: Rep[UInt])(implicit ctx: SourceContext) = repToUIntOps(a).abs
    def exp(a: Rep[UInt])(implicit ctx: SourceContext) = repToUIntOps(a).exp
    
    def empty(implicit ctx: SourceContext) = UInt(unit(0))
    def zero(a: Rep[UInt])(implicit ctx: SourceContext) = empty
  }
  
  // Constructor
  def uint_new(x: Rep[Int])(implicit ctx: SourceContext) : Rep[UInt]
  
  // Math ops
  def uint_plus(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_minus(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_times(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_divide(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_abs(x: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_exp(x: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  
  // Conversions
  def uint_int_value(x: Rep[UInt]) : Rep[Int]
}

trait UIntOpsExp extends UIntOps {
  this: OptiSDRExp =>
  
  // Object creation
  case class UIntNew(x: Exp[Int]) extends Def[UInt]
  
  def uint_new(x: Exp[Int])(implicit ctx: SourceContext) = reflectPure(UIntNew(x))
  
  // Operations
  case class UIntPlus(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  case class UIntMinus(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  case class UIntTimes(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  case class UIntDivide(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  case class UIntAbs(x: Exp[UInt]) extends Def[UInt]
  case class UIntExp(x: Exp[UInt]) extends Def[UInt]
  
  def uint_plus(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntPlus(x, y))
  def uint_minus(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntMinus(x, y))
  def uint_times(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntTimes(x, y))
  def uint_divide(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntDivide(x, y))
  def uint_abs(x: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntAbs(x))
  def uint_exp(x: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntExp(x))
  
  // Conversions
  case class UIntIntValue(x: Exp[UInt]) extends Def[Int]
  
  def uint_int_value(x: Exp[UInt]) = reflectPure(UIntIntValue(x))
}


trait UIntOpsExpOpt extends UIntOpsExp {
  this: OptiSDRExp =>
  
  object UIntConst {
    def unapply(x: Exp[UInt]) : Option[Int] = {
      x match {
        case Def(UIntNew(Const(e))) => Some(e)
      }
    }
  }

  override def uint_plus(lhs: Exp[UInt], rhs: Exp[UInt])(implicit ctx: SourceContext) : Exp[UInt] = (lhs,rhs) match {
    case (UIntConst(x), UIntConst(y)) => UInt(x+y)
    case (UIntConst(0), y) => y
    case (y, UIntConst(0)) => y
    case _ => super.uint_plus(lhs, rhs)
  }
  
  override def uint_minus(lhs: Exp[UInt], rhs: Exp[UInt])(implicit ctx: SourceContext) : Exp[UInt] = (lhs,rhs) match {
    case (UIntConst(x), UIntConst(y)) => UInt(x-y)
    case (y, UIntConst(0)) => y
    case _ => super.uint_minus(lhs, rhs)
  }
  
  override def uint_times(lhs: Exp[UInt], rhs: Exp[UInt])(implicit ctx: SourceContext) : Exp[UInt] = (lhs,rhs) match {
    case (UIntConst(x), UIntConst(y)) => UInt(x*y)
    case (_, UIntConst(0)) => rhs
    case (UIntConst(0), _) => lhs
    case _ => super.uint_times(lhs, rhs)
  }
}