package ppl.dsl.optisdr.primitive

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

import ppl.dsl.optisdr._

trait UIntOps extends Variables {
  this: OptiSDR =>
    
  // Static methods
  object UInt {
    def apply(x: Rep[Int])(implicit ctx: SourceContext) = uint_obj_new(x)
  }
  
  // Implicit numeric conversions
  //implicit def repIntToRepUInt(x: Rep[Int]): Rep[UInt] = UInt(x)
  
  implicit def repToUIntOps(x: Rep[UInt]) = new UIntOpsCls(x)
  implicit def varToUIntOps(x: Var[UInt]) = new UIntOpsCls(readVar(x))
  
  // Objects methods
  class UIntOpsCls(x: Rep[UInt]) {
    def +(y: Rep[UInt]) = uint_plus(x,y)
    def -(y: Rep[UInt]) = uint_minus(x,y)
    def *(y: Rep[UInt]) = uint_times(x,y)
    def /(y: Rep[UInt]) = uint_divide(x,y)
    def abs = uint_abs(x)
    def exp = uint_exp(x)
  }
  
  // Arith implicit
  implicit def untArith[T:Arith:Manifest]: Arith[UInt] = new Arith[UInt] {
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
  def uint_obj_new(x: Rep[Int])(implicit ctx: SourceContext) : Rep[UInt]
  
  // Math ops
  def uint_plus(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_minus(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_times(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_divide(x: Rep[UInt], y: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_abs(x: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
  def uint_exp(x: Rep[UInt])(implicit ctx: SourceContext) : Rep[UInt]
}

trait UIntOpsExp extends UIntOps {
  this: OptiSDRExp =>
  
  // Object creation
  case class UIntNew(x: Exp[Int]) extends Def[UInt]
  
  // Operations
  case class UIntPlus(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  case class UIntMinus(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  case class UIntTimes(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  case class UIntDivide(x: Exp[UInt], y: Exp[UInt]) extends Def[UInt]
  case class UIntAbs(x: Exp[UInt]) extends Def[UInt]
  case class UIntExp(x: Exp[UInt]) extends Def[UInt]
  
  def uint_obj_new(x: Exp[Int])(implicit ctx: SourceContext) = reflectPure(UIntNew(x))
  
  // Operations
  def uint_plus(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntPlus(x, y))
  def uint_minus(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntMinus(x, y))
  def uint_times(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntTimes(x, y))
  def uint_divide(x: Exp[UInt], y: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntDivide(x, y))
  def uint_abs(x: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntAbs(x))
  def uint_exp(x: Exp[UInt])(implicit ctx: SourceContext) = reflectPure(UIntExp(x))
}


trait UIntOpsExpOpt extends UIntOpsExp {
  this: OptiSDRExp =>

  override def uint_plus(lhs: Exp[UInt], rhs: Exp[UInt])(implicit ctx: SourceContext) : Exp[UInt] = (lhs,rhs) match {
    case (Def(Def(UIntNew(Const(x)))), Def(Def(UIntNew(Const(y))))) => UInt(unit(implicitly[Numeric[Int]].plus(x,y)))
    case (Def(Def(UIntNew(Const(0)))), y) => y
    case (y, Def(UIntNew(Const(0)))) => y
    case _ => super.uint_plus(lhs, rhs)
  }
  
  override def uint_minus(lhs: Exp[UInt], rhs: Exp[UInt])(implicit ctx: SourceContext) : Exp[UInt] = (lhs,rhs) match {
    case (Def(UIntNew(Const(x))), Def(UIntNew(Const(y)))) => UInt(unit(implicitly[Numeric[Int]].minus(x,y)))
    case (y, Def(UIntNew(Const(0)))) => y
    case _ => super.uint_minus(lhs, rhs)
  }
  
  override def uint_times(lhs: Exp[UInt], rhs: Exp[UInt])(implicit ctx: SourceContext) : Exp[UInt] = (lhs,rhs) match {
    case (Def(UIntNew(Const(x))), Def(UIntNew(Const(y)))) => UInt(unit(implicitly[Numeric[Int]].times(x,y)))
    case (_, Def(UIntNew(Const(0)))) => rhs
    case (Def(UIntNew(Const(0))), _) => lhs
    case _ => super.uint_times(lhs, rhs)
  }
}