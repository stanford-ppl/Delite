package ppl.dsl.optisdr.primitive

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

import ppl.dsl.optisdr._

trait ComplexIntOps extends Variables {
  this: OptiSDR =>
  
  // Primitive conversions to ComplexInt
  /* implicit def repIntToRepComplexInt(x: Rep[Int]): Rep[Double] = implicit_convert[Int,Double](x)
  implicit def repIntToRepFloat(x: Rep[Int]): Rep[Float] = implicit_convert[Int,Float](x)
  implicit def repFloatToRepDouble(x: Rep[Float]): Rep[Double] = implicit_convert[Float,Double](x)   */
  
  // Static methods
  object ComplexInt {
    def apply(real: Int, imag: Int)(implicit ctx: SourceContext) = complex_int_new(unit(real), unit(imag)) 
    def apply(real: Rep[Int], imag: Int)(implicit ctx: SourceContext) = complex_int_new(real, unit(imag))
    def apply(real: Int, imag: Rep[Int])(implicit ctx: SourceContext) = complex_int_new(unit(real), imag)
    def apply(real: Rep[Int], imag: Rep[Int])(implicit ctx: SourceContext) = complex_int_new(real, imag) 
  }
  
  // Implicit numeric conversions
  
  implicit def repToComplexIntOps(x: Rep[ComplexInt]) = new ComplexIntOpsCls(x)
  implicit def varToComplexIntOps(x: Var[ComplexInt]) = new ComplexIntOpsCls(readVar(x))
  
  // Objects methods
  class ComplexIntOpsCls(x: Rep[ComplexInt]) {
    def real = complex_int_real(x)
    def realValue = complex_int_real(x)
    def imag = complex_int_imag(x)
    
    def conj = complex_int_conj(x)
    
    def +(y: Rep[ComplexInt]) = complex_int_plus(x,y)
    def -(y: Rep[ComplexInt]) = complex_int_minus(x,y)
    def *(y: Rep[ComplexInt]) = complex_int_times(x,y)
    def /(y: Rep[ComplexInt]) = complex_int_divide(x,y)
    def abs = complex_int_abs(x)
    def exp = complex_int_exp(x)
  }
  
  // Arith implicit
  implicit def complexIntArith : Arith[ComplexInt] = new Arith[ComplexInt] {
    def +=(a: Rep[ComplexInt], b: Rep[ComplexInt])(implicit ctx: SourceContext) = repToComplexIntOps(a).+(b)
    def +(a: Rep[ComplexInt], b: Rep[ComplexInt])(implicit ctx: SourceContext) = repToComplexIntOps(a).+(b)
    def -(a: Rep[ComplexInt], b: Rep[ComplexInt])(implicit ctx: SourceContext) = repToComplexIntOps(a).-(b)
    def *(a: Rep[ComplexInt], b: Rep[ComplexInt])(implicit ctx: SourceContext) = repToComplexIntOps(a).*(b)
    def /(a: Rep[ComplexInt], b: Rep[ComplexInt])(implicit ctx: SourceContext) = repToComplexIntOps(a)./(b)
    def abs(a: Rep[ComplexInt])(implicit ctx: SourceContext) = repToComplexIntOps(a).abs
    def exp(a: Rep[ComplexInt])(implicit ctx: SourceContext) = repToComplexIntOps(a).exp
    
    def empty(implicit ctx: SourceContext) = ComplexInt(0, 0)
    def zero(a: Rep[ComplexInt])(implicit ctx: SourceContext) = empty
  }
  
  def complex_int_new(real: Rep[Int], imag: Rep[Int])(implicit ctx: SourceContext) : Rep[ComplexInt]
  
  // Accessors
  def complex_int_real(x: Rep[ComplexInt])(implicit ctx: SourceContext) : Rep[Int]
  def complex_int_imag(x: Rep[ComplexInt])(implicit ctx: SourceContext) : Rep[Int]
  
  // Operations
  def complex_int_conj(x: Rep[ComplexInt])(implicit ctx: SourceContext) : Rep[ComplexInt]
  //def complex_int_negate(x: Rep[ComplexInt])(implicit ctx: SourceContext) : Rep[ComplexInt]
  
  // Math ops
  /* def infix_+[L](lhs: L, rhs: Rep[ComplexInt])(implicit c: L => Rep[ComplexInt], ctx: SourceContext, o: Overloaded13): Rep[ComplexInt] = complex_int_plus(rhs,c(lhs))
  def infix_+[L:Manifest](lhs: Rep[L], rhs: Rep[ComplexInt])(implicit c: Rep[R] => Rep[L], ctx: SourceContext, o: Overloaded14): Rep[M[L]] = matrix_plus_scalar_withconvert[R,L,M[L]](toIntf(rhs),lhs)
  def infix_+[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: Rep[R])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[L]] => Interface[Matrix[L]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded15): Rep[M[R]] = matrix_plus_scalar_withconvert[L,R,M[R]](toIntf(lhs),rhs)
  def infix_+[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: R)(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[L]] => Interface[Matrix[L]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded16): Rep[M[R]] = matrix_plus_scalar_withconvert[L,R,M[R]](toIntf(lhs),unit(rhs))
  def infix_+[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Interface[Matrix[L]], rhs: Rep[M[R]])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded17): Rep[M[R]] = matrix_plus_withconvert[L,R,M[R]](lhs,toIntf(rhs))
  def infix_+[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: Rep[M[R]])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntfL: Rep[M[L]] => Interface[Matrix[L]], toIntfR: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded18): Rep[M[R]] = matrix_plus_withconvert[L,R,M[R]](toIntfL(lhs),toIntfR(rhs)) */
  
  def complex_int_plus(x: Rep[ComplexInt], y: Rep[ComplexInt])(implicit ctx: SourceContext) : Rep[ComplexInt]
  def complex_int_minus(x: Rep[ComplexInt], y: Rep[ComplexInt])(implicit ctx: SourceContext) : Rep[ComplexInt]
  def complex_int_times(x: Rep[ComplexInt], y: Rep[ComplexInt])(implicit ctx: SourceContext) : Rep[ComplexInt]
  def complex_int_divide(x: Rep[ComplexInt], y: Rep[ComplexInt])(implicit ctx: SourceContext) : Rep[ComplexInt]
  def complex_int_abs(x: Rep[ComplexInt])(implicit ctx: SourceContext) : Rep[ComplexInt]
  def complex_int_exp(x: Rep[ComplexInt])(implicit ctx: SourceContext) : Rep[ComplexInt]
}

trait ComplexIntOpsExp extends ComplexIntOps {
  this: OptiSDRExp =>
  
  // Object creation
  case class ComplexIntNew(real: Exp[Int], imag: Exp[Int]) extends Def[ComplexInt]
  
  def complex_int_new(real: Exp[Int], imag: Exp[Int])(implicit ctx: SourceContext) = reflectPure(ComplexIntNew(real, imag))
  
  // Accessors
  case class ComplexIntInt(x: Exp[ComplexInt]) extends Def[Int]
  case class ComplexIntImag(x: Exp[ComplexInt]) extends Def[Int]
  
  def complex_int_real(x: Exp[ComplexInt])(implicit ctx: SourceContext) = reflectPure(ComplexIntInt(x))
  def complex_int_imag(x: Exp[ComplexInt])(implicit ctx: SourceContext) = reflectPure(ComplexIntImag(x))
  
  // Operations
  case class ComplexIntConj(x: Exp[ComplexInt]) extends Def[ComplexInt]
  
  def complex_int_conj(x: Exp[ComplexInt])(implicit ctx: SourceContext) = reflectPure(ComplexIntConj(x))
  
  // Math operations
  case class ComplexIntPlus(x: Exp[ComplexInt], y: Exp[ComplexInt]) extends Def[ComplexInt]
  case class ComplexIntMinus(x: Exp[ComplexInt], y: Exp[ComplexInt]) extends Def[ComplexInt]
  case class ComplexIntTimes(x: Exp[ComplexInt], y: Exp[ComplexInt]) extends Def[ComplexInt]
  case class ComplexIntDivide(x: Exp[ComplexInt], y: Exp[ComplexInt]) extends Def[ComplexInt]
  case class ComplexIntAbs(x: Exp[ComplexInt]) extends Def[ComplexInt]
  case class ComplexIntExp(x: Exp[ComplexInt]) extends Def[ComplexInt]
  
  def complex_int_plus(x: Exp[ComplexInt], y: Exp[ComplexInt])(implicit ctx: SourceContext) = reflectPure(ComplexIntPlus(x, y))
  def complex_int_minus(x: Exp[ComplexInt], y: Exp[ComplexInt])(implicit ctx: SourceContext) = reflectPure(ComplexIntMinus(x, y))
  def complex_int_times(x: Exp[ComplexInt], y: Exp[ComplexInt])(implicit ctx: SourceContext) = reflectPure(ComplexIntTimes(x, y))
  def complex_int_divide(x: Exp[ComplexInt], y: Exp[ComplexInt])(implicit ctx: SourceContext) = reflectPure(ComplexIntDivide(x, y))
  def complex_int_abs(x: Exp[ComplexInt])(implicit ctx: SourceContext) = reflectPure(ComplexIntAbs(x))
  def complex_int_exp(x: Exp[ComplexInt])(implicit ctx: SourceContext) = reflectPure(ComplexIntExp(x))
  
  //def complex_int_negate(x: Exp[ComplexInt])(implicit ctx: SourceContext) = reflectPure(ComplexIntNew(0-x.real, 0-x.imag))
}

trait ComplexIntOpsExpOpt extends ComplexIntOpsExp {
  this: OptiSDRExp =>

  override def complex_int_plus(lhs: Exp[ComplexInt], rhs: Exp[ComplexInt])(implicit ctx: SourceContext) : Exp[ComplexInt] = (lhs,rhs) match {
    case (Def(ComplexIntNew(Const(xr), Const(xi))), Def(ComplexIntNew(Const(yr), Const(yi)))) => ComplexInt(xr+yr, xi+yi)
    case _ => super.complex_int_plus(lhs, rhs)
  }
  
  override def complex_int_minus(lhs: Exp[ComplexInt], rhs: Exp[ComplexInt])(implicit ctx: SourceContext) : Exp[ComplexInt] = (lhs,rhs) match {
    case (Def(ComplexIntNew(Const(xr), Const(xi))), Def(ComplexIntNew(Const(yr), Const(yi)))) => ComplexInt(xr-yr, xi-yi)
    case _ => super.complex_int_minus(lhs, rhs)
  }
  
  override def complex_int_times(lhs: Exp[ComplexInt], rhs: Exp[ComplexInt])(implicit ctx: SourceContext) : Exp[ComplexInt] = (lhs,rhs) match {
    case (Def(ComplexIntNew(Const(xr), Const(xi))), Def(ComplexIntNew(Const(yr), Const(yi)))) => ComplexInt(xr*yr-xi*yi,xr*yi+xi*yr)
    case _ => super.complex_int_times(lhs, rhs)
  }
}