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
    // def conj
    def real = complex_int_real(x)
    def imag = complex_int_imag(x)
  }
  
  def complex_int_new(real: Rep[Int], imag: Rep[Int])(implicit ctx: SourceContext) : Rep[ComplexInt]
  
  // Accessors
  def complex_int_real(x: Rep[ComplexInt])(implicit ctx: SourceContext) : Rep[Int]
  def complex_int_imag(x: Rep[ComplexInt])(implicit ctx: SourceContext) : Rep[Int]
  
  // Operations
  //def complex_int_negate(x: Rep[ComplexInt])(implicit ctx: SourceContext) : Rep[ComplexInt]
  
  // Math ops
  /* def infix_+[L](lhs: L, rhs: Rep[ComplexInt])(implicit c: L => Rep[ComplexInt], ctx: SourceContext, o: Overloaded13): Rep[ComplexInt] = complex_int_plus(rhs,c(lhs))
  def infix_+[L:Manifest](lhs: Rep[L], rhs: Rep[ComplexInt])(implicit c: Rep[R] => Rep[L], ctx: SourceContext, o: Overloaded14): Rep[M[L]] = matrix_plus_scalar_withconvert[R,L,M[L]](toIntf(rhs),lhs)
  def infix_+[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: Rep[R])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[L]] => Interface[Matrix[L]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded15): Rep[M[R]] = matrix_plus_scalar_withconvert[L,R,M[R]](toIntf(lhs),rhs)
  def infix_+[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: R)(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[L]] => Interface[Matrix[L]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded16): Rep[M[R]] = matrix_plus_scalar_withconvert[L,R,M[R]](toIntf(lhs),unit(rhs))
  def infix_+[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Interface[Matrix[L]], rhs: Rep[M[R]])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntf: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded17): Rep[M[R]] = matrix_plus_withconvert[L,R,M[R]](lhs,toIntf(rhs))
  def infix_+[L:Manifest,R:Arith:Manifest,M[X] <: Matrix[X]](lhs: Rep[M[L]], rhs: Rep[M[R]])(implicit c: Rep[L] => Rep[R], mb: MatrixBuilder[R,M[R]], toIntfL: Rep[M[L]] => Interface[Matrix[L]], toIntfR: Rep[M[R]] => Interface[Matrix[R]], m: Manifest[M[R]], ctx: SourceContext, o: Overloaded18): Rep[M[R]] = matrix_plus_withconvert[L,R,M[R]](toIntfL(lhs),toIntfR(rhs)) */
  
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
  //def complex_int_negate(x: Exp[ComplexInt])(implicit ctx: SourceContext) = reflectPure(ComplexIntNew(0-x.real, 0-x.imag))
}