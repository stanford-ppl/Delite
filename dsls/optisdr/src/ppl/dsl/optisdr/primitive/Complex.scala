package ppl.dsl.optisdr.primitive

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

import ppl.dsl.optisdr._

trait ComplexOps extends Variables {
  this: OptiSDR =>
  
  // Static methods
  object Complex {
    def apply(real: Rep[Real], imag: Rep[Real])(implicit ctx: SourceContext) = complex_obj_new(real, imag) 
  }
  
  implicit def repToComplexOps(x: Rep[Complex]) = new ComplexOpsCls(x)
  implicit def varToComplexOps(x: Var[Complex]) = new ComplexOpsCls(readVar(x))
  
  // Objects methods
  class ComplexOpsCls(x: Rep[Complex]) {
    // def conj
    def real = complex_real(x)
    def imag = complex_imag(x)
  }
  
  def complex_obj_new(real: Rep[Real], imag: Rep[Real])(implicit ctx: SourceContext) : Rep[Complex]
  
  // Accessors
  def complex_real(x: Rep[Complex])(implicit ctx: SourceContext) : Rep[Real]
  def complex_imag(x: Rep[Complex])(implicit ctx: SourceContext) : Rep[Real]
  
  // Implicit converts to complex
  implicit def repComplexIntToRepComplex(x: Rep[ComplexInt]): Rep[Complex]
  
  // Operations
  def complex_negate(x: Rep[Complex])(implicit ctx: SourceContext) : Rep[Complex]
  
  // Math operations
/*  def infix_+(lhs: Rep[Int], rhs: Rep[Complex])(ctx: SourceContext, o: Overloaded1): Rep[ComplexInt] = matrix_plus_scalar[Double,M[Double]](toIntf(rhs),repIntToRepDouble(lhs))
  def infix_+[M[X] <: Matrix[X]](lhs: Rep[Int], rhs: Rep[M[Float]])(implicit mb: MatrixBuilder[Float,M[Float]], toIntf: Rep[M[Float]] => Interface[Matrix[Float]], m: Manifest[M[Float]], ctx: SourceContext, o: Overloaded20): Rep[M[Float]] = matrix_plus_scalar[Float,M[Float]](toIntf(rhs),repIntToRepFloat(lhs))
  def infix_+[M[X] <: Matrix[X]](lhs: Rep[Float], rhs: Rep[M[Double]])(implicit mb: MatrixBuilder[Double,M[Double]], toIntf: Rep[M[Double]] => Interface[Matrix[Double]], m: Manifest[M[Double]], ctx: SourceContext, o: Overloaded21): Rep[M[Double]] = matrix_plus_scalar[Double,M[Double]](toIntf(rhs),repFloatToRepDouble(lhs))
  def infix_+[M[X] <: Matrix[X]](lhs: Float, rhs: Rep[M[Int]])(implicit mb: MatrixBuilder[Float,M[Float]], toIntf: Rep[M[Int]] => Interface[Matrix[Int]], m: Manifest[M[Float]], ctx: SourceContext, o: Overloaded22): Rep[M[Float]] = matrix_plus_scalar_withconvert[Int,Float,M[Float]](toIntf(rhs),unit(lhs))
  def infix_+[M[X] <: Matrix[X]](lhs: Double, rhs: Rep[M[Int]])(implicit mb: MatrixBuilder[Double,M[Double]], toIntf: Rep[M[Int]] => Interface[Matrix[Int]], m: Manifest[M[Double]], ctx: SourceContext, o: Overloaded23): Rep[M[Double]] = matrix_plus_scalar_withconvert[Int,Double,M[Double]](toIntf(rhs),unit(lhs))
  def infix_+[M[X] <: Matrix[X]](lhs: Double, rhs: Rep[M[Float]])(implicit mb: MatrixBuilder[Double,M[Double]], toIntf: Rep[M[Float]] => Interface[Matrix[Float]], m: Manifest[M[Double]], ctx: SourceContext, o: Overloaded24): Rep[M[Double]] = matrix_plus_scalar_withconvert[Float,Double,M[Double]](toIntf(rhs),unit(lhs)) */

}

trait ComplexOpsExp extends ComplexOps {
  this: OptiSDRExp =>
  
  // Object creation
  case class ComplexObjNew(real: Exp[Real], imag: Exp[Real]) extends Def[Complex]
  
  def complex_obj_new(real: Exp[Real], imag: Exp[Real])(implicit ctx: SourceContext) = reflectPure(ComplexObjNew(real, imag))
  
  // Accessors
  case class ComplexReal(x: Exp[Complex]) extends Def[Real]
  case class ComplexImag(x: Exp[Complex]) extends Def[Real]
  
  def complex_real(x: Exp[Complex])(implicit ctx: SourceContext) = reflectPure(ComplexReal(x))
  def complex_imag(x: Exp[Complex])(implicit ctx: SourceContext) = reflectPure(ComplexImag(x))
  
  // Implicit convert to complex
  def repComplexIntToRepComplex(x: Rep[ComplexInt]) = complex_obj_new(x.real, x.imag)
  
  // Operations
  def complex_conj(x: Exp[Complex])(implicit ctx: SourceContext) = complex_obj_new(x.real, 0.0-x.imag)
  def complex_negate(x: Exp[Complex])(implicit ctx: SourceContext) = complex_obj_new(0.0-x.real, 0.0-x.imag)
}