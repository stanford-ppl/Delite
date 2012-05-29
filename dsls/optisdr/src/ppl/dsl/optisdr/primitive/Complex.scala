package ppl.dsl.optisdr.primitive

import java.io.{PrintWriter}

import scala.reflect.Manifest
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import ppl.dsl.optisdr._

trait ComplexOps extends Variables {
  this: OptiSDR =>
  
  // Static methods
  object Complex {
    def apply(real: Real, imag: Real)(implicit ctx: SourceContext) = complex_new(unit(real), unit(imag)) 
    def apply(real: Rep[Real], imag: Real)(implicit ctx: SourceContext) = complex_new(real, unit(imag))
    def apply(real: Real, imag: Rep[Real])(implicit ctx: SourceContext) = complex_new(unit(real), imag)
    def apply(real: Rep[Real], imag: Rep[Real])(implicit ctx: SourceContext) = complex_new(real, imag) 
  }
  
  implicit def repToComplexOps(x: Rep[Complex]) = new ComplexOpsCls(x)
  implicit def varToComplexOps(x: Var[Complex]) = new ComplexOpsCls(readVar(x))
  
  // Objects methods
  class ComplexOpsCls(x: Rep[Complex]) {
    def conj(implicit ctx: SourceContext) = complex_conj(x)
    def real(implicit ctx: SourceContext) = complex_real(x)
    def realValue(implicit ctx: SourceContext) = complex_real(x)
    def imag(implicit ctx: SourceContext) = complex_imag(x)
    
    def +(y: Rep[Complex])(implicit ctx: SourceContext) = complex_plus(x,y)
    def -(y: Rep[Complex])(implicit ctx: SourceContext) = complex_minus(x,y)
    def *(y: Rep[Complex])(implicit ctx: SourceContext) = complex_times(x,y)
    def /(y: Rep[Complex])(implicit ctx: SourceContext) = complex_divide(x,y)
    def abs(implicit ctx: SourceContext) = complex_abs(x)
    def exp(implicit ctx: SourceContext) = complex_exp(x)
  }
  
  def complex_new(real: Rep[Real], imag: Rep[Real])(implicit ctx: SourceContext) : Rep[Complex]
  
  // Accessors
  def complex_real(x: Rep[Complex])(implicit ctx: SourceContext) : Rep[Real]
  def complex_imag(x: Rep[Complex])(implicit ctx: SourceContext) : Rep[Real]
  
  // Operations
  def complex_conj(x: Rep[Complex])(implicit ctx: SourceContext) : Rep[Complex]
  //def complex_negate(x: Rep[Complex])(implicit ctx: SourceContext) : Rep[Complex]
  
  // Arith implicit
  implicit def complexArith : Arith[Complex] = new Arith[Complex] {
    def +=(a: Rep[Complex], b: Rep[Complex])(implicit ctx: SourceContext) = repToComplexOps(a).+(b)
    def +(a: Rep[Complex], b: Rep[Complex])(implicit ctx: SourceContext) = repToComplexOps(a).+(b)
    def -(a: Rep[Complex], b: Rep[Complex])(implicit ctx: SourceContext) = repToComplexOps(a).-(b)
    def *(a: Rep[Complex], b: Rep[Complex])(implicit ctx: SourceContext) = repToComplexOps(a).*(b)
    def /(a: Rep[Complex], b: Rep[Complex])(implicit ctx: SourceContext) = repToComplexOps(a)./(b)
    def abs(a: Rep[Complex])(implicit ctx: SourceContext) = repToComplexOps(a).abs
    def exp(a: Rep[Complex])(implicit ctx: SourceContext) = repToComplexOps(a).exp
    
    def empty(implicit ctx: SourceContext) = Complex(0.0, 0.0)
    def zero(a: Rep[Complex])(implicit ctx: SourceContext) = empty
  }
  
  // Math operations
  def complex_plus(x: Rep[Complex], y: Rep[Complex])(implicit ctx: SourceContext) : Rep[Complex]
  def complex_minus(x: Rep[Complex], y: Rep[Complex])(implicit ctx: SourceContext) : Rep[Complex]
  def complex_times(x: Rep[Complex], y: Rep[Complex])(implicit ctx: SourceContext) : Rep[Complex]
  def complex_divide(x: Rep[Complex], y: Rep[Complex])(implicit ctx: SourceContext) : Rep[Complex]
  def complex_abs(x: Rep[Complex])(implicit ctx: SourceContext) : Rep[Complex]
  def complex_exp(x: Rep[Complex])(implicit ctx: SourceContext) : Rep[Complex]
  
  // Extra math operations to handle operations between different types
  //def infix_+(lhs: Rep[Int], rhs: Rep[Complex])(ctx: SourceContext, o: Overloaded1): Rep[Complex]
  //def infix_+(lhs: Rep[Real], rhs: Rep[Complex])(ctx: SourceContext, o: Overloaded2): Rep[Complex]
/*  def infix_+[M[X] <: Matrix[X]](lhs: Rep[Int], rhs: Rep[M[Float]])(implicit mb: MatrixBuilder[Float,M[Float]], toIntf: Rep[M[Float]] => Interface[Matrix[Float]], m: Manifest[M[Float]], ctx: SourceContext, o: Overloaded20): Rep[M[Float]] = matrix_plus_scalar[Float,M[Float]](toIntf(rhs),repIntToRepFloat(lhs))
  def infix_+[M[X] <: Matrix[X]](lhs: Rep[Float], rhs: Rep[M[Double]])(implicit mb: MatrixBuilder[Double,M[Double]], toIntf: Rep[M[Double]] => Interface[Matrix[Double]], m: Manifest[M[Double]], ctx: SourceContext, o: Overloaded21): Rep[M[Double]] = matrix_plus_scalar[Double,M[Double]](toIntf(rhs),repFloatToRepDouble(lhs))
  def infix_+[M[X] <: Matrix[X]](lhs: Float, rhs: Rep[M[Int]])(implicit mb: MatrixBuilder[Float,M[Float]], toIntf: Rep[M[Int]] => Interface[Matrix[Int]], m: Manifest[M[Float]], ctx: SourceContext, o: Overloaded22): Rep[M[Float]] = matrix_plus_scalar_withconvert[Int,Float,M[Float]](toIntf(rhs),complex(lhs))
  def infix_+[M[X] <: Matrix[X]](lhs: Double, rhs: Rep[M[Int]])(implicit mb: MatrixBuilder[Double,M[Double]], toIntf: Rep[M[Int]] => Interface[Matrix[Int]], m: Manifest[M[Double]], ctx: SourceContext, o: Overloaded23): Rep[M[Double]] = matrix_plus_scalar_withconvert[Int,Double,M[Double]](toIntf(rhs),complex(lhs))
  def infix_+[M[X] <: Matrix[X]](lhs: Double, rhs: Rep[M[Float]])(implicit mb: MatrixBuilder[Double,M[Double]], toIntf: Rep[M[Float]] => Interface[Matrix[Float]], m: Manifest[M[Double]], ctx: SourceContext, o: Overloaded24): Rep[M[Double]] = matrix_plus_scalar_withconvert[Float,Double,M[Double]](toIntf(rhs),complex(lhs)) */
  
  // Conversions
  def complex_int_value(x: Rep[Complex])(implicit ctx: SourceContext) : Rep[Int]
  def complex_uint_value(x: Rep[Complex])(implicit ctx: SourceContext) : Rep[UInt]
}

trait ComplexOpsExp extends ComplexOps with VariablesExp with BaseFatExp {
  this: OptiSDRExp =>
  
  // Object creation
  case class ComplexNew(real: Exp[Real], imag: Exp[Real]) extends Def[Complex]
  
  def complex_new(real: Exp[Real], imag: Exp[Real])(implicit ctx: SourceContext) = reflectPure(ComplexNew(real, imag))
  
  // Accessors
  case class ComplexReal(x: Exp[Complex]) extends Def[Real]
  case class ComplexImag(x: Exp[Complex]) extends Def[Real]
  
  def complex_real(x: Exp[Complex])(implicit ctx: SourceContext) = reflectPure(ComplexReal(x))
  def complex_imag(x: Exp[Complex])(implicit ctx: SourceContext) = reflectPure(ComplexImag(x))
  
  // Operations
  case class ComplexConj(x: Exp[Complex]) extends Def[Complex]
  
  case class ComplexSqNorm(x: Exp[Complex]) extends Def[Real]
  
  def complex_conj(x: Exp[Complex])(implicit ctx: SourceContext) = reflectPure(ComplexConj(x))
  //def complex_negate(x: Exp[Complex])(implicit ctx: SourceContext) = complex_obj_new(0.0-x.real, 0.0-x.imag)
  
  // Math operations
  case class ComplexPlus(x: Exp[Complex], y: Exp[Complex]) extends Def[Complex]
  case class ComplexMinus(x: Exp[Complex], y: Exp[Complex]) extends Def[Complex]
  case class ComplexTimes(x: Exp[Complex], y: Exp[Complex]) extends Def[Complex]
  case class ComplexDivide(x: Exp[Complex], y: Exp[Complex]) extends Def[Complex]
  case class ComplexAbs(x: Exp[Complex]) extends Def[Complex]
  case class ComplexExp(x: Exp[Complex]) extends Def[Complex]
  
  def complex_plus(x: Exp[Complex], y: Exp[Complex])(implicit ctx: SourceContext) = reflectPure(ComplexPlus(x, y))
  def complex_minus(x: Exp[Complex], y: Exp[Complex])(implicit ctx: SourceContext) = reflectPure(ComplexMinus(x, y))
  def complex_times(x: Exp[Complex], y: Exp[Complex])(implicit ctx: SourceContext) = reflectPure(ComplexTimes(x, y))
  def complex_divide(x: Exp[Complex], y: Exp[Complex])(implicit ctx: SourceContext) = reflectPure(ComplexDivide(x, y))
  def complex_abs(x: Exp[Complex])(implicit ctx: SourceContext) = reflectPure(ComplexAbs(x))
  def complex_exp(x: Exp[Complex])(implicit ctx: SourceContext) = reflectPure(ComplexExp(x))
  
  //def infix_+(lhs: Exp[Int], rhs: Exp[Complex])(ctx: SourceContext, o: Overloaded1) = complex_obj_new(lhs + rhs.real, rhs.imag)
  //def infix_+(lhs: Exp[Real], rhs: Exp[Complex])(ctx: SourceContext, o: Overloaded2) = complex_obj_new(lhs + rhs.real, rhs.imag)
  
  // Conversions
  case class ComplexIntValue(x: Exp[Complex]) extends Def[Int]
  case class ComplexUIntValue(x: Exp[Complex]) extends Def[UInt]
  
  def complex_int_value(x: Exp[Complex])(implicit ctx: SourceContext) = reflectPure(ComplexIntValue(x))
  def complex_uint_value(x: Exp[Complex])(implicit ctx: SourceContext) = reflectPure(ComplexUIntValue(x))
}

trait ComplexOpsExpOpt extends ComplexOpsExp {
  this: OptiSDRExp =>

  override def complex_plus(lhs: Exp[Complex], rhs: Exp[Complex])(implicit ctx: SourceContext) : Exp[Complex] = (lhs,rhs) match {
    case (Def(ComplexNew(Const(xr), Const(xi))), Def(ComplexNew(Const(yr), Const(yi)))) => Complex(xr+yr, xi+yi)
    case _ => super.complex_plus(lhs, rhs)
  }
  
  override def complex_minus(lhs: Exp[Complex], rhs: Exp[Complex])(implicit ctx: SourceContext) : Exp[Complex] = (lhs,rhs) match {
    case (Def(ComplexNew(Const(xr), Const(xi))), Def(ComplexNew(Const(yr), Const(yi)))) => Complex(xr-yr, xi-yi)
    case _ => super.complex_minus(lhs, rhs)
  }
  
  override def complex_times(lhs: Exp[Complex], rhs: Exp[Complex])(implicit ctx: SourceContext) : Exp[Complex] = (lhs,rhs) match {
    case (Def(ComplexNew(Const(xr), Const(xi))), Def(ComplexNew(Const(yr), Const(yi)))) => Complex(xr*yr-xi*yi,xr*yi+xi*yr)
    case _ => super.complex_times(lhs, rhs)
  }
}

trait BaseGenComplexOps extends GenericFatCodegen {
  val IR: ComplexOpsExp
  import IR._
  
  override def unapplySimpleIndex(e: Def[Any]) = e match {
    // What is this for???
    case _ => super.unapplySimpleIndex(e)
  }  
}

trait ScalaGenComplexOps extends BaseGenComplexOps with ScalaGenFat {
  val IR: ComplexOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure

    case ComplexNew(real,imag) => emitValDef(sym, "Complex(" + quote(real) + "," + quote(imag) + ")")
    case ComplexPlus(lhs,rhs) => emitValDef(sym, "Complex(" + quote(lhs) + ".real + " + quote(rhs) + ".real," + quote(lhs) + ".imag + " + quote(rhs) + ".imag)")
    case ComplexMinus(lhs,rhs) => emitValDef(sym, "Complex(" + quote(lhs) + ".real - " + quote(rhs) + ".real," + quote(lhs) + ".imag - " + quote(rhs) + ".imag)")
    case ComplexTimes(lhs,rhs) => emitValDef(sym, "Complex(" + quote(lhs) + ".real * " + quote(rhs) + ".real - " + quote(lhs) + ".imag * " + quote(rhs) + ".imag," + quote(lhs) + ".real * " + quote(rhs) + ".imag + " + quote(lhs) + ".imag * " + quote(rhs) + ".real)")
    case ComplexDivide(lhs,rhs) => emitValDef(sym, "{val d = " + quote(rhs) + ".real * " + quote(rhs) + ".real + " + quote(rhs) + ".imag * " + quote(rhs) + ".imag;" +
      "Complex((" + quote(lhs) + ".real * " + quote(rhs) + ".real + " + quote(lhs) + ".imag * " + quote(rhs) + ".imag)/d,(" + quote(rhs) + ".real * " + quote(lhs) + ".imag - " + quote(lhs) + ".imag * " + quote(rhs) + ".real)/d)")
    
    case ComplexAbs(e) => emitValDef(sym, "Math.sqrt(" + quote(e) + ".real * " + quote(e) + ".real + " + quote(e) + ".imag * " + quote(e) + ".imag)")
    case ComplexExp(e) => emitValDef(sym, "{val c = exp(" + quote(e) + ".real);Complex(Math.cos(" + quote(e) + ".real), Math.sin(" + quote(e) + ".imag))")
    
    case _ => super.emitNode(sym, rhs)
  }
}