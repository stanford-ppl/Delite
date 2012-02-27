package src.ppl.dsl.optisdr

class Real(val v: Double)

trait ComplexNumeric extends Numeric[ComplexInt] {
  def plus(x: ComplexInt, y: ComplexInt): ComplexInt = ComplexInt(x.real + y.real, x.imag + y.imag)
  def minus(x: ComplexInt, y: ComplexInt): ComplexInt = ComplexInt(x.real - y.real, x.imag - y.imag)
  def times(x: ComplexInt, y: ComplexInt): ComplexInt = ComplexInt(x.real*y.real - x.imag*y.imag, x.real*y.imag + y.real*x.imag)
  def quot(x: ComplexInt, y: ComplexInt): ComplexInt = {
	val denom = y.real*y.real + y.imag*y.imag
	ComplexInt((x.real*y.real + x.imag*y.imag)/denom, -x.real*y.imag + y.real*x.imag/denom)
  }
  def rem(x: ComplexInt, y: ComplexInt): ComplexInt =  "%s%%s" format (x, y)
  def negate(x: ComplexInt): ComplexInt = ComplexInt(-x.real, -x.imag)
  def fromInt(x: Int): ComplexInt = ComplexInt(x, 0)
  def toInt(x: ComplexInt): Int = sqrt(x.real*x.real + x.imag*x.imag).toInt
  def toLong(x: ComplexInt): Long = sqrt(x.real*x.real + x.imag*x.imag).toLong
  def toFloat(x: ComplexInt): Float = sqrt(x.real*x.real + x.imag*x.imag)
  def toDouble(x: ComplexInt): Double = sqrt(x.real*x.real + x.imag*x.imag)
}