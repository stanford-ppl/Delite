package ppl.dsl.optisdr.datastruct.scala

object Complex {
  def apply(real: Double, imag: Double) = new Complex(real, imag)
}

class Complex(val real: Double, val imag: Double)
