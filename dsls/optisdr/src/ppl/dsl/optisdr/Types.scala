package ppl.dsl.optisdr

import ppl.delite.framework.datastruct.scala.DeliteCollection

/* Primitive types */
trait Integral
trait UInt
trait Real
abstract class Complex
trait ComplexInt

/* Bits */
trait Bits // extends DeliteCollection[Boolean]