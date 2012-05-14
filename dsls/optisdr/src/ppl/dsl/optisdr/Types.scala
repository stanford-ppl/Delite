package ppl.dsl.optisdr

import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.dsl.optila.DenseVector

/* Primitive types */

/* Int is built in
 * Meant to represent Integers of various sizes in the TI generator
 */

// Unsigned integers of various sizes
trait UInt
 
/* Real is type aliased to Double in OptiSDR
 * Meant to represent all kinds of non-integer numbers, including fixed point and floating point
 * Always signed. There doesn't seem to be a need for unsigned non-integer types
 */

// Complex non-integer numbers
abstract class Complex

// Complex integers (is this needed even?)
trait ComplexInt

/* Bits */
trait Bits // extends DeliteCollection[Boolean]

// A soft bit is a probabilistic measure of a bit. For example, if a SoftBit was represented
trait SoftBit

// To express belongsto Ranges
class Range[T](lo: T, hi: T)

// To express belongs an enum
class ParamEnum[T](xs: T*) 

// Stream trait? so that width assert will only apply to them?
trait Stream[T] extends DeliteCollection[T]
