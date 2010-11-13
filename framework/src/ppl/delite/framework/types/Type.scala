package ppl.delite.framework.types

/**
 * Type provides hooks so that we are able to generate these types to a variety of architectures
 */
abstract class Type[T]

case class IntT() extends Type[Int]
case class FloatT() extends Type[Float]
case class BoolT() extends Type[Boolean]