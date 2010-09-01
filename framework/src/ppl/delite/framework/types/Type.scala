package ppl.delite.framework.types

/**
 * Type provides hooks so that we are able to generate these types to a variety of architectures
 */
abstract class Type

case class IntT extends Type
case class FloatT extends Type
case class BoolT extends Type