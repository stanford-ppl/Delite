package ppl.delite.framework.datastructures

abstract class Field

case class ScalarField() extends Field
case class ArrayField() extends Field 