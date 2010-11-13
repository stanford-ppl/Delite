package ppl.delite.framework.datafields

abstract class Field

case class ScalarField() extends Field
case class ArrayField() extends Field 