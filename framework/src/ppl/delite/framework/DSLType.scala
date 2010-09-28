package ppl.delite.framework

import datafields.ScalarField
import types.Type
import scala.virtualization.lms.common.{EffectExp}

trait DSLType extends EffectExp {

  def scalar[T](name: String, tp: Type[T]): ScalarField = nop
  //todo rework this as a collection hiearchy similar to Scala collections
  def array[T](name: String, tp: Type[T])(size: Int) = nop

  //def addCodeGenerator(cg: CodeGenerator)


  def nop = throw new RuntimeException("not implemented")
}
