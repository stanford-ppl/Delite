package ppl.delite.framework

import datastructures.ScalarField
import types.Type
import scala.virtualization.lms.common.{EffectExp}

trait DSLType {

  def scalar[T](name: String, tp: Type[T]): ScalarField = nop
  //todo rework this as a collection hiearchy similar to Scala collections
  def array[T](name: String, tp: Type[T])(size: Int) = nop

  //def addCodeGenerator(cg: CodeGenerator)


  private def nop = throw new RuntimeException("not implemented")
}
