package ppl.tests.dsls

import ppl.delite.framework.DSLType


trait SimpleFloatVector extends DSLType {

  def zeros(n: Rep[Int]): Rep[SimpleFloatVector] = nop
  def infix_+(v1: Rep[SimpleFloatVector], v2: Rep[SimpleFloatVector]): Rep[SimpleFloatVector] = nop

  //todo could we extend the __ext__ feature to handle this like apply
  class SimpleFloatVectorOps(v: Rep[SimpleFloatVector]) {
    def apply(i: Rep[Int]): Rep[Float] = nop
    def update(i: Rep[Int], f: Rep[Float]) = nop
    def pprint = nop
  }

  //todo, need to be able to only import this stuff automatically
  implicit def injectOpsSFV(v:Rep[SimpleFloatVector]) = new SimpleFloatVectorOps(v)

}

//abstract class SimpleFloatVector
