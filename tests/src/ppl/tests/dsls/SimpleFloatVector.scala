package ppl.tests.dsls

import ppl.delite.framework.DSLType


trait SimpleFloatVector extends DSLType {

  case class Zeros(n: Rep[Int]) extends Def[SimpleFloatVector]
  case class VectorPlus(v1: Rep[SimpleFloatVector], v2: Rep[SimpleFloatVector]) extends Def[SimpleFloatVector]
  case class VectorApply(v: Rep[SimpleFloatVector], i: Rep[Int]) extends Def[Float]
  case class VectorUpdate(v: Rep[SimpleFloatVector], i: Rep[Int]) extends Def[Unit]
  case class PPrint(v: Rep[SimpleFloatVector]) extends Def[String]

  def zeros(n: Rep[Int]): Rep[SimpleFloatVector] = Zeros(n)
  def __ext__+(v1: Rep[SimpleFloatVector], v2: Rep[SimpleFloatVector]): Rep[SimpleFloatVector] = VectorPlus(v1, v2)

  //todo could we extend the __ext__ feature to handle this like apply
  class SimpleFloatVectorOps(v: Rep[SimpleFloatVector]) {
    def apply(i: Rep[Int]): Rep[Float] = reflectEffect(VectorApply(v,i))
    def update(i: Rep[Int], f: Rep[Float]) = VectorUpdate(v,i)
    def pprint: Rep[String] = PPrint(v)
  }

  //todo, need to be able to only import this stuff automatically
  implicit def injectOpsSFV(v:Rep[SimpleFloatVector]) = new SimpleFloatVectorOps(v)

}

//abstract class SimpleFloatVector
