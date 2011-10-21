package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object CondRetTestRunner extends OptiMLApplicationRunner with CondRetTest

trait CondRetTest extends OptiMLApplication {

  def main() = {

    val vec1 = Vector.onesf(10).mutable
    vec1(3) = 3.4f
    val vec2 = Vector.zerosf(10).mutable
    vec2(5) = 2.8f

    val out = if(vec1(0) != 1.0f) vec1 + vec1
              else vec1 - vec2

    //TODO: Check if the result is correct: It prints -0.7999995 instead of -0.8
    val out2 = out + vec1

    out2.pprint
  }
}
