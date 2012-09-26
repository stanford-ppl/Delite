package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object MutationTestRunner extends OptiMLApplicationRunner with MutationTest

trait MutationTest extends OptiMLApplication {

  def main() = {

    tic()

    val vec1 = Vector.zerosf(10).mutable  

    vec1(4) = 3.4f                

    val vec2 = vec1.filter(e => e > 2.0f )  

    vec2.pprint                 

    toc(vec2)
  }
}
