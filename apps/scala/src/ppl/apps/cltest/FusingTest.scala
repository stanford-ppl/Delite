package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object FusingTestRunner extends OptiMLApplicationRunner with FusingTest

trait FusingTest extends OptiMLApplication {

  def main() = {

    val vec1 = Vector.zerosf(100).mutable
    val vec2 = Vector.onesf(100)
    val vec3 = vec1 + vec2
    val vec4 = vec3 * vec2
    val vec5 = vec4 - vec4
    val vec6 = vec4 + vec4
    vec1 += vec6

    vec1.pprint
    vec5.pprint
    vec6.pprint

  }
}
