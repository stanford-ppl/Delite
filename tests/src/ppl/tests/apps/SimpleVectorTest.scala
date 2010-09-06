package ppl.tests.apps

import ppl.delite.framework.DeliteApplication
import ppl.tests.dsls.SimpleFloatVector

object SimpleVectorTest extends DeliteApplication with SimpleFloatVector {

  def main() {
    println("SimpleVectorTest")
    val vec1 = zeros(10)
    val vec2 = zeros(10)

    val vec3 = vec1 + vec2

    println(vec3.pprint)
  }
}