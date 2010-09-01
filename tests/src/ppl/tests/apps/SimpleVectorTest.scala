package ppl.tests.apps

import ppl.delite.framework.DeliteApplication
import ppl.tests.dsls.SimpleFloatVector

object SimpleVectorTest extends DeliteApplication with SimpleFloatVector {

  def main() {
    println("SimpleVectorTest")
    val vec1 = zeros(10)
    val vec2 = zeros(10)

    for(i <- 0 until 10) {
      vec1(i) = i.toFloat
      vec2(i) = (2*i).toFloat
    }

    val vec3 = vec1 + vec2

    println(vec3.pprint)
  }
}