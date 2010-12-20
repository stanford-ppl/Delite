package ppl.dsl.tests

import ppl.delite.framework.DeliteApplication
import ppl.dsl.simple.{OptiML2Exp, OptiML2}

object SimpleVectorTest extends DeliteApplication with OptiML2Exp {

  def main() {
    //println("SimpleVectorTest")
    val vec1 = Vector.zeros(10)
    val vec2 = Vector.zeros(10)

    val vec3 = if(vec1.length > 10) {
      vec1 + vec2
    } else {
      vec1 - vec2
    }

    println(vec3)

  }
}