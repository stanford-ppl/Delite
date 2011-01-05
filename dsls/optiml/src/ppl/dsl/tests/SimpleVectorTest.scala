package ppl.dsl.tests

import ppl.delite.framework.DeliteApplication
import ppl.dsl.simple.{OptiML2Exp, OptiML2}

object SimpleVectorTest extends DeliteApplication with OptiML2Exp {

  def main() {
    //println("SimpleVectorTest")
    val vec1 = Vector.intZeros(10)
    val vec2 = Vector.intZeros(10)
    /*
    val vec3 = if(vec1.length > 10) {
      if(vec2.length > 10) {
        vec1 + vec2
      } else {
        vec2 - vec1
      }
    } else {
      vec1 - vec2
    }

    for(i <- 0 until vec3(0)) {
      vec3(i) = vec1(i)
    }
    */


    var idx = unit(0)

    while(idx < 10) {
      vec2(idx) = vec1(idx)
      vec2(idx) = vec1(idx)
      vec2(idx) = vec1(idx)
      vec2(idx) = vec1(idx)
      //idx = unit(10)
    }


    //println(vec3)

  }
}