package ppl.dsl.tests

import ppl.delite.framework.DeliteApplication
import ppl.dsl.simple.OptiML2


object SimpleVectorTest extends DeliteApplication with OptiML2 {

  def main() {
    println("SimpleVectorTest")
    val vec1 = Vector.zeros(10)
    val vec2 = Vector.zeros(10)
    val mat1 = Matrix.zeros(10,10)
    val mat2 = Matrix.zeros(10,10)   

    val vec3 = vec1 + vec2
    val mat3 = mat1 + mat2

    println(vec3.pprint)
    println(mat3.pprint)


  }
}