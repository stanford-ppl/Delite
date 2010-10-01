package ppl.tests.standalone.simple_dsl

import ppl.delite.framework.DeliteApplication
import ppl.tests.dsls.{SimpleFloatVectorGeneratorC, SimpleFloatVectorGeneratorScala, SimpleFloatVector}


object SimpleVectorTest extends DeliteApplication with SimpleFloatVector with SimpleFloatMatrix {

  def main() {
    println("SimpleVectorTest")
    val vec1 = vzeros(10)
    val vec2 = vzeros(10)
    val mat1 = mzeros(10)
    val mat2 = mzeros(10)

    val vec3 = vec1 + vec2
    val mat3 = mat1 + mat2

    println(vec3.pprint)
    println(mat3.pprint)


  }
}