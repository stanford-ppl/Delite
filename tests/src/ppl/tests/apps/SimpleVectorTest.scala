package ppl.tests.apps

import ppl.delite.framework.DeliteApplication
import ppl.tests.dsls.{SimpleFloatVectorGeneratorC, SimpleFloatVectorGeneratorScala, SimpleFloatVector}

object SimpleVectorTest extends DeliteApplication with SimpleFloatVector {

  //todo remove this from here
  val scg = new SimpleFloatVectorGeneratorScala {
    val intermediate: DeliteApplication.this.type = DeliteApplication.this
  }
  val ccg = new SimpleFloatVectorGeneratorC {
    val intermediate: DeliteApplication.this.type = DeliteApplication.this
  }
  generators = generators :: scg :: ccg 

  def main() {
    println("SimpleVectorTest")
    val vec1 = zeros(10)
    val vec2 = zeros(10)

    val vec3 = vec1 + vec2

    println(vec3.pprint)
  }
}