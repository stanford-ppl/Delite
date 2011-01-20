package ppl.dsl.tests.GPU_tests

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication


object RBM_kernels extends DeliteApplication with OptiMLExp {

  def main() {

    println("RBM kernel generation")

    val mat1 = Matrix[Float](10,10)
    val mat1_out = mat1.sigmoidf
    println(mat1_out)

    val vec2 = Vector[Float](10)
    val vec2_out = vec2.replicate(10,1)
    println(vec2_out)
    
    val mat3 = Matrix[Float](10,10)
    val mat3_out = mat3.sumCol
    println(mat3_out)
  }
}
