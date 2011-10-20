package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object ComputeTestRunner extends OptiMLApplicationRunner with ComputeTest

trait ComputeTest extends OptiMLApplication {

  def main() = {

    val matrix1 = Matrix.onesf(512,512)

    tic()

    val matrix2 = matrix1.map(ele => 
        {
          var i = 0
          var acc = 1.01f
          while(i < 1024) {
            acc = acc * ele
            i += 1
          }
          acc
        }
    )
    println(matrix2)
    toc(matrix2)
    println(matrix2(0,0))
  }
}
