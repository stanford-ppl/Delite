package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object BLASMMTestRunner extends OptiMLApplicationRunner with BLASMMTest

trait BLASMMTest extends OptiMLApplication {

  def main() = {

    var i = 0
    tic()
    while(i < 1) {
      val mat1 = Matrix.onesf(256,256)
      val mat2 = Matrix.onesf(256,256)
      val mat3 = mat1 * mat2
      println(mat3)
      i += 1
    }
    toc()
  }
}
