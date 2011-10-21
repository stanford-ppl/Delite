package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object BLASMVTestRunner extends OptiMLApplicationRunner with BLASMVTest

trait BLASMVTest extends OptiMLApplication {

  def main() = {

    //val mat1 = Matrix.onesf(512,1024)
    //val vec1 = Vector.onesf(1024)
    val mat1 = Matrix.onesf(100,1448)
    val vec1 = Vector.onesf(1448)

    tic()
    val vec2 = mat1 * vec1
    toc(vec2)
    println(vec2)
  }
}
