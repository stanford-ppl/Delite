package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object MatrixOpsTestRunner extends OptiMLApplicationRunner with MatrixOpsTest

trait MatrixOpsTest extends OptiMLApplication {

  def main() = {

    val mat1 = Matrix.onesf(10,5)
    val mat2 = mat1.sigmoidf
    mat2.pprint

    val mat3 = mat2.t
    mat3.pprint

    val mat4 = mat3.sumCol
    mat4.pprint
  }
}
