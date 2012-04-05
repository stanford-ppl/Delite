package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object VectorOpsTestRunner extends OptiMLApplicationRunner with VectorOpsTest

trait VectorOpsTest extends OptiMLApplication {

  def main() = {

    val vec1 = Vector.onesf(10).mutable
    vec1(3) = 0.4f
    vec1(5) = 2.5f
    val mat1 = vec1.replicate(3,1)
    mat1.pprint
  }
}
