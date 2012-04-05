package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object NestedMutableOpTestRunner extends OptiMLApplicationRunner with NestedMutableOpTest

trait NestedMutableOpTest extends OptiMLApplication {

  def main() = {

    var i = 0
    while(i < 2) {
      val vec1 = Matrix.zerosf(10,10)
      vec1 += Matrix.onesf(10,10)
      vec1.pprint
      i += 1
    }
  }
}
