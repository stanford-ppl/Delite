package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object CondTestRunner extends OptiMLApplicationRunner with CondTest

trait CondTest extends OptiMLApplication {

  def main() = {

    val vec1 = Vector.onesf(10)

    if(vec1(0) == 1.0f) {
      val vec2 = vec1 + Vector.onesf(10)
      vec2.pprint
    }
    else {
      vec1.pprint
    }

  }
}
