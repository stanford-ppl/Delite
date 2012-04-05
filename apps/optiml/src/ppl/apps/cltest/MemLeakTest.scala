package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object MemLeakTestRunner extends OptiMLApplicationRunner with MemLeakTest

trait MemLeakTest extends OptiMLApplication {

  def main() = {

    var i = 0
    while(i < 1000) {
      val vec1 = Matrix.onesf(  4000,4000)
      val vec2 = vec1 + 1.0f
      println(vec2)
      i += 1
    }
  }
}
