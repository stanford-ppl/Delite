package ppl.tests.scalatest.delite

import ppl.tests.scalatest._
import ppl.delite.framework.datastructures._

object DeliteSimpleMultiArrayRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner with DeliteSimpleMultiArray
trait DeliteSimpleMultiArray extends DeliteTestModule with DeliteTestDSLApplication {
  def main() = {

    val vec = DeliteMultiArray.fromFunction(10){i => 1.0f}
    vec foreach {k => collect(k == 1.0f)}

    mkReport
  }
}

class DeliteMultiArraySuite extends DeliteSuite {
  def testSimpleMultiArray() { compileAndTest(DeliteSimpleMultiArrayRunner) }
}
