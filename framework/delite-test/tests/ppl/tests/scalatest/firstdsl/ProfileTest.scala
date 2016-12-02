package ppl.tests.scalatest.firstdsl

import ppl.tests.scalatest.{DeliteTestRunner,DeliteTestModule,DeliteSuite}

import org.scala_lang.virtualized.virtualize
import org.scala_lang.virtualized.SourceContext

object ProfileTestRunner extends DeliteTestRunner with ProfileApplicationRunner with ProfileTest
@virtualize
trait ProfileTest extends DeliteTestModule with ProfileApplication {
  def main() = {
      var acc = 0.0
      val time =
        profile (100) times {
          for (i <- 0 until 100000 :Rep[Range]) {
            acc += Math.exp(i)*Math.pow(i,10.0)*42.0
          }
        } report average

    collect(time > 0.0)
    mkReport
  }
}

class FirstDSLTestSuite extends DeliteSuite {
  def testProfileDSL() { compileAndTest(ProfileTestRunner, enforceFullCoverage = false) }
}
