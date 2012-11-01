package ppl.tests.scalatest.firstdsl

import ppl.tests.scalatest.{DeliteTestRunner,DeliteTestModule,DeliteSuite}

object ProfileTestRunner extends DeliteTestRunner with ProfileApplicationRunner with ProfileTest
trait ProfileTest extends DeliteTestModule with ProfileApplication {
  def main() = {
      var acc = 0.
      val time = 
        profile (100) times {
          for (i <- 0 until 100000) {
            acc += Math.exp(i)*Math.pow(i,10.0)*42.0
          }
        } report average
    
    collect(time > 0.0)
    mkReport
  }
}

class FirstDSLTestSuite extends DeliteSuite {
  def testProfileDSL() { compileAndTest(ProfileTestRunner) }
}
