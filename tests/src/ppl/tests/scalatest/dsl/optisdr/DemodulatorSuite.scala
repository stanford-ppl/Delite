package ppl.tests.scalatest.dsl.optisdr

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optisdr.{OptiSDRApplicationRunner, OptiSDRApplication, Stream} // Need the Stream import otherwise we get Scala's stream
import ppl.dsl.optisdr.wifi.Demodulator16x16
import ppl.tests.scalatest._

import ppl.dsl.optisdr._

/* Testing OptiSDR stream operations functionality
 *
 * author:  Michael Wu (michaelmwu@stanford.edu)
 * created: 3/04/12
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */
 
object DemodulatorTestRunner extends DeliteTestRunner with OptiSDRApplicationRunner with DemodulatorApp
trait DemodulatorApp extends DeliteTestModule with OptiSDRApplication with Demodulator16x16 {
  def main() = {
    val a = Vector[Complex](0, false).mutable
    
    for(i <- 0 until 80) {
      a += Complex(1, 1)
      a += Complex(-0.5, i / 80.0)
      a += Complex(0.2, 0.4)
      a += Complex(-i / 40.0, 0)
    }
    
    val c = demodulator16x16()(a.unsafeImmutable.toStream)
    
    collect(true)
    
    mkReport
  }
}

class DemodulatorSuite extends DeliteSuite {
  def testDemodulator() { compileAndTest(DemodulatorTestRunner) }
}

