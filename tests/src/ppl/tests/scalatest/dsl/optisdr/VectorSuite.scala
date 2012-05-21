package ppl.tests.scalatest.dsl.optisdr

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optisdr.{OptiSDRApplicationRunner, OptiSDRApplication, Stream} // Need the Stream import otherwise we get Scala's stream
import ppl.tests.scalatest._

/* Testing OptiSDR stream operations functionality
 *
 * author:  Michael Wu (michaelmwu@stanford.edu)
 * created: 3/04/12
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */
 
object VectorTestRunner extends DeliteTestRunner with OptiSDRApplicationRunner with VectorApp
trait VectorApp extends DeliteTestModule with OptiSDRApplication {
  def main() = {
    // FFT
  
    collect(true)
    
    mkReport
  }
}

class VectorSuite extends DeliteSuite {
  def testVectorOps() { compileAndTest(VectorTestRunner) }
}

