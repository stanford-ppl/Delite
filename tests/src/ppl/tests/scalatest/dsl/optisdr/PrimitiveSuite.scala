package ppl.tests.scalatest.dsl.optisdr

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optisdr.{OptiSDRApplicationRunner, OptiSDRApplication}
import ppl.tests.scalatest._

/* Testing OptiSDR primitives functionality
 *
 * author:  Michael Wu (michaelmwu@stanford.edu)
 * created: 3/04/12
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

object ComplexTestRunner extends DeliteTestRunner with OptiSDRApplicationRunner with ComplexTest
trait ComplexTest extends DeliteTestModule with OptiSDRApplication {
  def main() = {
  }
}

class PrimitiveSuite extends DeliteSuite {
  def testComplexOps() { compileAndTest(ComplexTestRunner) }
}

