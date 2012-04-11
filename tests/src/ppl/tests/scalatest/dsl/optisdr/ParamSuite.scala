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

object EnumRangeTestRunner extends DeliteTestRunner with OptiSDRApplicationRunner with EnumRangeTest
trait EnumRangeTest extends DeliteTestModule with OptiSDRApplication {
  def main() = {
    val a = ParamEnum(1, 2, 3)
	
	val b = Range(2, 10)
  }
}

class ParamSuite extends DeliteSuite {
  def testEnumRange() { compileAndTest(EnumRangeTestRunner) }
}

