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

object RangeTestRunner extends DeliteTestRunner with OptiSDRApplicationRunner with RangeTest
trait RangeTest extends DeliteTestModule with OptiSDRApplication {
  def main() = {
    // Make ranges. Check if lifting works
    val a = Range(2, 10)
    
    // Make sure Range can extract constants!
    val b = unit(3) + 5
    val c = Range(b, 20)
  }
}

object EnumTestRunner extends DeliteTestRunner with OptiSDRApplicationRunner with EnumTest
trait EnumTest extends DeliteTestModule with OptiSDRApplication {
  def main() = {
    // Check if lifting works
    val a = ParamEnum(1, 2, 3)

    // Make sure Range can extract constants!
    val b = unit(3) + 5
    val c = ParamEnum(b, 20)
  }
}

object BelongsToTestRunner extends DeliteTestRunner with OptiSDRApplicationRunner with BelongsToTest
trait BelongsToTest extends DeliteTestModule with OptiSDRApplication {
  def main() = {
    // Make Enums and params
    val a = ParamEnum(1, 2, 3)

    val b = Range(2, 5)
    
    // Make some kind of variable
    val c = unit(1)
    
    // Make sure belongs to works
    belongsto(c, a)
    belongsto(c, b)
  }
}

class ParamSuite extends DeliteSuite {
  def testRange() { compileAndTest(RangeTestRunner) }
}

