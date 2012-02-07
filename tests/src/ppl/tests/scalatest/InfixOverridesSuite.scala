package ppl.tests.scalatest

import ppl.dsl.optiml.{Vector,RangeVector}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}

object InfixOverridesRunner extends DeliteTestRunner with OptiMLApplicationRunner with InfixOverrides
trait InfixOverrides extends DeliteTestModule with OptiMLApplication {
  def main() {

    val a = unit(1.0)

    // toString
    /* THIS DOESN'T WORK */
    //collect(a.toString == "1.0")
    /* THIS DOES */
    //collect(infix_toString(a) == "1.0")
    /* THIS DOES TOO */
    collect(a.ToString == "1.0")

    // clone
    //val b = a.clone
    //collect(a == b)

    // isInstanceOf/asInstanceOf
    //collect(v)

    mkReport
  }
}

class InfixOverridesSuite extends DeliteSuite {
  def testInfixOverrides() { compileAndTest(InfixOverridesRunner) }
}