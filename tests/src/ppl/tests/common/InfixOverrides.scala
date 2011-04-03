package ppl.tests.common

import ppl.dsl.optiml.datastruct.scala.{Vector,RangeVector}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}
import ppl.tests.dsl.optiml.{OptiMLSuite, OptiMLTestModule}

object InfixOverridesRunner extends OptiMLApplicationRunner with InfixOverrides
trait InfixOverrides extends OptiMLTestModule {
  def main() {
    implicit val collector = Vector[Boolean]()

    val a = unit(1.0)

    // toString
    /* THIS DOESN'T WORK */
    //collect(a.toString == "1.0")
    /* THIS DOES */
    //collect(infix_toString(a) == "1.0")
    /* THIS DOES TOO */
    collect(a.toStringL == "1.0")

    // clone
    //val b = a.clone
    //collect(a == b)

    // isInstanceOf/asInstanceOf
    //collect(v)

    mkReport
  }
}

class InfixOverridesSuite extends OptiMLSuite {
  def testInfixOverrides() { compileAndTest(InfixOverridesRunner) }
}