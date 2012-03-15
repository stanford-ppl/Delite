package ppl.tests.scalatest.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication}
import ppl.tests.scalatest._

/* Testing OptiML i/o functionality
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 12/24/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

object SimpleWriteReadRunner extends DeliteTestRunner with OptiMLApplicationRunner with SimpleWriteRead
trait SimpleWriteRead extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val x = Matrix.ones(10,10)
    writeMatrix(x, "test.mat")

    val y = readMatrix("test.mat")
    collect(y == x)

    mkReport
  }
}

class IOSuite extends DeliteSuite {
  def testSimpleWriteRead() { compileAndTest(SimpleWriteReadRunner) }
}

