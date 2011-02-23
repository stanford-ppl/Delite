package ppl.tests.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication}

/* Testing OptiML i/o functionality
*
* author:  Arvind Sujeeth (asujeeth@stanford.edu)
* created: 12/24/10
*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*
*/

object IOTestsRunner extends OptiMLApplicationRunner with IOTests

trait IOTests extends OptiMLApplication {

  def testWrite() = {
    val x = Matrix.ones(10,10)
    x.pprint
    MLOutputWriter.write(x, "test.mat")
  }

  def main() = {
    testWrite()
  }
}