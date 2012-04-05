package ppl.tests.misc

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication}
import ppl.dsl.optiml.application.ApplicationOps

/* Testing application-defined data structures
*
* author:  Arvind Sujeeth (asujeeth@stanford.edu)
* created: 12/24/10
*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*
*/

object ApplicationOpsTestsRunner extends OptiMLApplicationRunner with ApplicationOpsTests

trait ApplicationOpsTests extends OptiMLApplication {

  def main() = {
    val x = Rect(1,2,3,4)
  }
}