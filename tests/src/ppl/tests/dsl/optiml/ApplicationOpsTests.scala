package ppl.tests.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{ApplicationOps, OptiMLExp}

/* Description
*
* author:  Arvind Sujeeth (asujeeth@stanford.edu)
* created: 12/24/10
*
* Pervasive Parallelism Laboratory (PPL)
* Stanford University
*
*/

object ApplicationOpsTests extends DeliteApplication with OptiMLExp {

  def main() = {
    val x = Rect(1,2,3,4)
  }
}