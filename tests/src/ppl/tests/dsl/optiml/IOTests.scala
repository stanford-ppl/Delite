package ppl.tests.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.OptiMLExp

/* Description
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 12/24/10
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

object IOTests extends DeliteApplication with OptiMLExp {

  def testWrite() = {
    val x = Matrix.ones(10,10)
    x.pprint
    MLOutputWriter.write(x, "test.mat")
  }

  def main() = {
    testWrite()
  }
}