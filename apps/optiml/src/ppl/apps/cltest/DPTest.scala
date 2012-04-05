package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object DPTestRunner extends OptiMLApplicationRunner with DPTest

trait DPTest extends OptiMLApplication {

  def main() = {

    tic()

    val vec1 = Vector.zerosf(10).mutable  // Create a Float vector with size 10 (initialized to all 0's)

    vec1(4) = 3.4f                // Update an element of the vector

    val vec2 = vec1.filter(e => e > 3.0f )        // Add 1.0f to all the elements of the vector (data-parallel)

    vec2.pprint                   // Print the result vector

    toc(vec2)
  }
}
