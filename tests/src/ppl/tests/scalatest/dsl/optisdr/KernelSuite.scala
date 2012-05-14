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

trait SimpleKernel {
  this: OptiSDRApplication =>
  
  val simpleKernel = kernel () => {
    def run(a: Stream[Int], b: Stream[Int]) {
      a + b
    }
  }
}
 
object SimpleKernelRunner extends DeliteTestRunner with OptiSDRApplicationRunner with SimpleKernelApp
trait SimpleKernelApp extends DeliteTestModule with OptiSDRApplication with SimpleKernel {
  def main() = {
    val a = FakeStreamVector(1, 2, 3)
    val b = FakeStreamVector(3, 2, 4)
    
    val c = {simpleKernel()}(a, b)
  }
}

class PrimitiveSuite extends DeliteSuite {
  def testSimpleKernel() { compileAndTest(SimpleKernelTestRunner) }
}

