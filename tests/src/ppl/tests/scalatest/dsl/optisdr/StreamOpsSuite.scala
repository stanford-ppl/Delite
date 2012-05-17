package ppl.tests.scalatest.dsl.optisdr

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optisdr.{OptiSDRApplicationRunner, OptiSDRApplication, Stream} // Need the Stream import otherwise we get Scala's stream
import ppl.tests.scalatest._

/* Testing OptiSDR stream operations functionality
 *
 * author:  Michael Wu (michaelmwu@stanford.edu)
 * created: 3/04/12
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */
 
object StreamOpsTestRunner extends DeliteTestRunner with OptiSDRApplicationRunner with StreamOpsApp
trait StreamOpsApp extends DeliteTestModule with OptiSDRApplication {
  def main() = {
    val a = FakeStreamVector(3, 2, 4)
    val b = FakeStreamVector(1, 2, 3)
    
    val c = a - b
    
    collect(c(0) == 2)
    collect(c(1) == 0)
    collect(c(2) == 1)
    
    val d = a * b
    
    collect(d(0) == 3)
    collect(d(1) == 4)
    collect(d(2) == 12)
    
    val e = a + b
    
    collect(e(0) == 4)
    collect(e(1) == 4)
    collect(e(2) == 7)
    
    val f = a & b
    
    collect(f(0) == 1)
    collect(f(1) == 2)
    collect(f(2) == 0)
    
    val g = a | b
    
    collect(g(0) == 3)
    collect(g(1) == 2)
    collect(g(2) == 7)
    
    val h = a ^ b
    
    collect(h(0) == 2)
    collect(h(1) == 0)
    collect(h(2) == 7)
    
    mkReport
  }
}

class StreamOpsSuite extends DeliteSuite {
  def testStreamOps() { compileAndTest(StreamOpsTestRunner) }
}

