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
 
object VectorTestRunner extends DeliteTestRunner with OptiSDRApplicationRunner with VectorApp
trait VectorApp extends DeliteTestModule with OptiSDRApplication {
  def main() = {
    val a = Vector(Complex(1, 1), Complex(-0.5, 0), Complex(0.2, 0.4), Complex(1, 0))
    val b = FFT_1D(a)
    println("Complex FFT")
    b.pprint
    
    val a_1 = IFFT_1D(b)
    println("Complex IFFT")
    a_1.pprint
    
    val c = Vector[Real](1.0, 0.2, -3, 0)
    val d = FFT_1D(c)
    println("Real FFT")
    d.pprint
    
    val c_1 = IFFT_1D(d)
    println("Real IFFT")
    c_1.pprint
  
    collect(true)
    
    mkReport
  }
}

class VectorSuite extends DeliteSuite {
  def testVectorOps() { compileAndTest(VectorTestRunner) }
}

