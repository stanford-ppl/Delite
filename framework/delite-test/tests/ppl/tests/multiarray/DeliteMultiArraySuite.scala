package ppl.tests.multiarray

import ppl.tests.scalatest._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.codegen.delite.MultiArrayGenException

trait DeliteMultiArrayTestbenchRunner extends DeliteTestRunner with DeliteTestDSLApplicationRunner 
  with DeliteMultiArrayOpsExp

trait DeliteMultiArrayTestbench extends DeliteTestModule with DeliteTestDSLApplication 
  with DeliteMultiArrayOps

object SinglyNestedMultiArrayRunner extends DeliteMultiArrayTestbenchRunner with SinglyNestedMultiArray
trait SinglyNestedMultiArray extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = DeliteArray1D.fromFunction(10){i => DeliteArray1D[Float](3)}
    vec foreach {k => collect(k.reduce(0.0f){(a,b) => a + b} == 0.0f)}

    mkReport
  }
}

object NullMultiArrayRunner extends DeliteMultiArrayTestbenchRunner with NullMultiArray 
trait NullMultiArray extends DeliteMultiArrayTestbench {
  def main() = {
    // Can infer DeliteArray1D[DeliteArray1D[??]] 
    // type erasure hides nested types
    val vec = DeliteArray1D[DeliteArray1D[Int]](3)
    val str = vec.mkString(",")

    collect(str == "null,null,null")

    mkReport
  }
}

object MultiArrayUpdateRunner extends DeliteMultiArrayTestbenchRunner with MultiArrayUpdate 
trait MultiArrayUpdate extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = DeliteArray1D[DeliteMultiArray[Int]](3)
    vec forIndices {i => vec(i) = DeliteArray1D.fromFunction(3){j => j}}

    collect(vec(0).reduce(0){_+_} == 3)

    mkReport
  }
}

object IllegalMultiArrayUpdateRunner extends DeliteMultiArrayTestbenchRunner with IllegalMultiArrayUpdate 
trait IllegalMultiArrayUpdate extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = DeliteArray1D[DeliteMultiArray[Int]](3)
    vec forIndices {i => vec(i) = DeliteArray1D.fromFunction(3){j => j}}

    // Disallowed - all nested arrays must have same dimensionality
    vec(1) = DeliteArray2D.fromFunction(2, 2){(i,j) => i + j}

    collect(vec(0).reduce(0){_+_} == 3)

    mkReport
  }
}

object IllegalMutationRunner extends DeliteMultiArrayTestbenchRunner with IllegalMutation
trait IllegalMutation extends DeliteMultiArrayTestbench {
  def main() = {
    val vec = DeliteArray2D.fromFunction(2, 2){(i,j) => i + j}
    
    // Not allowed - vec is immutable
    vec(0,0) = 10

    collect(vec(0,0) == 0)

    mkReport
  }
}

class DeliteMultiArraySuite extends DeliteSuite {

  override def compileAndTest(app: DeliteTestRunner, checkMultiLoop: Boolean = false) {
    super.compileAndTest(app, checkMultiLoop)
    intercept[MultiArrayGenException] {0}
  }

  def testSinglyNestedMultiArray() { compileAndTest(SinglyNestedMultiArrayRunner) }
  def testNullMultiArray() { compileAndTest(NullMultiArrayRunner) }
  def testMultiArrayUpdate() { compileAndTest(MultiArrayUpdateRunner) }
  def testIllegalMultiArrayUpdate() { compileAndTest(IllegalMultiArrayUpdateRunner) }
  def testIllegalMutation() { compileAndTest(IllegalMutationRunner) }
} 
