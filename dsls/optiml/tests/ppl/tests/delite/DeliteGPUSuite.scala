package ppl.tests.scalatest.delite

import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication}
import ppl.tests.scalatest._

object DeliteGPUBLASMMRunner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPUBLASMM
trait DeliteGPUBLASMM extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val mat1 = Matrix.onesf(256,512)
    val mat2 = Matrix.onesf(512,128)
    val out = mat1 * mat2

    collect(out.numRows == 256)
    collect(out.numCols == 128)
    collect(out(0) == 512)

    mkReport
  }
}

object DeliteGPUBLASMVRunner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPUBLASMV
trait DeliteGPUBLASMV extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val mat1 = Matrix.onesf(512,1024)
    val vec1 = Vector.onesf(1024)
    val out = mat1 * vec1

    collect(out.length == 512)
    collect(out(0) == 1024)

    mkReport
  }
}

object DeliteGPUCondRunner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPUCond
trait DeliteGPUCond extends DeliteTestModule with OptiMLApplication {
  def main() = {
    
    val vec1 = Vector.onesf(10)

    val out = if(vec1(0) == 1.0f) {
                val vec2 = vec1 + Vector.onesf(10)
                vec2(0)
              }
              else {
                vec1(0)
              }

    collect(out == 2.0f)

    mkReport
  }
}

object DeliteGPUCondReturnRunner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPUCondReturn
trait DeliteGPUCondReturn extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val vec1 = Vector.onesf(10).mutable
    vec1(3) = 3.4f
    val vec2 = Vector.zerosf(10).mutable
    vec2(5) = 2.8f

    val out = if(vec1(0) != 1.0f) vec1 + vec1
              else vec1 - vec2

    collect(out(0) == 1.0f)
    collect(out(3) == 3.4f)
    collect(out(5) == -1.8f)

    mkReport
  }
}

object DeliteGPUMemLeakRunner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPUMemLeak
trait DeliteGPUMemLeak extends DeliteTestModule with OptiMLApplication {
  def main() = {

    var i = 0
    while(i < 1000) {
      val mat1 = Matrix.onesf(4000,4000)
      val out = mat1 + 1.0f
      collect(out(0,0) == 2.0f)
      i += 1
    }

    mkReport
  }
}

object DeliteGPUMutationRunner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPUMutation
trait DeliteGPUMutation extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val vec1 = Vector.zerosf(10).mutable  
    vec1(4) = 3.4f                
    val out = vec1.filter(e => e > 2.0f )  

    collect(out.length == 1)
    collect(out(0) == 3.4f)

    mkReport
  }
}

object DeliteGPUNestedMutationRunner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPUNestedMutation
trait DeliteGPUNestedMutation extends DeliteTestModule with OptiMLApplication {
  def main() = {

    var i = 0
    while(i < 2) {
      val out = Matrix.onesf(10,10).mutable
      out += Matrix.onesf(10,10)
      collect(out(0,0) == 2.0f)
      i += 1
    }

    mkReport
  }
}

object DeliteGPUObjectReductionRunner extends DeliteTestRunner with OptiMLApplicationRunner with DeliteGPUObjectReduction
trait DeliteGPUObjectReduction extends DeliteTestModule with OptiMLApplication {
  def main() = {
    val in = Matrix.onesf(4096,4)
    val out = ((0::in.numRows) { i => in(i).Clone }).sum

    collect(out(0) == 4096.0f)
    collect(out(1) == 4096.0f)
    collect(out(2) == 4096.0f)
    collect(out(3) == 4096.0f)
    
    mkReport
  }
}

class DeliteGPUSuite extends DeliteSuite {
  //def testDeliteGPUBLASMM() { compileAndTest(DeliteGPUBLASMMRunner); }
  //def testDeliteGPUBLASMV() { compileAndTest(DeliteGPUBLASMVRunner); }
  def testDeliteGPUCond() { compileAndTest(DeliteGPUCondRunner, CHECK_MULTILOOP); }
  def testDeliteGPUCondReturn() { compileAndTest(DeliteGPUCondReturnRunner, CHECK_MULTILOOP); }
  def testDeliteGPUMemLeak() { compileAndTest(DeliteGPUMemLeakRunner, CHECK_MULTILOOP); }
  def testDeliteGPUMutation() { compileAndTest(DeliteGPUMutationRunner, CHECK_MULTILOOP); }
  def testDeliteGPUNestedMutation() { compileAndTest(DeliteGPUNestedMutationRunner, CHECK_MULTILOOP); }
  def testDeliteGPUObjectReduction() { compileAndTest(DeliteGPUObjectReductionRunner, CHECK_MULTILOOP); }

}
