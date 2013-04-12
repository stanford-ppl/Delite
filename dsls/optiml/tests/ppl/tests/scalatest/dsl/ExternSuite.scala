package ppl.tests.scalatest.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication}
import ppl.tests.scalatest._

/* 
 *  Unit tests for external ops
 */

object MatMultRunner extends DeliteTestRunner with OptiMLApplicationRunner with MatMult
trait MatMult extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val x = Matrix.rand(10,20)
    val y = Matrix.rand(20,10)
    val res = x * y

    var i = 0
    while(i < x.numRows) {
      var j = 0
      while(j < y.numCols) {
        val elem = (x(i) * y.getCol(j)).sum
        collect(abs(elem-res(i,j)) < 0.000001)
        j += 1
      }
      i += 1
    }

    mkReport
  }
}

object MatMultVRunner extends DeliteTestRunner with OptiMLApplicationRunner with MatMultV
trait MatMultV extends DeliteTestModule with OptiMLApplication {
  def main() = {

    val x = Matrix.rand(10,20)
    val y = Vector.rand(20).t
    val res = x * y

    var i = 0
    while(i < x.numRows) {
      val elem = (x(i) * y).sum
      collect(abs(elem-res(i)) < 0.000001)
      i += 1
    }

    mkReport
  }
}

class ExternSuite extends DeliteSuite {
  def testMatMult() { compileAndTest(MatMultRunner) }
  def testMatMultV() { compileAndTest(MatMultVRunner) }
}

