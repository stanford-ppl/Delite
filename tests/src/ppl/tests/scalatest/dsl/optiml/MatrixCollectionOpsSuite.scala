package ppl.tests.scalatest.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication}
import ppl.tests.scalatest._

object MapRowsRunner extends DeliteTestRunner with OptiMLApplicationRunner with MapRows
trait MapRows extends DeliteTestModule with OptiMLApplication {
  def main() = {

		val x = Matrix.zeros(10,10)
		val y = x mapRows { i => Vector.ones(10) }
		collect (y == Matrix.ones(10,10))
    mkReport
  }
}

object ReduceRowsRunner extends DeliteTestRunner with OptiMLApplicationRunner with ReduceRows
trait ReduceRows extends DeliteTestModule with OptiMLApplication {
  def main() = {

		val x = Matrix.ones(10,10)
		val y = x reduceRows { (r1,r2) => r1 + r2 }
		collect(y == Vector(10.,10.,10.,10.,10.,10.,10.,10.,10.,10.))		
    mkReport
  }
}

class MatrixCollectionOpsSuite extends DeliteSuite {
  def testMapRows() { compileAndTest(MapRowsRunner) }
	def testReduceRows() { compileAndTest(ReduceRowsRunner) }
}

