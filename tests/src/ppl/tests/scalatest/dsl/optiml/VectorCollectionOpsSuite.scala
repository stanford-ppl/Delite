package ppl.tests.scalatest.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication}
import ppl.tests.scalatest._

object SimpleFlatMapRunner extends DeliteTestRunner with OptiMLApplicationRunner with SimpleFlatMap
trait SimpleFlatMap extends DeliteTestModule with OptiMLApplication {
  def main() = {

		val x = Vector(1,2,3,4,5)
		val y = x flatMap { e => Vector.zeros(e) }
		collect(y.length == x.sum)		
    mkReport
  }
}

object SimpleFlattenRunner extends DeliteTestRunner with OptiMLApplicationRunner with SimpleFlatten
trait SimpleFlatten extends DeliteTestModule with OptiMLApplication {
  def main() = {

		val x = Vector(Vector(1,2,3,4,5), Vector(6,7,8,9))
		val y = DenseVector.flatten(x)
		collect(y == Vector(1,2,3,4,5,6,7,8,9))		
    mkReport
  }
}


class VectorCollectionOpsSuite extends DeliteSuite {
  def testFlatMap() { compileAndTest(SimpleFlatMapRunner) }
	def testFlatten() { compileAndTest(SimpleFlattenRunner) }
}

