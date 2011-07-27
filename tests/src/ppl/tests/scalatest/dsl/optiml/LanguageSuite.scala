package ppl.tests.scalatest.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication}
import ppl.tests.scalatest._

object SumRunner extends DeliteTestRunner with OptiMLApplicationRunner with Sum
trait Sum extends DeliteTestModule with OptiMLApplication {
  def main() = {
    implicit val collector = ArrayBuffer[Boolean]()

		val x = sum(0, 10) { i =>
			i*2
		}
		collect(x == 90)
    mkReport
  }
}

object SumIfRunner extends DeliteTestRunner with OptiMLApplicationRunner with SumIf
trait SumIf extends DeliteTestModule with OptiMLApplication {
  def main() = {
    implicit val collector = ArrayBuffer[Boolean]()

    val y = Vector(true, false, true, false, true, false, false, false, true, true)
    val x = sumIf(0,10) { y(_) } { i => Vector.ones(5) }
    //x.pprint
		collect(x == Vector(5.0, 5.0, 5.0, 5.0, 5.0))
    mkReport
  }
}

object IndexVectorConstructRunner extends DeliteTestRunner with OptiMLApplicationRunner with IndexVectorConstruct
trait IndexVectorConstruct extends DeliteTestModule with OptiMLApplication {
  def main() = {
    implicit val collector = ArrayBuffer[Boolean]()

		val x = (0::10) { i =>
			i*2
		}
		collect(x == Vector(0,2,4,6,8,10,12,14,16,18))
    mkReport
  }
}

object IndexVectorConstruct2Runner extends DeliteTestRunner with OptiMLApplicationRunner with IndexVectorConstruct2
trait IndexVectorConstruct2 extends DeliteTestModule with OptiMLApplication {
  def main() = {
    implicit val collector = ArrayBuffer[Boolean]()

		val x = (0::10, *) { i =>
			Vector.ones(2) * i
		}
		collect(x == Matrix(
								 	 Vector(0.,0.),
									 Vector(1.,1.),
									 Vector(2.,2.),
									 Vector(3.,3.),
									 Vector(4.,4.),
									 Vector(5.,5.),
									 Vector(6.,6.),
									 Vector(7.,7.),
									 Vector(8.,8.),
									 Vector(9.,9.))
								 )
												
		val y = (0::2, 0::3) { (i,j) =>
			i*j
		}
		collect(y == Matrix(Vector(0,0,0),
												Vector(0,1,2)))
    mkReport
  }
}


class LanguageSuite extends DeliteSuite {
  def testSum() { compileAndTest(SumRunner) }
  def testSumIf() { compileAndTest(SumIfRunner) }
  def testIndexVector() { compileAndTest(IndexVectorConstructRunner) }
  def testIndexVector2() { compileAndTest(IndexVectorConstruct2Runner) }
}

