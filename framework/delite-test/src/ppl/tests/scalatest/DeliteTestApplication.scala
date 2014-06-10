package ppl.tests.scalatest

import ppl.delite.framework.datastructures._
import scala.virtualization.lms.common.Record

//TODO: move this into test suite (need to be able to provide sample inputs)
object DeliteTestMainRunner extends DeliteTestDSLApplicationRunner with DeliteTestFunction
object DeliteTestFunctionRunner extends DeliteTestDSLApplicationRunner with DeliteTestFunction {
  override def functionName = "DeliteTestFunction"
  registerFunction(testFunction _) 
}

trait DeliteTestFunction extends DeliteTestDSLApplication {
  def main() = {
    val a = DeliteArray.fromFunction(100){ i => i.toDouble }
    val res = testFunction(a, 1, 2.0)
    println(res)
  }

  def testFunction(a: Rep[DeliteArray[Double]], b: Rep[Int], c: Rep[Double]) = {
    val res = a.map(_ + b).reduce(_+_, 0.0)
    res / c
  }

}
