package ppl.tests.scalatest

import ppl.delite.framework.datastructures._

object HW1_O extends DeliteTestDSLApplicationRunner with HW1

trait HW1 extends DeliteTestDSLApplication {
  def main() = {
    val a = args(0).toInt
    val b = args(1).toInt
    val c = delite_int_plus(a, b);

//    val a = DeliteArray.fromFunction(100){ i => i.toDouble }
//    val res = testFunction(a, 1, 2.0)
    println(c)
  }

  def testFunction(a: Rep[DeliteArray[Double]], b: Rep[Int], c: Rep[Double]) = {
    val res = a.map(_ + b).reduce(_+_, 0.0)
    res / c
  }

}
