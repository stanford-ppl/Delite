package ppl.tests.scalatest

import ppl.delite.framework.datastructures._

object HW1_O extends DeliteHWDSLApplicationRunner with HW1

trait HW1 extends DeliteHWDSLApplication {
  def main() = {
    val a = args(0).toInt
    val b = args(1).toInt
    val c = a + b;


    println(c)
  }

  def testFunction(a: Rep[DeliteArray[Double]], b: Rep[Int], c: Rep[Double]) = {
    val res = a.map(_ + b).reduce(_+_, 0.0)
    res / c
  }

}
