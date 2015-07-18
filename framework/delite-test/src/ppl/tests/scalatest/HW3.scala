package ppl.tests.scalatest

import ppl.delite.framework.datastructures._

object HW3_O extends DeliteHWDSLApplicationRunner with HW3

trait HW3 extends DeliteHWDSLApplication {
  def main() = {
//    val a = DeliteArray.fromFunction(100){ i => i.toDouble }
    val a = DeliteArray.fromFunction(100){ i => i }
    val res = testFunction(a, 1, 2)
    println(res)
  }

  def testFunction(a: Rep[DeliteArray[Int]], b: Rep[Int], c: Rep[Int]) = {
    val res = a.map(_ + b).reduce(_+_, 0)

    /*
     * for (i <- 0 to a.size) {
     *   tmp[i] = a[i] + b
     * }
     */

    res
  }

}
