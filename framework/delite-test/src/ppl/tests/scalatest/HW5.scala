package ppl.tests.scalatest

import ppl.delite.framework.datastructures._

object HW5_O extends DeliteTestDSLApplicationRunner with HW5

/*
 * Example containing fusable loops without a producer-consumer relationship
 */
trait HW5 extends DeliteTestDSLApplication {
  def main() = {
    val a = DeliteArray.fromFunction(100){ i => i + 1 }
    val b = a.map(x => x*x)
    val c = a.map(x => x+2)
    println(b(0)+c(0))
  }
}
