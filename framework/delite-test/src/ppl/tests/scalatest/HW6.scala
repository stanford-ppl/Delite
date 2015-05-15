package ppl.tests.scalatest

import ppl.delite.framework.datastructures._

object HW6_O extends DeliteTestDSLApplicationRunner with HW6

trait HW6 extends DeliteTestDSLApplication {
  def main() = {
    val a = DeliteArray.fromFunction(100){ i => DeliteArray.fromFunction(50) {j => i + 1} }
    val b = a.reduce((row1, row2) => row1.zip(row2)((x,y) => x*y), DeliteArray.fromFunction(50) { i => 0 })
    println(b)
  }
}
