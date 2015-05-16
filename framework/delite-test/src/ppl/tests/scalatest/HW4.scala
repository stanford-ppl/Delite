package ppl.tests.scalatest

import ppl.delite.framework.datastructures._

object HW4_O extends DeliteHWDSLApplicationRunner with HW4

trait HW4 extends DeliteHWDSLApplication {
  def main() = {
    val a = DeliteArray.fromFunction(100){ i => i * i + 1 }
    val b = a.reduce((x, y) => x*y, 0)
    println(b)
  }
}
