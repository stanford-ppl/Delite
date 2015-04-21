package ppl.tests.scalatest

import ppl.delite.framework.datastructures._

object HW2_O extends DeliteTestDSLApplicationRunner with HW2

trait HW2 extends DeliteTestDSLApplication {
  def main() = {
    val a = DeliteArray.fromFunction(100){ i => i + 1 }
    println(a(0))
  }
}
