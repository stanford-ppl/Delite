package ppl.tests.scalatest

import ppl.delite.framework.datastructures._

object HW2_O extends DeliteHWDSLApplicationRunner with HW2

trait HW2 extends DeliteHWDSLApplication {
  def main() = {
//    val a = DeliteArray.fromFunction(100){ i => i + 1 }
    val a = DeliteArray[Int](100)
    a.map(x => x)
//    println(a(0))
  }
}
