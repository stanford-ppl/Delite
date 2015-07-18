package ppl.tests.scalatest

import ppl.delite.framework.datastructures._

object HW5_O extends DeliteHWDSLApplicationRunner with HW5

/*
 * Example containing fusable loops without a producer-consumer relationship
 */
trait HW5 extends DeliteHWDSLApplication {
  def main() = {
    val arr1 = DeliteArray.fromFunction(100){ i => i + 1 }
    val rval = arr1.reduce((x,y) => x+y, 0)
    val arr2 = arr1.map(x => x*rval)
//    val arr3 = arr2.zip(arr2)((a,b) => a+b)
    println(arr2(0))
  }
}
