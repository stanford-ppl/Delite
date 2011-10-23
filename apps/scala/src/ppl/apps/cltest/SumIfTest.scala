package ppl.apps.cltest

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object SumIfTestRunner extends OptiMLApplicationRunner with SumIfTest

trait SumIfTest extends OptiMLApplication {

  def main() = {

    /*
    val m = 10
    val y = Vector.onesf(m).mutable
    y(3) = 3.4f
    //val x = Matrix.onesf(m,7)
    val x = Vector.onesf(7)
    val out = sumIf[Vector[Double]](0,m) {y(_)==1.0f} {x+=Vector.}
    println(out)
    */
  }
}
