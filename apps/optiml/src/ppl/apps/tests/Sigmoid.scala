package ppl.apps.tests

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml._
import ppl.delite.framework.datastructures._

object SigmoidOpRunner extends OptiMLApplicationRunner with SigmoidOp

trait SigmoidOp extends OptiMLApplication {

  def main() = {
    val v1 = DeliteArrayBuffer.fromFunction(10)(i => 1.0f)
    val out = if (v1(0) == 1.0f) {
      val v2 = v1.zip(DeliteArrayBuffer.fromFunction(10)(i =>1.0f)){ _ + _ }
      v2(0)
    } else {
      v1(0)
    }

    println(out == 2.0f)

  }


}

