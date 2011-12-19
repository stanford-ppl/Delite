package ppl.apps.tests

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml._

object SigmoidOpRunner extends OptiMLApplicationRunner with SigmoidOp

trait SigmoidOp extends OptiMLApplication {

  def main() = {
	  val in = Matrix.onesf(5,10)
	  val out = in.sigmoidf
	  out.pprint
  }


}

