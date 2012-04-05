package ppl.apps.tests

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml._

object MutableOpsRunner extends OptiMLApplicationRunner with MutableOps

trait MutableOps extends OptiMLApplication {

  def main() = {
	  val acc = Vector.zerosf(10).mutable
	  val in = Matrix.onesf(5,10).mutable

	  var i = 0
      while ( i < 5) {
	  	acc += in(i) * i
		i += 1
	  }
	  acc.pprint
  }


}

