package ppl.apps.tests

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml._

object AnyTypeRunner extends OptiMLApplicationRunner with AnyType

trait AnyType extends OptiMLApplication {

  def main() = {
	  var i = 0
      while ( i < 5) {
	  	println("ITER: " + i)
		i += 1
	  }
  }


}

