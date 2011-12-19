package ppl.apps.tests

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml._

object GPUWhileLoopRunner extends OptiMLApplicationRunner with GPUWhileLoop

trait GPUWhileLoop extends OptiMLApplication {

  def main() = {
	  var i = 0
      while ( i < 3) {
	  	val a = Matrix.onesf(5,10).mutable
		a(i,i) = 3.4f
	  	val b = Vector.onesf(10).mutable
		b(i) = 1.2f
	  	val c = a * b
	  	c.pprint
		i += 1
	  }
  }

}

