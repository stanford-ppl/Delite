package ppl.dsl.optiml.apps.gpuKernel

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object GPUMatMult extends DeliteApplication with OptiMLExp {

  def main() = {

    val x = Matrix[Double](10,10)
    val y = Matrix[Double](10,10)

	for(i <- 0 until x.numRows) {
		for(j <- 0 until x.numCols) {
				x(i,j) = 1.0
				y(i,j) = 2.0
		}
	}

	println("x is "); x.pprint;
	println("y is "); y.pprint;
    val z = x * y
    println("z is "); z.pprint;
  }
}
