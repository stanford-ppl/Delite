package ppl.tests.microbenchmarks

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object MultiLoopsBenchmarkRunner extends OptiMLApplicationRunner with MultiLoopsBenchmark

trait MultiLoopsBenchmark extends OptiMLApplication {
	
	def testSmallOp() {
		val v = Vector[Double](100, true)
		var y = v map { i => pow(exp(i), 2) }		
	}
	
	def testLargeOp() {
		val v = Vector[Double](1000000, true)
		var y = v map { i => pow(exp(i), 2) }		
	}
	
	def main() = {
		// println(" === small map")
		// tic()
		// testSmallOp()
		// toc()
		println(" === large while")
		tic()
		testLargeOp()
		toc()		
	}
}
