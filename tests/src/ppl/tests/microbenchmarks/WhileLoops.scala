package ppl.tests.microbenchmarks

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object WhileLoopsBenchmarkRunner extends OptiMLApplicationRunner with WhileLoopsBenchmark

trait WhileLoopsBenchmark extends OptiMLApplication {
	
	def testSequentialLoop() {
		val v = Vector[Double](10000, true).mutable
		var i = 0
		// 10 sequential operations, no parallel operations
		while (i < v.length) {
			val x = 1.0
			val y = v.median
			if (v.contains(99)) {
				v += 42
			}
			v(100) = v.sort.apply(100) // this is unfortunate
			v(i) = x + y + random[Double]
			i += 1
		}
	}
	
	def testFineLoop() {
		val v = Vector[Double](100, true)
		var i = 0
		while (i < 100000) {
			val x = (v + 5 * 10 / 2 - 72 ).mutable
			x(5) = random[Double]
			i += 1
		}
	}
	
	def testCoarseLoop() {
		val v = Vector[Double](100000, true)
		var i = 0
		while (i < 10000) {
			val x = (v + 5 * 10 / 2 - 72).mutable 
			x(5) = random[Double]
			i += 1
		}		
	}
	
	def main() = {
		// println(" === sequential while")
		// tic()
		// testSequentialLoop()
		// toc()
		// println(" === fine while")
		// tic()
		// testFineLoop()
		// toc()		
		println(" === coarse while")
		tic()
		testCoarseLoop()
		toc()		
	}
}
