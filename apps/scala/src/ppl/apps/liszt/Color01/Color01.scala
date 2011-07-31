package ppl.apps.liszt.Color01

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object Color01Runner extends DeLisztApplicationRunner with Color01

object Color01 extends DeLisztApplication {
	
	val scattersField01 = FieldWithConst[Edge, Float](0.f)
	val scattersField02 = FieldWithConst[Vertex, Float](0.f)
	val scattersField04 = FieldWithConst[Edge, Int](0)
	
	val itersPerTest = 1000
	def printResults(numWrites : Int, numLocations : Int, totalTime : Double) {
		val scattersPerSec = numWrites / totalTime
		Print(numWrites, " writes to ", numLocations, " memory locations took ", totalTime, "s, average of ", scattersPerSec/1000000, " MScatters/sec, ", scattersPerSec/1000000*8, " MBytes/sec")		
	}

	def testColor1() { //Edge -> itself [1-coloring]
		Print("Running ", itersPerTest, " iterations of test edge -> itself (expect 1 coloring)")

		var startTime = 0.0;
		var avgTime = 0.0
		var perIterTime = 0.0;
		var i = 0
		while (i < itersPerTest+1) {
			if (i == 0) { Print("Warmup round...") }
			if (i > 0) { startTime = processor_time() }
			
			for (e <- edges(mesh)) {
				scattersField01(e) += 1.0f
			}
			
			if (i > 0) {
				val perIterTime = processor_time() - startTime
				avgTime = (perIterTime + (i-1)*avgTime)/i
			}
			i += 1
			
		}
		val numWrites = size(edges(mesh))*2
		val numLocations = size(vertices(mesh))
		printResults(numWrites, numLocations, avgTime)
		Print("========================================================================")
	}

	def testColor6() { //Edge -> 2 Vertices [6 coloring]
		Print("Running ", itersPerTest, " iterations of test edge -> vertices (expect 6 coloring)")
		var startTime = 0.0;
		var avgTime = 0.0
		var perIterTime = 0.0;
		var i = 0
		while (i < itersPerTest+1) {
			if (i == 0) { Print("Warmup round...") }
			if (i > 0) { startTime = processor_time() }
			
			for (e <- edges(mesh)) {
				scattersField02(head(e)) += 1.0f
				scattersField02(tail(e)) += 1.0f
			}
			
			if (i > 0) {
				val perIterTime = processor_time() - startTime
				avgTime = (perIterTime + (i-1)*avgTime)/i
			}
			i += 1
			
		}
		
		val numWrites = size(edges(mesh))*2
		val numLocations = size(vertices(mesh))
		printResults(numWrites, numLocations, avgTime)
		Print("========================================================================")
	}


	def testColor4() { //Face -> Edges [4 coloring]
		Print("Running ", itersPerTest, " iterations of test face -> edges (expect 4 coloring)")
		var startTime = 0.0;
		var avgTime = 0.0
		var perIterTime = 0.0;
		var i = 0
		while (i < itersPerTest+1) {
			if (i == 0) { Print("Warmup round...") }
			if (i > 0) { startTime = processor_time() }
			
			for (f <- faces(mesh)) {
				for (e <- edges(f)) {
					scattersField04(e) += 1
				}
			}
			
			if (i > 0) {
				val perIterTime = processor_time() - startTime
				avgTime = (perIterTime + (i-1)*avgTime)/i
			}
			i += 1
			
		}
		
		val numWrites = size(faces(mesh))*4
		val numLocations = size(edges(mesh))
		printResults(numWrites, numLocations, avgTime)
		Print("========================================================================")
	}

	// def testColor4() : Double = { //Face -> Edges [4 coloring]
	// 	Print("Running ", itersPerTest, " iterations of test face -> edges (expect 4 coloring)")
	// 	var startTime = 0.0;
	// 	var avgTime = 0.0
	// 	var perIterTime = 0.0;
	// 	var i = 0
	// 	while (i < itersPerTest+1) {
	// 		if (i == 0) { Print("Warmup round...") }
	// 		if (i > 0) { startTime = processor_time() }
	// 		
	// 		for (f <- faces(mesh)) {
	// 			for (e <- edges(f)) {
	// 				scattersField04(e) += 1
	// 			}
	// 		}
	// 		
	// 		if (i > 0) {
	// 			val perIterTime = processor_time() - startTime
	// 			avgTime = (perIterTime + (i-1)*avgTime)/i
	// 		}
	// 		i += 1
	// 		
	// 	}
	// 	
	// 	for (e <- edges(mesh)) {
	// 		if (ID(e) > 500000 && ID(e) < 500100) {
	// 			Print(scattersField04(e))
	// 		}
	// 	}
	// 	val numWrites = size(faces(mesh))*4
	// 	val numLocations = size(edges(mesh))
	// 	printResults(numWrites, numLocations, avgTime)
	// 	Print("========================================================================")
	// 	avgTime
	// }

	
	
		
	def main() {		
		//THIS TEST IS WRITTEN ASSUMING WE ARE RUNNING ON A CUBE GRID
		
		Print("Loaded a mesh with the following dimensions:")
		Print("  cells: ", size(cells(mesh)))
		Print("  faces: ", size(faces(mesh)))
		Print("  edges: ", size(edges(mesh)))
		Print("  verts: ", size(vertices(mesh)))
		Print("Starting tests:")
		
		var starttime = wall_time()
		testColor1()
		testColor4()
		testColor6()
		Print("Total time for all tests: ", wall_time() - starttime)
		
    }
}