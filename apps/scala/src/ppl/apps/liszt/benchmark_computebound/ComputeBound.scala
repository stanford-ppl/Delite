package ppl.apps.liszt.benchmark_computebound

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object ComputeBoundRunner extends DeLisztApplicationRunner with ComputeBound

object ComputeBound extends DeLisztApplication {
	val position = FieldWithLabel[Vertex,Vec[_3,Float]]("position")
	val outc = FieldWithConst[Cell,Float](0)
	
	def main() {
		var t = 0.f
		val deltat = 0.2f

		Print("Running 2-level sequential pattern.")
		while (t<20.f) {
			for (c <- cells(mesh)) {
				var avg = 0.f
				for (v <- vertices(c)) {
					var e = 0
					val p = position(v)
					while (e < 2000) {
						avg *= p(_0)
						e += 1
					}
				}
				outc(c) = avg
			}
			t += deltat
		}
		
		
	}
}