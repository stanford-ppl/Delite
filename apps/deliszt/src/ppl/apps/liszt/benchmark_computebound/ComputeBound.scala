package ppl.apps.liszt.benchmark_computebound

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object ComputeBoundRunner extends DeLisztApplicationRunner with ComputeBound

trait ComputeBound extends DeLisztApplication {
	def main() {
    val position = FieldWithLabel[Vertex,Vec[_3,Float]]("position")
    val outc = FieldWithConst[Cell,Float](0f)
  
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
						avg = avg * p(_0)
						e += 1
					}
				}
				outc(c) = avg
			}
			t += deltat
		}
		
		
	}
}