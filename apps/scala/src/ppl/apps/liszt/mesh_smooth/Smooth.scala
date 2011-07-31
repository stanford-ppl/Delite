package ppl.apps.liszt.mesh_smooth

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object MeshSmoothRunner extends DeLisztApplicationRunner with Smooth

object Smooth extends DeLisztApplication {
	val position = FieldWithLabel[Vertex,Vec[_3,Float]]("position")
	val smooth_pos = FieldWithConst[Vertex,Vec[_3,Float]]( Vec(0.f,0.f,0.f) )
	def main() {
		var a = 0
		while(a < 10) {
			for(v <- vertices(mesh)) {
				var pos = position(v)
				val n = size(vertices(v))
				for(v2 <- vertices(v)) {
					pos += position(v2)
				}
				smooth_pos(v) = pos / (n + 1)
			}
			for(v <- vertices(mesh))
				position(v) = smooth_pos(v)
			a += 1
		}
	}
}