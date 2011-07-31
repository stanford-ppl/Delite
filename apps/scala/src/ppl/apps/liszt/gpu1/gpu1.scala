package ppl.apps.liszt.gpu1

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object Gpu1Runner extends DeLisztApplicationRunner with Gpu1

object SC extends DeLisztApplication {
	var x = 0
	val interior_set = BoundarySet[Face]("default-interior")
	
	def main() {
		//val start_time = cpu_time();
		//Print("Wall time:")
		//Print(start_time)
		var moo = 0
		for (f <- interior_set) {
			for (c <- cells(f)) {
				moo += 1
			}
			x += 1
		}
		moo += 1
		x += 1
		Print(moo)
		Print(x)
		Print(wall_time());
		//Print(wall_time());
		//Print(wall_time() - start_time);
		// for(c <- cells(mesh)) {
		// 	var zero = 0
		// 	for(c2 <- cells(c)) {
		// 		if(ID(c2) == 0)
		// 			zero = 1
		// 	}
		// 	if(zero == 0)
		// 	Print(zero," ",Phi(c))
		// }
		// Print("Hello there suckers")
	}	
}
