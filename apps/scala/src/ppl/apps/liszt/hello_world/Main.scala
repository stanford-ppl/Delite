package ppl.apps.liszt.hellow_world

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication, DeLisztExp}

object HelloWorldRunner extends DeLisztApplicationRunner with Main

object Main extends DeLisztApplication {
    val field = FieldWithConst[Cell,Int](0)
    def main() {
		for(f <- faces(mesh)) {
			val c = if(ID(f) == 4) inside(f) else outside(f)
			val v = if(ID(c) == 3) vertex(c,0) else vertex(c,1)
			Print(v)
		}
    }
}