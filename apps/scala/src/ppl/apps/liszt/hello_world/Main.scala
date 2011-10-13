package ppl.apps.liszt.hello_world

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.datastruct.scala.MetaInteger._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication}

object HelloWorldRunner extends DeLisztApplicationRunner with HelloWorld

trait HelloWorld extends DeLisztApplication {
    def main() {
      val field = FieldWithConst[Cell,Int](0)
      val ffield = FieldWithConst[Face,Int](0)
      
      for(c <- cells(mesh)) {
        field(c) += 1
      }
      for(f <- faces(mesh)) {
        val c = if(ID(f) == 4) inside(f) else outside(f)
        val v = if(ID(c) == 3) vertex(c,0) else vertex(c,1)
        Print(v)
        /*for (ce <- cells(f)) {
          field(ce) += 10
          ffield(f) += 10
        } */
      }
    }
}
