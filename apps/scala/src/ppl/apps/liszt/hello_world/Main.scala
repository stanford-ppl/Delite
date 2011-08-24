package ppl.apps.liszt.hello_world

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication}

object HelloWorldRunner extends DeLisztApplicationRunner with HelloWorld

trait HelloWorld extends DeLisztApplication {
    var field : Rep[Field[Cell,Int]] = null
    var ffield : Rep[Field[Face,Int]] = null

    def main() {
      field = FieldWithConst[Cell,Int](0)
      ffield = FieldWithConst[Face,Int](0)
      
      for(c <- cells(mesh)) {
        field(c) += 1
      }
      for(f <- faces(mesh)) {
        val c = if(ID(f) == 4) inside(f) else outside(f)
        val v = if(ID(c) == 3) vertex(c,0) else vertex(c,1)
        Print(v)
        for (ce <- cells(f)) {
          field(ce) += 10
          ffield(f) += 10
        }
      }
    }
}
