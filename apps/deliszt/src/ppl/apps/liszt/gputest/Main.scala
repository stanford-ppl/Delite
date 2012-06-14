package ppl.apps.liszt.gputest

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object HelloGPURunner extends DeLisztApplicationRunner with HelloGPU

trait HelloGPU extends DeLisztApplication {
    def main() {
      val field = FieldWithConst[Cell,Double](0.0)
      val ffield = FieldWithConst[Face,Double](0.0)
      
      for(c <- cells(mesh)) {
        field(c) += 3
      }
      for(c <- cells(mesh)) {
        Print(field(c))
      }
      /*
      for(f <- faces(mesh)) {
        val c = if(ID(f) == 4) inside(f) else outside(f)
        //val v = if(ID(c) == 3) vertex(c,0) else vertex(c,1)
        field(c) = 3
        //Print(v)
        /*for (ce <- cells(f)) {
          field(ce) += 10
          ffield(f) += 10
        } */
      }
      */
    }
}
