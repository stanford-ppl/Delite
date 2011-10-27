package ppl.apps.liszt.test

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object RangeLoopRunner extends DeLisztApplicationRunner with RangeLoop

trait RangeLoop extends DeLisztApplication {
  def main() {
    var velocity = Vec(1.f,0.f,0.f)

    Print("Velocity", velocity)
    
    //initialize geometry fields
    for(i <- 0 until 3) {
      velocity = velocity + Vec(0.f, 1.f, 1.f)
      Print("Velocity add", velocity)
    }
    
    Print("Velocity", velocity)
  }
}
