package ppl.apps.liszt.gputest

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object VecCopyTestRunner extends DeLisztApplicationRunner with VecCopyTest

trait VecCopyTest extends DeLisztApplication {
    var face_values : Rep[Field[Face,Double]] = null
    var interior_set : Rep[BoundarySet[Face]] = null
    
    def main() {
      face_values = FieldWithConst[Face,Double](0.0)
      interior_set = BoundarySet[Face]("default-interior")
      val globalVelocity = Vec(1.0,0.0,0.0)
      
      for(f <- interior_set) {
        val localVel = Vec(4.0,1.0,2.0)
        val resVel = localVel + globalVelocity
        face_values(f) = resVel(0)
      }
      for(f <- interior_set) {
        Print(face_values(f))
      }
    }
}
