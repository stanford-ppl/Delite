package ppl.apps.liszt.gputest

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object NestedVecTestRunner extends DeLisztApplicationRunner with NestedVecTest

trait NestedVecTest extends DeLisztApplication {
    var int3_init : Rep[Vec[_3,Double]] = null
    var cell_centroid : Rep[Field[Cell,Vec[_3,Double]]] = null
    var face_values : Rep[Field[Face,Double]] = null
    var interior_set : Rep[BoundarySet[Face]] = null
    
    def main() {
      int3_init = Vec(1.0,2.0,3.0)
      cell_centroid = FieldWithConst[Cell,Vec[_3,Double]](int3_init)
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
