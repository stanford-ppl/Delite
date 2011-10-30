package ppl.apps.liszt.gputest

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object NestedVecTestRunner extends DeLisztApplicationRunner with NestedVecTest

trait NestedVecTest extends DeLisztApplication {
    var int3_init : Rep[Vec[_3,Int]] = null
    var cell_centroid : Rep[Field[Cell,Vec[_3,Int]]] = null
    var face_values : Rep[Field[Face,Int]] = null
    var interior_set : Rep[BoundarySet[Face]] = null
    
    def main() {
      int3_init = Vec(1,2,3)
      cell_centroid = FieldWithConst[Cell,Vec[_3,Int]](int3_init)
      face_values = FieldWithConst[Face,Int](0)
      interior_set = BoundarySet[Face]("default-interior")
      val globalVelocity = Vec(1,0,0)
      
      for(f <- interior_set) {
        val localVel = Vec(4,1,2)
        val resVel = localVel + globalVelocity
        face_values(f) = ID(f) + resVel(0)
      }
      for(f <- interior_set) {
        Print(face_values(f))
      }
    }
}
