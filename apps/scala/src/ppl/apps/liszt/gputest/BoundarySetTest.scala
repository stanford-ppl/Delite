package ppl.apps.liszt.gputest

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.datastruct.scala.MetaInteger._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication}

object BoundarySetTestRunner extends DeLisztApplicationRunner with BoundarySetTest

trait BoundarySetTest extends DeLisztApplication {
    //var int3_init : Rep[Vec[_3,Int]] = null
    //var cell_centroid : Rep[Field[Cell,Vec[_3,Int]]] = null
    var face_values : Rep[Field[Face,Int]] = null
    var interior_set : Rep[BoundarySet[Face]] = null
    
    def main() {
      //int3_init = Vec(1,2,3)
      //cell_centroid = FieldWithConst[Cell,Vec[_3,Int]](int3_init)
      face_values = FieldWithConst[Face,Int](0)
      interior_set = BoundarySet[Face]("default-interior")
      
      for(f <- interior_set) {
        face_values(f) = ID(f)
      }
      //for(f <- faces(mesh)) {
      for(f <- interior_set) {
        Print(face_values(f))
      }
    }
}
