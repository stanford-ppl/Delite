package ppl.apps.liszt.gputest

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object BoundarySetTestRunner extends DeLisztApplicationRunner with BoundarySetTest

trait BoundarySetTest extends DeLisztApplication {
    var face_values : Rep[Field[Face,Double]] = null
    var interior_set : Rep[BoundarySet[Face]] = null
    
    def main() {
      face_values = FieldWithConst[Face,Double](0.0)
      interior_set = BoundarySet[Face]("default-interior")
      
      for(f <- interior_set) {
        face_values(f) = ID(f)
      }
      for(f <- interior_set) {
        Print(face_values(f))
      }
    }
}
