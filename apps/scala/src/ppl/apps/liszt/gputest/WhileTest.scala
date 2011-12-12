package ppl.apps.liszt.gputest

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object WhileTestRunner extends DeLisztApplicationRunner with WhileTest

trait WhileTest extends DeLisztApplication {
    var face_values : Rep[Field[Face,Double]] = null
    //var interior_set : Rep[BoundarySet[Face]] = null
    
    def main() {
      face_values = FieldWithConst[Face,Double](0.0)
      //interior_set = BoundarySet[Face]("default-interior")
      
      var idx = 0
      while (idx < 5) {
        val multiplier = idx * 2

        for(f <- faces(mesh)) {
          face_values(f) = ID(f) * multiplier
        }

        idx = idx + 1
      }

      for(f <- faces(mesh)) {
        Print(face_values(f))
      }
    }
}

