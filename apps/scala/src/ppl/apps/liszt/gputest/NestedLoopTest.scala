package ppl.apps.liszt.gputest

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object NestedLoopTestRunner extends DeLisztApplicationRunner with NestedLoopTest

trait NestedLoopTest extends DeLisztApplication {
    var face_values : Rep[Field[Face,Double]] = null
    
    def main() {
      face_values = FieldWithConst[Face,Double](0.0)
      
      for(f <- faces(mesh)) {
        for(c <- cells(f)) {
          face_values(f) += ID(c)
        }
      }
      for(f <- faces(mesh)) {
        Print(face_values(f))
      }
    }
}
