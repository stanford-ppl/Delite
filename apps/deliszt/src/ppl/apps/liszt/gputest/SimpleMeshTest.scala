package ppl.apps.liszt.gputest

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object SimpleMeshTestRunner extends DeLisztApplicationRunner with SimpleMeshTest

trait SimpleMeshTest extends DeLisztApplication {
    var face_values : Rep[Field[Face,Double]] = null
    
    def main() {
      face_values = FieldWithConst[Face,Double](0.0)
      
      for(f <- faces(mesh)) {
        val c = if(ID(f) == 4) inside(f) else outside(f)
        face_values(f) = ID(c)
      }
      for(f <- faces(mesh)) {
        Print(face_values(f))
      }
    }
}
