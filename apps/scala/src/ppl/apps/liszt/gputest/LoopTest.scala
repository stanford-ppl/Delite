package ppl.apps.liszt.gputest

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object MeshTestRunner extends DeLisztApplicationRunner with MeshTest

trait MeshTest extends DeLisztApplication {
    var face_values : Rep[Field[Face,Int]] = null
    
    def main() {
      face_values = FieldWithConst[Face,Int](0)
      
      for(f <- faces(mesh)) {
        val c = if(ID(f) == 4) inside(f) else outside(f)
        val v = if(ID(c) == 3) vertex(c,0) else vertex(c,1)
        face_values(f) += ID(c) + ID(v)
      }
      for(f <- faces(mesh)) {
        Print(face_values(f))
      }
    }
}
