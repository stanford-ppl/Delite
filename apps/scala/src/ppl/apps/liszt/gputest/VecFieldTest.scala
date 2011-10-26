package ppl.apps.liszt.gputest

import ppl.dsl.deliszt.datastruct.scala._
import ppl.dsl.deliszt.datastruct.scala.MetaInteger._
import ppl.dsl.deliszt.{DeLisztApplicationRunner, DeLisztApplication}

object VecFieldTestRunner extends DeLisztApplicationRunner with VecFieldTest

trait VecFieldTest extends DeLisztApplication {
    var int3_init : Rep[Vec[_3,Int]] = null
    var cell_centroid : Rep[Field[Cell,Vec[_3,Int]]] = null
    var cell_volume : Rep[Field[Cell,Int]] = null
    
    def main() {
      int3_init = Vec(1,2,3)
      cell_centroid = FieldWithConst[Cell,Vec[_3,Int]](int3_init)
      cell_volume = FieldWithConst[Cell,Int](0)
      
      for(c <- cells(mesh)) {
        cell_volume(c) += cell_centroid(c)(2)
      }
      for(c <- cells(mesh)) {
        Print(cell_volume(c))
      }
    }
}
