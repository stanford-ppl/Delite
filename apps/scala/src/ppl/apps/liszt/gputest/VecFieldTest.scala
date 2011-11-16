package ppl.apps.liszt.gputest

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object VecFieldTestRunner extends DeLisztApplicationRunner with VecFieldTest

trait VecFieldTest extends DeLisztApplication {
    var int3_init : Rep[Vec[_3,Double]] = null
    var cell_centroid : Rep[Field[Cell,Vec[_3,Double]]] = null
    var cell_volume : Rep[Field[Cell,Double]] = null
    
    def main() {
      int3_init = Vec(1.0,2.0,3.0)
      cell_centroid = FieldWithConst[Cell,Vec[_3,Double]](int3_init)
      cell_volume = FieldWithConst[Cell,Double](0)
      
      for(c <- cells(mesh)) {
        cell_volume(c) += cell_centroid(c)(2)
      }
      for(c <- cells(mesh)) {
        Print(cell_volume(c))
      }
    }
}
