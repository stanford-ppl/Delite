package ppl.apps.liszt.gputest

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object CCWTestRunner extends DeLisztApplicationRunner with CCWTest

trait CCWTest extends DeLisztApplication {
    var cell_values : Rep[Field[Cell,Double]] = null
    
    def main() {
      cell_values = FieldWithConst[Cell,Double](2.0)
      
      for(c <- cells(mesh)) {
        for(f <- faces(c)) {
          for(e <- edgesCCW(towards(f,c))) {
            cell_values(c) += ID(e)
          }
        }
      }

      for(c <- cells(mesh)) {
        Print(cell_values(c))
      }
    }
}
