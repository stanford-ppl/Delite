package ppl.apps.liszt.gputest

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object CCWTestRunner extends DeLisztApplicationRunner with CCWTest

trait CCWTest extends DeLisztApplication {
    var cell_values : Rep[Field[Cell,Int]] = null
    var cell_values_1 : Rep[Field[Cell,Int]] = null
    
    def main() {
      cell_values = FieldWithConst[Cell,Int](2)
      
      for(c <- cells(mesh)) {
        for(f <- faces(c)) {
          for(e <- edgesCCW(towards(f,c))) {
          //for(e <- edgesCCW(f)) {
            cell_values(c) += ID(e)
          }
        }
      }

      for(c <- cells(mesh)) {
        Print(cell_values(c))
      }
    }
}
