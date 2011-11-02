package ppl.apps.liszt.gputest

import ppl.dsl.deliszt._
import ppl.dsl.deliszt.MetaInteger._

object MatFieldTestRunner extends DeLisztApplicationRunner with MatFieldTest

trait MatFieldTest extends DeLisztApplication {
  var int3_zero  : Rep[Vec[_3,Int]] = null
  var int33_zero : Rep[Mat[_3,_3,Int]] = null
  var PMat       : Rep[Field[Edge,Mat[_3,_3,Int]]] = null
  var edge_values: Rep[Field[Edge,Mat[_3,_3,Int]]] = null
    
  def main() {
    int3_zero  = Vec(1,2,3)
    int33_zero = Mat(int3_zero,int3_zero,int3_zero)
    PMat = FieldWithConst[Edge,Mat[_3,_3,Int]](int33_zero)
    edge_values = FieldWithConst[Edge,Int](0)
    
    for(e <- edges(mesh)) {
      edge_values(e) += PMat(e)
    }
    for(e <- edges(mesh)) {
      Print(edge_values(e))
    }
  }
}
