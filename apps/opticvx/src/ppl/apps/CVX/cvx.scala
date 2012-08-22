package ppl.apps.CVX

import ppl.dsl.CVX._

object HelloCVXRunner extends CVXApplicationRunner with HelloCVX
trait HelloCVX extends CVXApplication {
  def main() = {
    val x = OptVar(10) 
    val y = x + intToExpr(3) // does nothing! woohoo
    val z = intToExpr(3) + x
    val a = x + x + intToExpr(3)
    //val z = x + doubleToExpr(3.0)
    //val w = floatToExpr(3.0f) + x
  }
}
