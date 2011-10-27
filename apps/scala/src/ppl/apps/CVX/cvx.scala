import ppl.dsl.CVX._
object HelloCVXRunner extends CVXApplicationRunner with HelloCVX

trait HelloCVX extends CVXApplication {
  def main() = {
    val x = OptVar(10) 
    val y = x + 3  // does nothing! woohoo
    val z = 3 + x
    //val z = x + doubleToExpr(3.0)
    //val w = floatToExpr(3.0f) + x
  }
}
