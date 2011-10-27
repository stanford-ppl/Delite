import ppl.dsl.CVX._
object HelloCVXRunner extends CVXApplicationRunner with HelloCVX

trait HelloCVX extends CVXApplication {
  def main() = {
    val x = OptVar(10) 
    val y = x + unit(3)  // does nothing! woohoo
  }
}
