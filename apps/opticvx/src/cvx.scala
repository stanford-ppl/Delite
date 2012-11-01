import ppl.dsl.opticvx._

import scala.util.Random

object HelloCVXRunner extends OptiCVXApplicationRunner with HelloCVX

trait HelloCVX extends OptiCVXApplication /*with OptiCVXLibrary*/ {
  def main() = {
    //println("Testing OptiCVX library functions:")
    //println("abs(-1.5) = " + resolve(abs(-1.5)))
    //println("max(1,2) = " + resolve(max(1.0,2.0)))
    //println("min(1,2) = " + resolve(min(1.0,2.0)))
    //println("norm2([3,4]) = " + resolve(norm2(inputvector(3.0,4.0))))
    //println("sqrt(4) = " + resolve(sqrt(4.0)))
    //println("square(3) = " + resolve(square(3.0)))
    //println("inv(0.5) = " + resolve(inv(0.5)))
    //println("geomean(2,8) = " + resolve(geomean(2.0,8.0)))
    
    /*
    var input: Rep[Array[Double]] = null
  
    val x = cvxexpr()
    val y = cvxexpr()
    val z = cvxexpr()
    val l = cvxexpr()
    val n = cvxparam()
    val m = cvxparam()
    solve(
      params(1 -> n, input.length -> m),
      given(ifor(n, (i) => input(i)) -> l),
      over(scalar -> x, vector(m) -> y),
      let(x + x -> z),
      where(
        cfor(m, (i) => (y(i) <= x)),
        x >= 0
      ),
      minimize(
        sum(m, (i) => y(i)*l(i)) - x
      )
    )
    */
    
    
    val x = cvxexpr()
    solve(
      params(),
      given(),
      over(scalar -> x),
      let(),
      where(
        x >= 1
      ),
      minimize(
        x
      )
    )
  }
}
