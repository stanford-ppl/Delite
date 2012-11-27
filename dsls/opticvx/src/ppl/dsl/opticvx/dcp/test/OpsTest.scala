package ppl.dsl.opticvx.dcp.test

import scala.collection.immutable.Seq
import ppl.dsl.opticvx.dcp._

object DCPOpsTestApp extends DCPOps {
  def main(args: Array[String]) {
    val x = cvxexpr()
    val y = cvxexpr()
    val z = cvxexpr()
    solve(
      params(),
      given(),
      over(scalar -> x, vector(3) -> y),
      let(x + x -> z),
      where(
        x >= 0
      ),
      minimize(
        z
      )
    )

    /*
    solve(
      params(),
      given(),
      over(scalar -> x, vector(input.length) -> y),
      let(x + x -> z),
      where(
        cfor(input.length)((n) => (y(n) <= x)),
        x >= 0
      ),
      minimize(
        sum(input.length)((n) => y(n)*input(n)) - x
      )
    )
    */
  }
}
