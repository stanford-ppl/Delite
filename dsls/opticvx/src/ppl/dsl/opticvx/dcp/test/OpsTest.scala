package ppl.dsl.opticvx.dcp.test

import scala.collection.immutable.Seq
import ppl.dsl.opticvx.dcp._

object DCPOpsTestApp extends DCPOpsDefinite {
  def main(args: Array[String]) {
    val x = cvxexpr()
    val y = cvxexpr()
    solve(
      params(),
      given(),
      over(scalar -> x, scalar -> y),
      let(),
      where(
        x * 3 >= 2,
        y - x >= 1
      ),
      minimize(
        y
      )
    )
    println(x.resolve)
    println(y.resolve)

    /*
    val n = cvxparam()
    val l = cvxexpr()
    val x = cvxexpr()
    val y = cvxexpr()
    val z = cvxexpr()
    solve(
      params(3 -> n),
      given(double2inputdesc(3.4) -> l),
      over(scalar -> x, vector(n) -> y),
      let(x + x -> z),
      where(
        cfor(n)(i => (y(i) <= x)),
        x >= 0
      ),
      minimize(
        (sumfor(n)((i) => y(i))) - x
      )
    )
    println(l.resolve)
    println(x.resolve)
    println(y.resolve)
    println(z.resolve)
    */
  }
}
