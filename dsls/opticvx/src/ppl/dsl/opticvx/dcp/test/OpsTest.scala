package ppl.dsl.opticvx.dcp.test

import scala.collection.immutable.Seq
import ppl.dsl.opticvx.dcp._

object DCPOpsTestApp extends DCPOps {
  
  val square = {
    val x = cvxexpr()
    val t = cvxexpr()
    cvxfun(
      params(),
      given(),
      args(scalar -> x),
      sign(positive),
      tonicity(x.sign),
      vexity(positive),
      over(scalar -> t),
      let(),
      where(
        in_secondorder_cone(cat(2*x, t-1), t+1)
      ),
      maximize(t)
    )
  }

  def main(args: Array[String]) {
    
    /*
    val x = cvxexpr()
    val y = cvxexpr()
    val v = cvxexpr()
    val n = cvxparam()
    solve(
      params(4 -> n),
      given(vector_input(n)((i) => i) -> v),
      over(scalar -> x, vector(n) -> y),
      let(),
      where(
        cfor(n)(i => y(i) >= v(i)),
        in_secondorder_cone(cat(2*y(2), x-1), x+1)
      ),
      minimize(
        x + sumfor(n)(i => y(i))
      )
    )
    println(x.resolve)
    println(v.resolve)
    println(y.resolve)
    */

    val x = cvxexpr
    val y = cvxexpr
    val z = cvxexpr
    solve(
      params(), given(),
      over(scalar -> x), 
      let(),
      where(
        square(x) <= 2.0
      ),
      maximize(
        x
      )
    )
    println("x = " + x.resolve(0).toString)

    /* maybe:

    trait MyProblem extends OptiCVXProblem {
      trait params {
        self: _params =>
        val n = param()
      }
      trait given {
        self: _given =>
        val v = vector(n)
      }
      trait over {
        self: _over =>
        val x = scalar()
        val y = vector(n)
      }
      trait let {
        self: _let =>
      }
      def where(implicit e: _where) = {
        import e._
        for (i <- n) yield y(i) >= v(i)
        in_secondorder_cone(cat(2*y(2), x-1), x+1)
      }
    }

    */

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
