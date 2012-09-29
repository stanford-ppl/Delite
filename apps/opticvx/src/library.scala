import ppl.dsl.opticvx._

trait OptiCVXLibrary extends OptiCVXApplication {
  val abs = cvxfun (convex) arguments (nomonotonicity) body ((x) => {
    val t = variable()
    t >= x
    t >= -x
    minimize (t) over (t)
    t
  })

  val max = cvxfun (convex) arguments (increasing, increasing) body ((x,y) => {
    val t = variable()
    t >= x
    t >= y
    minimize (t) over (t)
    t
  })

  val min = cvxfun (concave) arguments (increasing, increasing) body ((x,y) => {
    val t = variable()
    t <= x
    t <= y
    minimize (-t) over (t)
    t
  })

  //for a lot of the functions supported in CVX, we need to be able to
  //add a variable number of constraints (depending on vector size), or
  //do vector slicing and concatenation, neither of which OptiCVX
  //currently supports

  val norm2 = cvxfun (convex) arguments (nomonotonicity) body ((x) => {
    val z = variable()
    constrain_secondordercone(x,z)
    minimize (z) over (z)
    z
  })

  val sqrt = cvxfun (concave) arguments (nondecreasing) body ((x) => {
    val v = variable(vector(1))
    constrain_rotatedcone(v,1.0,x)
    maximize (v(0)) over (v)
    v(0)
  })

  val square = cvxfun (convex) arguments (nomonotonicity) body ((x) => {
    val z = variable()
    constrain_rotatedcone(reshape(x,vector(1)),1.0,z)
    minimize (z) over (z)
    z
  })

  val inv = cvxfun (convex) arguments (decreasing) body ((x) => {
    val z = variable()
    constrain_rotatedcone(reshape(1.0,vector(1)),x,z)
    minimize (z) over (z)
    z
  })

  val geomean = cvxfun (concave) arguments (increasing, increasing) body ((x,y) => {
    val v = variable(vector(1))
    constrain_rotatedcone(v,x,y)
    maximize (v(0)) over (v)
    v(0)
  })
}