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

  /*
  val max = cvxfun_vararg(
    vexity=convex,
    monotonicity=increasing)
  ((xs) => {
    val t = variable()
    for x <- xs {
      t >= x
    }
    minimize (t) over (t)
    t
  })

  val quad_over_lin = cvxfun (vexity=convex, sign=positive)
  
  val quad_over_lin = cvxfun (vexity=convex, sign=positive)
    arg (monotonicity_at_0=zero, shape=vector, name="x")
    arg (monotonicity=decreasing, shape=scalar, name="y")
    body    
  ((x, y) => {
    
  })

  //the only magic we support is:
  //  - universal functions (call a function that normally takes 1 scalar with a vector input; result is a vector)
  //  - varargs functions (call a function that normally takes 1 vector with some number of scalars)
  //note that only functions of 1 argument are supported for this magic

  val max = cvxfun (convex) with(n <- param) args(v <- increasing(n)) {
    val t = variable()
    for (i <- 0 until n) {
      t >= v(i)
    }
    return t
  }

  val max = cvxfun (convex) with(param)((n) => args(increasing(n))((v) => {
    val t = variable()
    for (i <- 0 until n) {
      t >= v(i)
    }
  }))

  val max = cvxfun (convex) args ((n) => vector(n, increasing) body ((v) => {
    val t = variable()
    for (i <- 0 until n) {
      t >= v(i)
    }
    return t
  }))

  val max = cvxfun (convex) body ((n) => ((v) => {
    v ~ vector(n, increasing)
    val t = variable()
    for (i <- 0 until n) {
      t >= v(i)
    }
    return t
  }))

  val max = cvxfun(vexity=convex, shape=vector, monotonicity=increasing) ((v) => {
    val t = variable()
    for (i <- 0 until v.size) {
      t >= v(i)
    }
    return t
  })

  def max(v: Vector#Increasing): Convex = {
    val t = variable()
    for (i <- 0 until v.size) {
      t >= v(i)
    }
    minimize (t) over (t)
    return t
  }

  def max(xs: Increasing*): Convex = {
    val t = variable()
    for(x <- xs) {
      t >= v(i)
    }
    minimize (t) over (t)
    return t
  }

  */

  /*
  object foobar {
    def foreach(fx: (Int)=>Unit): String = {
      fx(3)
      fx(4)
      return "bar"
    }
  } 

  val aaa = scala.Console.println(for (x <- foobar) { scala.Console.println(x) })
  */

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