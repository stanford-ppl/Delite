package ppl.dsl.optiml.library.regression

import ppl.dsl.optiml._

/* Linear Regression API for OptiML programs.
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 11/1/11
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

trait OptiMLLinReg {
  this: OptiMLApplication =>

  val linreg = new LinregOps

  class LinregOps {
    def weighted(x: Rep[DenseMatrix[Double]], y: Rep[DenseVector[Double]])
      = linreg_weighted(x,y)
      
    def unweighted(x: Rep[DenseMatrix[Double]], y: Rep[DenseVector[Double]])
      = linreg_unweighted(x,y)
  }

  // unweighted linear regression using the normal equations
  // input: input training vector x
  //        output training vector y
  // output: predictions along uniformly sampled points
  private def linreg_unweighted(x: Rep[DenseMatrix[Double]], y: Rep[DenseVector[Double]]): Rep[DenseVector[Double]] = {
    // by convention, x_0 = 1
    val X = x.mutable
    X.insertCol(0, Vector.ones(X.numRows).t) 
    
    // theta = inv(X.'X)*(X.'*y) (if y is a col vector)
    val theta = ((X.t*X).inv)*(X.t*y)

    // the resulting fitted line is given by the equation
    //   h(x) = theta_0 + theta_1*x_1 + ...
    theta
  }

  private def linreg_weighted(x: Rep[DenseMatrix[Double]], y: Rep[DenseVector[Double]]): Rep[DenseVector[Double]] = {
    val tau = 10
    val X = x.mutable
    X.insertCol(0, Vector.ones(X.numRows).t) 
  
    // initialize prediction points
    val xstep = 25.0/X.numRows
    val xref_pts = Vector.uniform(-10, xstep, 14.99).t
    //val xref = Matrix(Vector.ones(xref_pts.length).t, xref_pts) 
    val xref = ((0::x.numCols+1,*) { i => if (i == 0) Vector.ones(xref_pts.length) else xref_pts }).t
    
    //val O = Matrix.identity(X.numRows)
    val Xt = X.t

    // calculate predictions
    val guess = (0::xref.numRows){ e =>
      val x_cur = xref(e) //xref(e,1)
      val weights = X.mapRowsToVector(row => exp(((x_cur-row)*:*(x_cur-row).t)/(2.0*tau*tau)*(-1)))
      //val weights = x.map(ele => exp(-.1*(x_cur-ele)*(x_cur-ele)/(2.0*tau*tau))/2.0)
      val W = Matrix.diag(weights.length, weights) // M x M
      val t1 = Xt*W
      val theta = ((t1*X).inv)*(t1*y)
      (theta.t) *:* (xref(e).t)
    }

    guess
  }
}