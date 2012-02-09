package ppl.apps.ml.svm

/* SVMModel encapsulates the necessary information for SVM training and classification.
 * It also publishes algorithms for training and testing the SVM.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Jun 21, 2009
 * modified: Jun 23, 2009
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml._

trait SVMModel { this: OptiMLApplication =>

  // model data
  // TODO: NPE here from IR being null until the constructor is finished and...
  //private var weights: Var[Vector[Double]] = null
  //private var b: Var[Double] = null

  // construct directly from model
  def load(modelFilename: Rep[String]) = {
    val in = readVector(modelFilename)
    val b = in(in.length-1)
    val weights = in.take(in.length-1)
    (weights, b)
  }

  /////////////
  // training

  //def train(X: Rep[TrainingSet[Double,Double]], C: Rep[Double], tol: Rep[Double], max_passes: Rep[Int]) = {
  def train(X: Rep[DenseMatrix[Double]], labels: Rep[DenseVector[Double]], C: Rep[Double], tol: Rep[Double], max_passes: Rep[Int]) = {
    println("Training SVM using the SMO algorithm")

    // adjust the classification labels to -1 and +1 for SMO
    val Y = /*X.*/labels map { e => if (e == 0) -1. else 1. }

    // internal model storage
    //val weights = Vector.zeros(X.numCols).mutable
    var b = 0.0

    // intermediate training info
    //var alphas = Vector.zeros(X.numRows).mt // col vector
    val alphas = Vector.zeros(X.numRows).mutable
    alphas.mt // col vector

    val numSamples = X.numRows
    var passes = 0
    var iter = 0

    while (passes < max_passes){
      print(".")
      iter += 1
      var num_changed_alphas = 0
      var i = 0
      while(i < numSamples){ //TR
      //for (i <- 0 until numSamples) {
        // TODO: x761 -- code is recalculating alphas from original definition here
        val alphasOld = alphas.Clone
        
        val f_i = (alphasOld*Y*(X*X(i).t)).sum + b //TR M*V alph0
        val E_i = f_i - Y(i)

        if (((Y(i)*E_i < -1.*tol) && (alphasOld(i) < C)) || ((Y(i)*E_i > tol) && (alphasOld(i) > 0))){
          // select a candidate j from the remaining numSamples-i samples at random
          var j = floor(random[Double]*(numSamples-1)).AsInstanceOf[Int]+1
          while (j == i){
            j = floor(random[Double]*(numSamples-1)).AsInstanceOf[Int]+1
          }

          val f_j = (alphasOld*Y*(X*X(j).t)).sum + b //TR M*V alph0 -- inside if, cannot be fused with the one in f_i (calc actually happens further down)
          val E_j = f_j - Y(j)
                        
          val old_aj = alphasOld(j) //TR: making it a val should not move it down!
          //var old_ai = alphas(i)

          // calculate bounds L and H that must hold in order for a_i, alphas(j) to
          // satisfy constraints and check
          var L = 0.0
          var H = 0.0
          if (Y(i) != Y(j)){
            L = max(0., alphasOld(j) - alphasOld(i))
            H = min(C, C + alphasOld(j) - alphasOld(i))
          }else{
            L = max(0., alphasOld(i) + alphasOld(j) - C)
            H = min(C, alphasOld(i) + alphasOld(j))
          }

          if (L != H){ //TR: problem: if/then/else will not force old_aj
            // calculate eta
            val eta = (X(i)*:*X(j)*2) - (X(i)*:*X(i)) - (X(j)*:*X(j))
            // check eta
            if (eta < 0){
              // compute new alphas(j)

              //alphas = alphas.Clone //TR
              alphas(j) = alphasOld(j) - Y(j)*(E_i-E_j)/eta //TR functionalize?

              // clip alphas(j) if necessary
              if (alphas(j) > H) alphas(j) = H
              else if (alphas(j) < L) alphas(j) = L

              // check alphas(j) convergence
              if (abs(alphas(j) - old_aj) >  .00001){
                // find a_i to maximize objective function

                val old_ai = alphasOld(i)
                //alphas = alphas.Clone //TR
                alphas(i) = alphasOld(i) + Y(i)*Y(j)*(old_aj-alphas(j)) //TR functionalize?

                // compute the new b such that KKT conditions are satisfied
                val old_b = b
                val b1 = b - E_i - (X(i)*:*X(i))*Y(i)*(alphas(i)-old_ai) - (X(i)*:*(X(j)))*Y(j)*(alphas(j)-old_aj)
                val b2 = b - E_j - (X(i)*:*X(j))*Y(i)*(alphas(i)-old_ai) - (X(j)*:*(X(j)))*Y(j)*(alphas(j)-old_aj)
                if ((alphas(i) > 0) && (alphas(i) < C)){
                  b = b1
                }
                if ((alphas(j) > 0) && (alphas(j) < C)){
                  b = b2
                }
                if (old_b == b){
                  // neither threshold valid
                  b = ((b1+b2)/2)
                }

                num_changed_alphas += 1
              } // alpha converged?
            } // negative eta?
          } // L != H?
        } // main if (select alphas)
        i += 1 //TR
      } // for i = 1 to numSamples

      if (num_changed_alphas == 0){
        passes += 1
      }else{
        passes = 0;
      }
    } // while

  println("ITER " + iter)

    // SMO finished
    println("num iterations: " + iter)

    // compute the weights (assuming a linear kernel)
    val weights = sum(0,X.numRows) { i =>
		X(i) * alphas(i) * Y(i)
	}
  /*
	var i = 0
    while(i < X.numRows){
    //for (i <- 0 until X.numRows){
   //   weights += X(i)*alphas(i)*Y(i)
      i += 1
    }
	*/
    print("\\n")

    (weights, b)
  }

  ////////////
  // testing

  def classify(weights: Rep[DenseVector[Double]], b: Rep[Double], test_pt: Interface[Vector[Double]]): Rep[Int] = {
    // SVM prediction is W'*X + b
    if ((weights*:*test_pt + b) < 0){
      -1
    }
    else 1
  }

  ////////////
  // utility

  def saveModel(weights: Rep[DenseVector[Double]], b: Rep[Double], filename: Rep[String]) = {
    val out = weights.Clone
    out += b
    writeVector(out, filename)
  }
}
