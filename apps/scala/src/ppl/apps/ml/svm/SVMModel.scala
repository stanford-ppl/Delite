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

import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix,TrainingSet}
import ppl.dsl.optiml.OptiMLExp
import ppl.delite.framework.DeliteApplication

trait SVMModel {
  // TODO: how do we clean this up in app code?
  val IR: DeliteApplication with OptiMLExp
  import IR._

  // model data
  private var weights: Rep[Vector[Double]] = null
  private var b = unit(0.0)

  // construct directly from model
  def load(modelFilename: Rep[String]) {
    val in = MLInputReader.readVector(modelFilename)
    b = in(in.length-1)
    weights = in.take(in.length-1)
  }

  /////////////
  // training

  def train(X: Rep[TrainingSet[Double,Double]], Y: Rep[Vector[Double]], C: Rep[Double], tol: Rep[Double], max_passes: Rep[Int]) = {
    println("Training SVM using the SMO algorithm")

    // internal model storage
    weights = Vector.zeros(X.numCols)

    // intermediate training info
    val alphas = Vector.zeros(X.numRows).t // col vector

    val numSamples = X.numRows
    var passes = unit(0)
    
    while (passes < max_passes){
      print(".")
      var num_changed_alphas = unit(0)
      for (i <- 0 until numSamples){
        val f_i = (alphas*Y*(X*X(i).t)).sum + b
        val E_i = f_i - Y(i)

        if (((Y(i)*E_i < -1.*tol) && (alphas(i) < C)) || ((Y(i)*E_i > tol) && (alphas(i) > 0))){
          // select a candidate j from the remaining numSamples-i samples at random
          var j = Math.floor(random[Double]*(numSamples-1)).asInstanceOfL[Int]+1
          while (j == i){
            j = Math.floor(random[Double]*(numSamples-1)).asInstanceOfL[Int]+1
          }

          val f_j = (alphas*Y*(X*X(j).t)).sum + b
          val E_j = f_j - Y(j)
                        
          var old_aj = alphas(j)
          var old_ai = alphas(i)

          // calculate bounds L and H that must hold in order for a_i, alphas(j) to
          // satisfy constraints and check
          var L = unit(0.0)
          var H = unit(0.0)
          if (Y(i) != Y(j)){
            L = Math.max(0., alphas(j) - alphas(i))
            H = Math.min(C, C + alphas(j) - alphas(i))
          }else{
            L = Math.max(0., alphas(i) + alphas(j) - C)
            H = Math.min(C, alphas(i) + alphas(j))
          }

          if (L != H){
            // calculate eta
            val eta = (X(i):*X(j)*2) - (X(i):*X(i)) - (X(j):*X(j))
            // check eta
            if (eta < 0){
              // compute new alphas(j)
              alphas(j) = alphas(j) - Y(j)*(E_i-E_j)/eta
              // clip alphas(j) if necessary
              if (alphas(j) > H) alphas(j) = H
              else if (alphas(j) < L) alphas(j) = L

              // check alphas(j) convergence
              if (Math.abs(alphas(j) - old_aj) >  .00001){
                // find a_i to maximize objective function
                old_ai = alphas(i)
                alphas(i) = alphas(i) + Y(i)*Y(j)*(old_aj-alphas(j))

                // compute the new b such that KKT conditions are satisfied
                val old_b = b
                // TODO: this is interesting; with deferral, we can compute both b1 and b2
                // without wasting cycles on the one that actually never gets used.
                val b1 = b - E_i - (X(i):*X(i))*Y(i)*(alphas(i)-old_ai) - (X(i):*(X(j)))*Y(j)*(alphas(j)-old_aj)
                val b2 = b - E_j - (X(i):*X(j))*Y(i)*(alphas(i)-old_ai) - (X(j):*(X(j)))*Y(j)*(alphas(j)-old_aj)
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
      } // for i = 1 to numSamples

      if (num_changed_alphas == 0){
        passes += 1
      }else{
        passes=0;
      }
    } // while

    // SMO finished
    // compute the weights (assuming a linear kernel)
    for (i <- 0 until X.numRows){
      weights = weights + X(i)*alphas(i)*Y(i)
    }
    print("\n")
  }

  ////////////
  // testing

  def classify(test_pt : Rep[Vector[Double]]) : Rep[Int] = {
    // SVM prediction is W'*X + b
    if ((weights:*test_pt + b) < 0){
      -1
    }
    else 1
  }

  ////////////
  // utility

  def saveModel(filename : Rep[String]) = {
    val out = weights.cloneL
    out += b
    MLOutputWriter.writeVector(out, filename)
  }
}