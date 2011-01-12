package ppl.apps.ml.kmeans

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix}
import ppl.delite.framework.DeliteApplication

object kmeans extends DeliteApplication with OptiMLExp {

  def print_usage = {
    println("Usage: kmeans <input data file> <initmu data file>")
    exit(-1)
  }
  
  private val tol = 0.001 // tolerance (for convergence)

  def main() {
    
    if (args.length < 1) print_usage

    val x = loadMatrix(args(0))
    val mu = loadMatrix(args(1))
    //val mu = Matrix((0::32) { e => x(randomInt(x.numRows)) })
    val oldmu = Matrix.zeros(mu.numRows, x.numCols)

    tic
    val (iter, mu2) = k_means(x, mu, oldmu)
    toc
    println("finished in " + iter + " iterations")
    mu2.pprint
  }

  def k_means(x: Rep[Matrix[Double]], mu: Rep[Matrix[Double]], old_mu: Rep[Matrix[Double]]): (Rep[Int], Rep[Matrix[Double]]) = {
    val m = x.numRows
    val n = x.numCols
    val k = mu.numRows
    var iter = unit(0)

    untilconverged(mu, tol){ mu =>
      iter += 1
      //println("iter: " + iter)

      // update c -- calculate distances to current centroids
      val c = (0::m){e => findNearestCluster(x(e), mu)}

      // update mu -- move each cluster centroid to the mean of the points assigned to it
      // TODO: switch to matrix constructor
      for (j <- (0::k)) {
      //for (j <- 0 until k) {
        //println("j: " + j)
        // this is much slower than the version below, even with variable boxing
//        val (weightedpoints, points) = t2( sum(0, m) { i =>
//          // TODO: the generated code is recalculating c every time!!  x321 line 166
//          if (c(i) == j){
//            (x(i), unit(1.))
//          }
//          else {
//            (NilV[Double], unit(0.))
//          }
//        })
        val weightedpoints = Vector.zeros(n)
        var points = unit(0)
        for (i <- 0 until m){
          if (c(i) == j){
            weightedpoints += x(i)
            points += 1
          }
        }
        if (points == 0) mu(j) = Vector.zeros(n)
        else mu(j) = weightedpoints / points
      }

      mu
    }
    (iter,mu)
  }

  def findNearestCluster( x_i: Rep[Vector[Double]], mu: Rep[Matrix[Double]] ) : Rep[Int] = {
    // TODO: need minIndex for this
    //(mu mapRows { row => dist(x_i, row, SQUARE) }).minIndex
    var min_d = Double.PositiveInfinity
    var min_j = unit(-1)
    var j = unit(0)
    while( j < mu.numRows ){
      //println("-- j: " + j)
      val dist = sum(0, x_i.length){ e => (x_i(e)-mu(j,e))*(x_i(e)-mu(j,e)) }
      if (dist < min_d){
        min_d = dist
        min_j = j
      }
      j += 1
    }

    return min_j
  }
}
