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

    k_means(x, mu, oldmu)
  }

  def k_means(x: Rep[Matrix[Double]], mu: Rep[Matrix[Double]], old_mu: Rep[Matrix[Double]]): (Rep[Int], Rep[Matrix[Double]]) = {
    val m = x.numRows
    val n = x.numCols
    val k = mu.numRows
    var iter = unit(0)

    untilconverged(mu, tol){ mu =>
      iter += 1

      // update c -- calculate distances to current centroids
      val c = (0::m){e => findNearestCluster(x(e), mu)}

      // update mu -- move each cluster centroid to the mean of the points assigned to it
      //(0::k).mforeach(j => {
      for (j <- (0::k)) {
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
    var min_d = unit(scala.Double.PositiveInfinity)
    var min_j = unit(-1)

    var j = unit(0)
    while( j < mu.numRows ){
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
