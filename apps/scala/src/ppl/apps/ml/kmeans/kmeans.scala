package ppl.apps.ml.kmeans

import ppl.dsl.optiml._
import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix}
import ppl.delite.framework.DeliteApplication

object kmeansRunner extends OptiMLApplicationRunner with kmeans

trait kmeans extends OptiMLApplication {

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
    var iter = 0

    val newMu = untilconverged(mu, tol){ mu =>
      iter += 1

      // update c -- calculate distances to current centroids
      val c = (0::m){e => findNearestCluster(x(e), mu)}

      // update mu -- move each cluster centroid to the mean of the points assigned to it
      (0::k, *) { j =>
        //println("j: " + j)
        // this is much slower than the version below, even with variable boxing
        val (weightedpoints, points) = t2( sum(0, m) { i =>
          if (c(i) == j){
            (x(i), unit(1.))
          }
          else {
            (NilV[Double], unit(0.))
          }
        })
        // TODO: this does not appear to work anymore (need to check effect ordering in generated code)
//        val weightedpoints = Vector.mzeros(n)
//        var points = 0
//        var i = 0
//        while (i < m){
//          if (c(i) == j){
//            weightedpoints += x(i) //TODO TR check mutable?
//            points += 1
//          }
//          i += 1
//        }
        if (points == 0)
          points += 1
        weightedpoints / points
      }
    }
    (iter,newMu)
  }

  def findNearestCluster( x_i: Rep[Vector[Double]], mu: Rep[Matrix[Double]] ) : Rep[Int] = {
    // TODO: need minIndex for this
    //(mu mapRows { row => dist(x_i, row, SQUARE) }).minIndex
    var min_d = Double.PositiveInfinity
    var min_j = -1
    var j = 0
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
