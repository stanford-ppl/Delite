package ppl.dsl.optiml.library.cluster

import ppl.dsl.optiml.OptiMLApplication
import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix}
import ppl.dsl.optiml.datastruct.scala.TrainingSet

/* K-means clustering API for OptiML programs.
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 3/11/11
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

trait OptiMLKmeans {
  this: OptiMLApplication =>

  val kmeans = new kmeansOps

  // TODO: TrainingSet needs to have Labels be optional
  class kmeansOps {
    def cluster(x: Rep[TrainingSet[Double,Int]], numClusters: Rep[Int] = 32, tol: Rep[Double] = .001, initMu: Option[Rep[Matrix[Double]]] = None)
      = kmeans_cluster(x, numClusters, tol, initMu)
  }

  private def kmeans_cluster(x: Rep[TrainingSet[Double,Int]], numClusters: Rep[Int], tol: Rep[Double], initMu: Option[Rep[Matrix[Double]]]) = {
    val m = x.numSamples
    val n = x.numFeatures
    val mu = initMu getOrElse Matrix((0::numClusters) { e => x(random(x.numRows)) })
    var iter = 0

    val newMu = untilconverged(mu, tol){ mu =>
      iter += 1

      // update c -- calculate distances to current centroids
      val c = (0::m){e => findNearestCluster(x(e), mu)}

      // update mu -- move each cluster centroid to the mean of the points assigned to it
      (0::numClusters, *) { j =>
        val (weightedpoints, points) = t2( sum(0, m) { i =>
          if (c(i) == j){
            (x(i), unit(1.))
          }
          else {
            (ZeroVector[Double](n), unit(0.))
          }
        })
//        val weightedpoints = Vector.mzeros(n)
//        //val weightedpoints = Vector.zeros(n)
//        var points = 0
//        var i = 0
//        while (i < m){
//          if (c(i) == j){
//            weightedpoints += x(i) //TODO TR check mutable?
//            points += 1
//          }
//          i += 1
//        }

        if (points == 0) {
          weightedpoints
        }
        else weightedpoints / points
      }

    }
    (iter,newMu)
  }

  private def findNearestCluster( x_i: Rep[Vector[Double]], mu: Rep[Matrix[Double]] ): Rep[Int] = {
    // why is the parameter type needed?
    (mu mapRows { row: Rep[Vector[Double]] => dist(x_i, row, SQUARE) }).minIndex
    // TODO: sum needs to reflectEffect for this to generate correctly apparently
//    var min_d = Double.PositiveInfinity
//    var min_j = -1
//    var j = 0
//    while( j < mu.numRows ){
//      //println("-- j: " + j)
//      val dist = sum(0, x_i.length){ e => (x_i(e)-mu(j,e))*(x_i(e)-mu(j,e)) }
//      if (dist < min_d){
//        min_d = dist
//        min_j = j
//      }
//      j += 1
//    }
//
//    min_j
  }
}