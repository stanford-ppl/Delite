package ppl.dsl.optiml.library.cluster

import ppl.dsl.optiml._

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
  this: OptiMLApplication with OptiMLLift =>

  val kmeans = new kmeansOps

  class kmeansOps {
    def cluster(x: Rep[UnsupervisedTrainingSet[Double]], numClusters: Rep[Int] = 32, tol: Rep[Double] = .001, initMu: Option[Rep[DenseMatrix[Double]]] = None)
      = kmeans_cluster(x, numClusters, tol, initMu)
  }

  private def kmeans_cluster(x: Rep[UnsupervisedTrainingSet[Double]], numClusters: Rep[Int], tol: Rep[Double], initMu: Option[Rep[DenseMatrix[Double]]]) = {
    val m = x.numSamples
    val n = x.numFeatures
    val mu = initMu getOrElse ((0::numClusters, *) { i => x(random(m)) })
    var iter = 0

    println("m:"+m+",n:"+n+",numClusters:"+numClusters+",mu.numRows:"+mu.numRows);

    val newMu = untilconverged(mu, tol){ mu =>
      iter += 1

      // update c -- calculate distances to current centroids
      val c = (0::m){e => findNearestCluster(x(e), mu)}

      // update mu -- move each cluster centroid to the mean of the points assigned to it
      (0::numClusters, * /*0::n*/) { j =>
        val weightedpoints = sumIf[DenseVector[Double],VectorView[Double]](0, m) (c(_) == j) { x(_) } 
        //val points = sumIf(0,m) (c(_) == j) { _ => 1 }
        val points = c.count(_ == j)  // cannot fuse because sum strips first iteration

        //if (points == 0) {
        //  weightedpoints          
        //}
        //else weightedpoints / points
        val d = if (points == 0) 1 else points 
        weightedpoints / d
      }

    }
    (iter,newMu)
  }

  private def findNearestCluster( x_i: Rep[VectorView[Double]], mu: Rep[DenseMatrix[Double]] ): Rep[Int] = {
    (mu mapRowsToVector { row => dist(x_i, row, SQUARE) }).minIndex
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
