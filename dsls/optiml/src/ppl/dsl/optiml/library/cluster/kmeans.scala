package ppl.dsl.optiml.library.cluster

import ppl.dsl.optiml._
import reflect.{Manifest, SourceContext}

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

    println("m:"+m+",n:"+n+",numClusters:"+numClusters+",mu.numRows:"+mu.numRows)

    val newMu = untilconverged(mu, tol){ mu =>
      iter += 1

      val c = (0::m){e => findNearestCluster(x(e), mu)}   

      /*
      (0::numClusters, *) { j =>
        val weightedpoints = sumRowsIf(0, m) (c(_) == j) { x(_) } 
        val points = c.count(_ == j)
        val d = if (points == 0) 1 else points 
        weightedpoints / d
      }
      */

      //TODO: mutable reduce with accInit
      val allWP = indexvector_hashreduce((0::m), i => c(i), i => x(i).Clone, (a:Rep[DenseVector[Double]],b:Rep[DenseVector[Double]]) => a + b)
      val allP = indexvector_hashreduce((0::m), i => c(i), i => 1, (a:Rep[Int],b:Rep[Int]) => a + b)

      (0::numClusters, *) { j =>
        val weightedpoints = allWP(j)
        val points = allP(j)
        val d = if (points == 0) 1 else points 
        weightedpoints / d
      }
    }((x, y) => dist(x, y, SQUARE), implicitly[Manifest[DenseMatrix[Double]]], implicitly[Cloneable[DenseMatrix[Double]]], implicitly[SourceContext])

    (iter,newMu)
  }

  private def findNearestCluster( x_i: Interface[Vector[Double]], mu: Rep[DenseMatrix[Double]] ): Rep[Int] = {
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
