package ppl.apps.bio.spade

import ppl.dsl.optiml._

trait Upsampling {
  this: OptiMLApplication =>

  /*
  def upsample(data: Rep[DenseMatrix[Double]],
               cluster_data: Rep[DenseMatrix[Double]],
               cluster_assign: Rep[DenseVector[Int]]) = {

    println("obs = "+data.numRows+", dim = "+data.numCols+", cls = "+cluster_data.numRows)

    val assign = data.mapRows {row1:Rep[DenseVector[Double]] =>
      val distances = cluster_data.mapRows{row2:Rep[DenseVector[Double]]=> dist(row1,row2)}
      val idx = distances.minIndex
      cluster_assign(idx)
    }
    assign

    /*
    val assign = data.mapRows {row1:Rep[DenseVector[Double]] =>
      val distances = cluster_data.mapRows{row2:Rep[DenseVector[Double]]=> dist(row1,row2)}
      val idx = distances.minIndex
      cluster_assign(idx)
    }
    assign
    */
    /*
    var assign = DenseVector[Int](data.numRows)
    for( idx <- (0::data.numRows)){
      if(idx%1000 == 0) println("  (imperative) # processed node = " + idx)
      var min_idx  = 0
      var min_dist = scala.Double.MaxValue
      var j = 0
      while(j < cluster_data.numRows){
        val d = dist(data(idx), cluster_data(j))
        if (d < min_dist) {
          min_idx = j
          min_dist = d
        }
        j += 1
      }
      assign(idx) = cluster_assign(min_idx)
    }
    assign
    */
  }
  */

}

