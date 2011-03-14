package ppl.apps.bio.spade

import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix,TrainingSet}
import ppl.dsl.optiml.OptiMLApplication

trait Downsampling {
  this: OptiMLApplication =>

  def downsample(data: Rep[TrainingSet[Double,Int]], arcsinhCofactor: Rep[Double], scale: Rep[Double],
                 usedMarkers: Rep[Vector[Int]], isNormalize: Rep[Boolean], normalizeWeight: Rep[Double]): Rep[Vector[Int]] = {


    println("   Input matrix size: " + data.numRows + "*" + data.numCols)

    // TODO: we need to push this implementation detail out of the application and into the DSL
    // (1) number of cells if data matrix is small, (2) block size = 2000 if data large, (3) if number of cells is
    //  extremely large, we need to limit block size, otherwise line 38 (in matlab code) will be out of memory
    val numSamples = min(data.numRows, 2000, Math.floor(2500000000l/data.numRows).asInstanceOfL[Int])
    println("   numSamples = " + numSamples)

    val medMinDist = computeMedianMinDist(data, numSamples)
    println("   med_min_dist = " + medMinDist)

    val kernelWidth = scale * medMinDist
    val apprxWidth  = 1.5 * medMinDist
    println("   For this " + data.numRows + " channel data, KERNEL WIDTH is " + kernelWidth + ", APPRX WIDTH is " + apprxWidth)

    countNeighbors(data, kernelWidth, apprxWidth)
  }

  private def computeMedianMinDist(data: Rep[TrainingSet[Double,Int]], numSamples: Rep[Int],
                                   targetPrctile: Rep[Int] = 5, kernelWidthPara: Rep[Int] = 10) = {
    println("   finding empirical dist of the min distance between cells ...")

    // sampled_data is numSamples x data.numCols
    val sampleIndices = sample(0::data.numRows, numSamples)

    val minDist = (0::numSamples) { i =>
      val sampleIdx = sampleIndices(i)
      val neighborIdx = nearestNeighborIndex(sampleIdx, data, false)
      val d = dist(data(sampleIdx), data(neighborIdx))
      d
    }
    minDist.median
  }

  private def countNeighbors(data: Rep[TrainingSet[Double,Int]], kernelWidth: Rep[Double], apprxWidth: Rep[Double]): Rep[Vector[Int]] = {

    println("   finding local density for each cell ...")
    Vector[Int](data.numRows, true)

    /*
    val distances = Matrix[Double](data.numRows, data.numRows, (i,j) => data.dist(i,j))

    //streaming version (fastest streaming version)
    val densities = Vector[Int](data.numRows, true)
    distances.foreachRow { (row, idx) =>
      if(idx%1000 == 0) println("  (streaming) # processed node = " + idx)
      if(densities(idx) == 0) {
        val distancesInRange = row map (e => if (e < kernelWidth) 1 else 0)  // find neighbors in range
        val c = distancesInRange.sum               // count # of neighbors
        val neighbors = row find (_ < apprxWidth)    // find approx neighbors
        densities.updatev(neighbors,c)               // approximately update
      }
    }
    densities
    */

    /*
     * Fused should look like this:
     *
    val nRows = data.numRows
    val densities = Vector[Int](nRows)
    distances.foreachRow { (row, idx) =>
     if(idx%1000 == 0) println("  (streaming fusing - nRows, Array) # processed node = " + idx)
     if(densities(idx) == 0) {
       var c = 0
       var j = 0
       var neighbors = new Array[Int](nRows)
       var neighbor_id = 0
       while(j < nRows){
         val e = data.dist(idx, j)
         val r = if (e < kernel_width) 1 else 0
         c += r
         if(e < apprx_width){
           neighbors(neighbor_id) = j
           neighbor_id += 1
         }
         j += 1
       }
       while(neighbor_id > 0){
         neighbor_id -= 1
         densities(neighbors(neighbor_id)) = c
       }
     }
     */
  }

}