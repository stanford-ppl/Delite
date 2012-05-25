package ppl.apps.bio.spade

import ppl.dsl.optiml._

trait Downsampling {
  this: OptiMLApplication =>

  def downsample(data: Rep[UnsupervisedTrainingSet[Double]], arcsinhCofactor: Rep[Double], scale: Rep[Double],
                 usedMarkers: Rep[DenseVector[Int]], isNormalize: Rep[Boolean], normalizeWeight: Rep[Double]): Rep[DenseVector[Int]] = {


    println("   Input matrix size: " + data.numSamples + "*" + data.numFeatures)

    // TODO: we need to push this implementation detail out of the application and into the DSL
    // (1) number of cells if data matrix is small, (2) block size = 2000 if data large, (3) if number of cells is
    //  extremely large, we need to limit block size, otherwise line 38 (in matlab code) will be out of memory
    val numSamples = min(data.numSamples, 2000, floor(2500000000l/data.numSamples).AsInstanceOf[Int])
    println("   numSamples = " + numSamples)

    val medMinDist = computeMedianMinDist(data, numSamples)
    println("   med_min_dist = " + medMinDist)

    val kernelWidth = scale * medMinDist
    val apprxWidth  = 1.5 * medMinDist
    println("   For this " + data.numSamples + " channel data, KERNEL WIDTH is " + kernelWidth + ", APPRX WIDTH is " + apprxWidth)

    countNeighbors(data, kernelWidth, apprxWidth)
  }

  private def computeMedianMinDist(data: Rep[UnsupervisedTrainingSet[Double]], numSamples: Rep[Int],
                                   targetPrctile: Rep[Int] = 5, kernelWidthPara: Rep[Int] = 10) = {
    println("   finding empirical dist of the min distance between cells ...")

    // sampled_data is numSamples x data.numFeatures
//    val sampleIndices = sample(0::data.numSamples, numSamples)
//
//    val minDist = (0::numSamples) { i =>
//      val sampleIdx = sampleIndices(i)
//      val neighborIdx = nearestNeighborIndex(sampleIdx, data, false)
//      dist(data(sampleIdx), data(neighborIdx))
//    }
//    minDist.median

    // TEMPORARY fixing this value to preserve deterministic results for comparison
    4.4593519740000005
  }

  private def countNeighbors(data: Rep[UnsupervisedTrainingSet[Double]], kernelWidth: Rep[Double], apprxWidth: Rep[Double]): Rep[DenseVector[Int]] = {

    println("   finding local density for each cell ...")

    //streaming version (fastest streaming version)
    val distances = Stream[Double](data.numSamples, data.numSamples){ (i,j) => dist(data(i),data(j)) }
    val densities = DenseVector[Int](data.numSamples, true)

    // NOTE: by allowing row.index to be expressed, we have essentially enabled writing the data race with densities.
    // This is a trade-off of restricted expression for performance, but is it the trade-off we want?
    for (row <- distances.rows) {
      if(row.index%1000 == 0) println("  (streaming) # processed node = " + row.index)
      if(densities(row.index) == 0) {
        val neighbors = row find { _ < apprxWidth }
        densities(neighbors) = row count { _ < kernelWidth }
      }
    }
    densities

    /* alternative :
    
    for (i <- (0 until data.numSamples)) {
      val row = (0::data.numSamples) { j => dist(data(i), data(j)) }
      if(i%1000 == 0) println("  (streaming) # processed node = " + i)
      if(densities(row.index) == 0) {
        val neighbors = row find { _ < apprxWidth }
        densities(neighbors) = row count { _ < kernelWidth }
      }
    }
    
    */



    /*
     * Fused should look like this:
     *
    val nRows = data.numSamples
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