import asplos._

trait SpadeFrame extends PPLApp {
  // TODO: What do these represent?
  val arcsinhCofactor = unit(5.0)
  val scale = unit(5.0)
  val isNormalize = unit(false)
  val normalizedWeight = unit(1.0)

  def countNeighbors(sData: Rep[Array1D[Double]], R: Rep[Int], D: Rep[Int], kernelWidth: Rep[Double], apprxWidth: Rep[Double]): Rep[Array1D[Int]]
  def main() { 
    val samples = read2D(DATA_FOLDER + "spade/nbt_1991_sd2.dat")
    val indices = read1D(DATA_FOLDER + "spade/indices.dat").map{_.toInt} // Random indices
    println("Matrix size: " + samples.nRows + " * " + samples.nCols)

    val R = samples.nRows
    val D = samples.nCols

    // Calculate median distance to nearest neighbor for random sample of 2000 points
    val subsampleSize = Math.min(samples.nRows, 2000)
    val sampleIndices = indices.slice(0 :@: subsampleSize)
    val neighborDists = collect(subsampleSize){i =>
      val sampleIdx = sampleIndices(i)
      val sample = samples.slice(sampleIdx, *)
      val nearest = reduce(R)((unit(0.0),unit(0))){j =>     // MinIndex loop
        val row = samples.slice(j, *)
        val dist = if (j != sampleIdx) 
                     reduce(D)(0.0){d => Math.abs(sample(d) - row(d)) }{_+_} // L1 distance metric
                   else
                     Double.PositiveInfinity
        (dist, j)
      }{(d1,d2) => if (tuple2_get1(d1) < tuple2_get1(d2)) d1 else d2
      }
      tuple2_get1(nearest) // Get index of closest class
    }
    val sortedDists = neighborDists.sort
    val medMinDist = sortedDists(subsampleSize / 2)

    val kernelWidth = medMinDist * scale
    val apprxWidth = medMinDist * 1.5
    println("Median min distance = " + medMinDist)
    println("Kernel width = " + kernelWidth + ", approx. width = " + apprxWidth)

    //val densities = countNeighbors(samples.data, R, D, kernelWidth, apprxWidth)
    //densities.slice(0 :@: 10).pprint
  }
}

object Spade extends PPLCompiler with SpadeApp
object SpadeFunc extends PPLCompiler with SpadeApp {
  registerFunction(countNeighbors _)
  override def functionName = "countNeighbors"
}
trait SpadeApp extends SpadeFrame {
  def countNeighbors(sData: Rep[Array1D[Double]], R: Rep[Int], D: Rep[Int], kernelWidth: Rep[Double], apprxWidth: Rep[Double]): Rep[Array1D[Int]] = {
    // ---------- Tiling Hints -----------
    tile(R, tileSize = 200, max = 500000)
    tile(D, tileSize = 20, max = 100)  
    // -----------------------------------

    val samples = Array2D(sData, R, D)
    val densities = Array1D[Int](R)

    forIndices(R){i => 
      if (densities(i) == 0) {
        val sample = samples.slice(i, *)

        // --- These three loops should be fused
        val dists = collect(R){j => 
          val row = samples.slice(j, *)
          reduce(D)(0.0){i => Math.abs(sample(j) - row(j)) }{_+_}
        }
        val neighbors = filter(R){j => dists(j) < apprxWidth}{j => j}
        val density = filterReduce(R)(0){j => dists(j) < kernelWidth}{j => 1}{_+_}
        // --- 

        forIndices(neighbors.length){j => densities(neighbors(j)) = density }
      }
    }

    densities
  }
}

// How to manually write fused filter/filterReduce/collect?

/*object SpadeBlocked extends PPLCompiler with SpadeBlockedApp
object SpadeBlockedFunc extends PPLCompiler with SpadeBlockedApp {
  registerFunction(countNeighbors _)
  override def functionName = "countNeighbors"
}
trait SpadeBlockedApp extends SpadeFrame {
  def countNeighbors(sData: Rep[Array1D[Double]], R: Rep[Int], D: Rep[Int], kernelWidth: Rep[Double], apprxWidth: Rep[Double]): Rep[Array1D[Int]] = {
    // ---------- Tiling Hints -----------
    tile(R, tileSize = 250, max = ?)
    tile(D, tileSize = 10, max = ?)  
    // -----------------------------------

  }
}*/
