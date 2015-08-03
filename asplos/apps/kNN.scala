import asplos._

trait kNNFrame extends PPLApp {
  val K = 3

  // N - number of training points
  // R - number of testing points
  // D - dimensionality of point (number of columns)
  def evalKNN(trainData: Rep[Array1D[Int]], trainLabels: Rep[Array1D[Int]], testData: Rep[Array1D[Int]], R: Rep[Int], D: Rep[Int]): Rep[Array1D[Int]]
  def main() {
    val data = readImg(DATA_FOLDER + "knn/letter-data.dat")
    val DR = data.nRows 
    val DC = data.nCols

    println("Read in data with " + DR + " rows and " + DC + " columns")

    // Tiling hints for 2D read (collect)
    // TODO: Shouldn't actually need to block the 2D read
    tile(DR, tileSize = 100, max = ?)
    tile(DC, tileSize = 20, max = 20)
    //------------------------------------

    val heldOutRatio = 0.2
    val N = Math.floor(heldOutRatio * DR).toInt  // Number of training instances
    val R = DR - N                               // Number of testing instances
    val D = DC - 1                               // Feature dimension

    println("Using " + R + " testing instances and " + N + " training instances")

    // TODO: Should be R :@: N for training data
    val testData  = data.bslice(0 :@: R, 0 :@: D); val testLabels  = data.bslice(0 :@: R, D)
    val trainData = data.bslice(R :@: N, 0 :@: D); val trainLabels = data.bslice(R :@: N, D)
   
    // ---------- Tiling Hints -----------
    tile(N, tileSize = 200, max = ?)
    tile(R, tileSize = 100, max = ?)
    tile(D, tileSize = 20, max = 20)
    // -----------------------------------

    val labelsOut = evalKNN(trainData.data, trainLabels, testData.data, R, D)

    var i = 0
    while (i < R) {
      if (labelsOut(i) != testLabels(i)) {
        println("#" + i + ": Incorrect (expected " + testLabels(i) + ", found " + labelsOut(i) + ")")
      }
      else {
        println("#" + i + ": Correct (expected " + testLabels(i) + ", found " + labelsOut(i) + ")")
      }
      i += 1
    }

    val nErr = reduce(R)(0){i => if (labelsOut(i) != testLabels(i)) 1 else 0 }{_+_}
    val percent = 100.0 * (nErr.toFloat / testLabels.length.toFloat)
    println("Number incorrect: " + nErr + "/" + testLabels.length + "(" + percent + "%)")
  
  }
}

object kNN extends PPLCompiler with kNNApp
object kNNFunc extends PPLCompiler with kNNApp {
  registerFunction(evalKNN _)
  override def functionName = "evalKNN"
}
trait kNNApp extends kNNFrame {
  def evalKNN(trainData: Rep[Array1D[Int]], trainLabels: Rep[Array1D[Int]], testData: Rep[Array1D[Int]], R: Rep[Int], D: Rep[Int]): Rep[Array1D[Int]] = {

    val N = trainLabels.length

    // ---------- Tiling Hints -----------
    tile(N, tileSize = 200, max = ?)
    tile(R, tileSize = 100, max = ?)
    tile(D, tileSize = 20, max = 20)
    // -----------------------------------

    val refs = Array2D(trainData, N, D)
    val inputs = Array2D(testData, R, D)

    // Blocked correctly
    collect(R){i => 
      val pt = inputs.slice(i, *)

      // TODO: How to block fold? Is it useful to block here?
      val kPairs = fold(K){z => (unit(100000), unit(0)) }(N){j => 
        val refPt = refs.slice(j, *)
        val dist = reduce(D)(0){d => val diff = refPt(d) - pt(d); diff*diff}{_+_}
        (dist, j)
      }{(a,b) =>
        a.priorityInsert(b){(x,y) => tuple2_get1(x) < tuple2_get1(y)}
      }{(a,b) => a}

      // Fits, no blocking necessary
      val kLabels = groupByReduce(K){i => trainLabels( tuple2_get2(kPairs(i)) )}{i => 1}{_+_}

      // Extra tiling hint - should be inferred automatically later by range analysis?
      val L = kLabels.size
      tile(L, tileSize = K, max = K)  // Maximum number of groups here is K

      // Fits, no blocking necessary
      val minPair = reduce(L)( (unit(0),unit(0)) ){i =>
          (kLabels.keys(i), kLabels.values(i))
      }{(a,b) => if (tuple2_get2(a) > tuple2_get2(b)) a else b }
      tuple2_get1(minPair)
    }
  }
}

/*object kNNBlocked extends PPLCompiler with kNNBlockedApp
object kNNBlockedFunc extends PPLCompiler with kNNBlockedApp {
  registerFunction (evalKNN _)
  override def functionName = "evalKNN"
}
trait kNNBlockedApp extends kNNFrame {
  def evalKNN(trainData: Rep[Array1D[Float]], trainLabels: Rep[Array1D[Int]], 
              testData: Rep[Array1D[Float]], testLabels: Rep[Array1D[Int]], 
              D: Rep[Int]): Rep[Array1D[Int]] = {

    val N = trainLabels.length
    val R = testLabels.length

    // ---------- Tiling Hints -----------
    tile(N, tileSize = 100, max = ?)
    tile(R, tileSize = 200, max = ?)
    tile(D, tileSize = 100, max = 1000)
    // -----------------------------------

    val refs = Array2D(trainData, N, D)
    val inputs = Array2D(testData, R, D)

    tileAssemble[Int,Array1D[Int],Array1D[Int]]( Array1D[Int](R) ){ii => ii}{ii =>
      val ptBlk = input.bslice(ii, *)
      collect(ii.len){i => 
        val pt = ptBlk.bslice(i, *)
        
        // TODO: map function produces something of size B, accumulator is of size K...
        val kIndices = tiledReduce(N)( Array1D[(Float,Int)](K) ){jj => 
          val refBlk = refs.bslice(jj, *)
          collect(jj.len){j => 
            val refPt = refBlk.bslice(j, *)
            // D is small, shouldn't be blocked
            val dist = reduce(D)(0.0){d => val diff = refPt(d) - pt(d); diff*diff}{_+_}
            (dist, j)
          }
        }{(a,b) => 
          // concatenate a and b
          // sortIndices of concatenated array, using tuple2_get1 as comparison
          // take K
        }
        //val inds = sortIndices(N){(i,j) => if (dists(i) < dists(j)) 1 else -1 }
        //val kIndices = inds.bslice(0 :@: K)

        val kLabels = groupByReduce(K){i => trainLabels(kIndices(i))}{i => 1}{_+_}

        // Extra tiling hint
        val L = kLabels.size
        tile(L, tileSize = 3, max = 3)

        val minLabel = reduce(L)( (unit(0),unit(0)) ){i =>
            (kLabels.keys.apply(i), kLabels.values.apply(i))
        }{(a,b) => if (tuple2_get2(a) > tuple2_get2(b)) a else b }
        tuple2_get1(minLabel)
      }
    }

  }
}*/
