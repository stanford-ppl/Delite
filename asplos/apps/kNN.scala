import asplos._

trait kNNFrame extends PPLApp {
  val heldOutRatio = 0.2
  val K = 3

  // N - number of reference points
  // R - number of input points
  // D - dimensionality of point (number of columns)
  def evalKNN(refData: Rep[Array1D[Double]], refLabels: Rep[Array1D[Int]], 
              inData: Rep[Array1D[Double]], inLabels: Rep[Array1D[Int]], 
              D: Rep[Int]): Rep[Double]
  def main() {
    val data = read2D(DATA_FOLDER + "knn/input.dat")
    val labels = read1D(DATA_FOLDER + "knn/labels.dat").map{_.toInt}

    val nTestVecs = Math.floor(heldOutRatio * data.nRows).toInt

    val testData = data.bslice(0 :@: nTestVecs, *)
    val testLabels = labels.bslice(0 :@: nTestVecs)
    val trainData = data.bslice(nTestVecs :@: (data.nRows - nTestVecs), *)
    val trainLabels = labels.bslice(nTestVecs :@: (data.nRows - nTestVecs))
    val D = trainData.nCols

    val errCount = evalKNN(trainData.data, trainLabels, testData.data, testLabels, D)
    println("Total error count: " + errCount)
    println("Total error rate: " + errCount / nTestVecs.toDouble)
  }
}

object kNN extends PPLCompiler with kNNApp
object kNNFunc extends PPLCompiler with kNNApp {
  registerFunction(evalKNN _)
  override def functionName = "evalKNN"
}
trait kNNApp extends kNNFrame {
  def evalKNN(refData: Rep[Array1D[Double]], refLabels: Rep[Array1D[Int]], 
              inData: Rep[Array1D[Double]], inLabels: Rep[Array1D[Int]], 
              D: Rep[Int]): Rep[Double] = {

    val N = refLabels.length
    val R = inLabels.length

    // ---------- Tiling Hints -----------
    tile(N, tileSize = 100, max = ?)
    tile(R, tileSize = 200, max = ?)
    tile(D, tileSize = 100, max = 1000)
    // -----------------------------------

    val refs = Array2D(refData, N, D)
    val inputs = Array2D(inData, R, D)

    reduce(R)(0){i => 
      val pt = inputs.slice(i, *)
      val dists = collect(N){j => 
        val refPt = refs.slice(j, *)
        reduce(D)(0.0){d => val diff = refPt(d) - pt(d); diff*diff}{_+_}
      }
      val inds = sortIndices(N){(i,j) => if (dists(i) < dists(j)) 1 else -1 }
      val kIndices = inds.slice(0 :@: K)

      //val kLabels = groupByReduce(K){i => refLabels(kIndices(i))}{i => 1}{_+_}
      //val minLabel = reduce(kLabels.size)( (unit(0),unit(0)) ){i =>
      //  (kLabels.keys.apply(i), kLabels.values.apply(i))
      //}{(a,b) => if (tuple2_get2(a) > tuple2_get2(b)) a else b }
      //val cLabel = tuple2_get1(minLabel)
      val cLabel = 0
      if (cLabel == inLabels(i)) 0 else 1
    }{_+_}
  }
}
