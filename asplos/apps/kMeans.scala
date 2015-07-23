import asplos._

trait kMeansFrame extends PPLApp {
  def kMeans(xData: Rep[Array1D[Double]], muData: Rep[Array1D[Double]], M: Rep[Int], D: Rep[Int], K: Rep[Int]): Rep[Array1D[Double]]
  def main() {
    val x  = read2D(DATA_FOLDER + "/kmeans/mandrill-large.dat")
    val mu = read2D(DATA_FOLDER + "/kmeans/initmu.dat")

    val M = x.nRows   // Number of samples
    val D = x.nCols   // Number of dimensions per sample
    val K = mu.nRows  // Number of clusters
    println("M: " + M + ", D: " + D + ", K: " + K)

    val muOld = mu.unsafeMutable

    val tol = unit(0.001)     // Convergence tolerance
    var delta = scala.Double.MaxValue
    var iter = unit(0)
    val maxIter = 30
    val minIter = 1

    while ( (Math.abs(delta) > tol && iter < maxIter) ||  iter < minIter) {
      val muNewData = kMeans(x.data, muOld.unsafeImmutable.data, M, D, K)
      val muNew = Array2D(muNewData, K, D)

      delta = reduce(K,D)(0.0){(i,j) => val diff = muOld(i,j) - muNew(i,j); diff*diff}{_+_} // SQUARE distance
      forIndices(K,D){(i,j) => muOld(i,j) = muNew(i,j) }

      iter += 1
    }

    val muNew = muOld 
    println("mu: "); muNew.pprint
  }
}

/* k-Means */
object kMeans extends PPLCompiler with kMeansApp
object kMeansFunc extends PPLCompiler with kMeansApp {
  registerFunction(kMeans _)
  override def functionName = "kMeans"
}
trait kMeansApp extends kMeansFrame {
  def kMeans(xData: Rep[Array1D[Double]], muData: Rep[Array1D[Double]], M: Rep[Int], D: Rep[Int], K: Rep[Int]): Rep[Array1D[Double]] = {
    // ---------- Tiling Hints -----------
    tile(M, tileSize = 40, max = ?)
    tile(D, tileSize = 40, max = ?)
    tile(K, tileSize = 16, max = 16)
    // -----------------------------------
  
    val x = Array2D(xData, M, D)
    val mu = Array2D(muData, K, D)

    // Closest class IDs for all M samples
    val c = collect(M){i =>
      val row = x.slice(i, *) 
      val minC = reduce(K)((unit(0.0),unit(0))){j =>     // MinIndex loop
        val muRow = mu.slice(j, *)
        val dist = reduce(D)(0.0){d => val diff = muRow(d) - row(d); diff*diff}{_+_} // SQUARE distance
        (dist, j)
      }{(d1,d2) => if (tuple2_get1(d1) < tuple2_get1(d2)) d1 else d2}
      tuple2_get2(minC) // Get index of closest class
    }

    // TODO: Change to immutable
    val wp = blockReduce[Double,Array1D[Double],Array2D[Double]](M)(b0 = 1)(Array2D[Double](K,D))({ii => c(ii.start) :@: 1}, {ii => 0 :@: D}){ii => 
      x.bslice(ii.start, *)
    }{(a,b) => collect(D){i => a(i) + b(i)} }

    val p = blockReduce[Int,Array1D[Int],Array1D[Int]](M)(b0 = 1)(Array1D[Int](K)){ii => c(ii.start) :@: 1}{ii => 
      box(unit(1))
    }{(a,b) => box(debox(a) + debox(b)) }

    // Divide by counts
    val newMu = blockAssem[Double,Array1D[Double],Array2D[Double]](K)(b0 = 1)(Array2D[Double](K,D))({ii => ii},{ii => 0 :@: D}){ii =>
      val weightedpoints = wp.slice(ii.start, *)
      val points = p(ii.start) 
      val d = if (points == 0) 1 else points
      collect(D){i => weightedpoints(i) / d}
    }
    newMu.data
  }
}

/* Manually blocked k-Means */
/*object kMeansBlocked extends PPLCompiler with kMeansBlockedApp
object kMeansBlockedFunc extends PPLCompiler with kMeansBlockedApp {
  registerFunction(kMeans _)
  override def functionName = "kMeans"
}
trait kMeansBlockedApp extends kMeansFrame {
  def kMeans(xData: Rep[Array1D[Double]], muData: Rep[Array1D[Double]], M: Rep[Int], D: Rep[Int], K: Rep[Int]): Rep[Array1D[Double]] = {
    // ---------- Tiling Hints -----------
    tile(M, tileSize = 40, max = ?)
    tile(D, tileSize = 40, max = ?)
    tile(K, tileSize = 16, max = 16)
    // -----------------------------------
  
    val x = Array2D(xData, M, D)
    val mu = Array2D(muData, K, D)

  }
}*/