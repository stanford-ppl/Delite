import asplos._

trait kMeansFrame extends PPLApp {
  def kMeans(xData: Rep[Array1D[Float]], muData: Rep[Array1D[Float]], M: Rep[Int], D: Rep[Int], K: Rep[Int]): Rep[Array1D[Float]]
  def main() {
    val x  = read2D(DATA_FOLDER + "kmeans/mandrill-large.dat")
    val mu = read2D(DATA_FOLDER + "kmeans/initmu.dat")

    val M = x.nRows   // Number of samples
    val D = x.nCols   // Number of dimensions per sample
    val K = mu.nRows  // Number of clusters
    println("M: " + M + ", D: " + D + ", K: " + K)

    // ---------- Tiling Hints -----------
    tile(M, tileSize = 100, max = ?)
    tile(D, tileSize = 5,  max = 5)
    tile(K, tileSize = 16, max = 16)
    tile(mu.nCols, tileSize = 16, max = 16) // HACK: Needed for "tiling" mu read
    // -----------------------------------

    //val muOld = mu.unsafeMutable

    //val tol = unit(0.001)     // Convergence tolerance
    //var delta = scala.Float.MaxValue
    //var iter = unit(0)
    //val maxIter = 30
    //val minIter = 1

    //while ( (Math.abs(delta) > tol && iter < maxIter) ||  iter < minIter) {
    val muNewData = kMeans(x.data, mu.data, M, D, K)
    val muNew = Array2D(muNewData, K, D)

    //  delta = reduce(K,D)(0.0){(i,j) => val diff = muOld(i,j) - muNew(i,j); diff*diff}{_+_} // SQUARE distance
    //  forIndices(K,D){(i,j) => muOld(i,j) = muNew(i,j) }

    //  iter += 1
    //}

    //val muNew = muOld 
    println("mu: "); muNew.pprint
  }
}

object kMeans extends PPLCompiler with kMeansApp
object kMeansFunc extends PPLCompiler with kMeansApp {
  registerFunction(kMeans _)
  override def functionName = "kMeans"
}
trait kMeansApp extends PPLCompiler with kMeansFrame {
  def kMeans(xData: Rep[Array1D[Float]], muData: Rep[Array1D[Float]], M: Rep[Int], D: Rep[Int], K: Rep[Int]): Rep[Array1D[Float]] = {
    // ---------- Tiling Hints -----------
    tile(M, tileSize = 40, max = ?)
    tile(D, tileSize = 5,  max = 5)
    tile(K, tileSize = 16, max = 16)
    // -----------------------------------
  
    val x = Array2D(xData, M, D)
    val muIn = Array2D(muData, K, D)
    val mu = muIn.bslice(0 :@: K, 0 :@: D) // TODO: Infer this automatically

    def minLabel(i: Exp[Int]): Exp[Int] = {
      val row = x.slice(i, *) 
      val minC = reduce(K)((unit(0.0f),unit(0))){j =>     // MinIndex loop
        val muRow = mu.slice(j, *)
        val dist = reduce(D)(0.0f){d => val diff = muRow(d) - row(d); diff*diff}{_+_} // SQUARE distance
        (dist, j)
      }{(d1,d2) => if (tuple2_get1(d1) < tuple2_get1(d2)) d1 else d2}
      tuple2_get2(minC) // Get index of closest class
    }

    val (wp, p) = fusedFatLoopNest2(M)(1){i => 
      // Common
      val rv0 = minLabel(i) :@: 1

      // Loop 1
      val defA = rawBlockReduce[Float,Array1D[Float],Array2D[Float]](i)(List(unit(1),D), List(0))(Array2D[Float](K,D))(List(rv0, 0 :@: D)){
        x.bslice(i, *)
      }{(a,b) => collect(D){j => a(j) + b(j)} }

      // Loop 2
      val defB = rawBlockReduce[Int,Array1D[Int],Array1D[Int]](i)(List(unit(1)), Nil)(Array1D[Int](K))(List(rv0)){
        box(unit(1))
      }{(a,b) => box(debox(a) + debox(b)) }

      (defA,defB)
    }

    // Divide by counts
    val newMu = blockAssem[Float,Array1D[Float],Array2D[Float]](K)(b0 = 1)(Array2D[Float](K,D))({ii => ii},{ii => 0 :@: D}){ii =>
      val weightedpoints = wp.slice(ii.start, *)
      val points = p(ii.start) 
      val d = if (points == 0) 1 else points
      collect(D){i => weightedpoints(i) / d}
    }
    newMu.data
  }
}

/* k-Means Unfused version */
/*object kMeans extends PPLCompiler with kMeansApp
object kMeansFunc extends PPLCompiler with kMeansApp {
  registerFunction(kMeans _)
  override def functionName = "kMeans"
}
trait kMeansApp extends kMeansFrame {
  def kMeans(xData: Rep[Array1D[Float]], muData: Rep[Array1D[Float]], M: Rep[Int], D: Rep[Int], K: Rep[Int]): Rep[Array1D[Float]] = {
    // ---------- Tiling Hints -----------
    tile(M, tileSize = 40, max = ?)
    tile(D, tileSize = 5,  max = 5)
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

    val wp = blockReduce[Float,Array1D[Float],Array2D[Float]](M)(b0 = 1)(Array2D[Float](K,D))({ii => c(ii.start) :@: 1}, {ii => 0 :@: D}){ii => 
      x.bslice(ii.start, *)
    }{(a,b) => collect(D){i => a(i) + b(i)} }

    val p = blockReduce[Int,Array1D[Int],Array1D[Int]](M)(b0 = 1)(Array1D[Int](K)){ii => c(ii.start) :@: 1}{ii => 
      box(unit(1))
    }{(a,b) => box(debox(a) + debox(b)) }

    // Divide by counts
    val newMu = blockAssem[Float,Array1D[Float],Array2D[Float]](K)(b0 = 1)(Array2D[Float](K,D))({ii => ii},{ii => 0 :@: D}){ii =>
      val weightedpoints = wp.slice(ii.start, *)
      val points = p(ii.start) 
      val d = if (points == 0) 1 else points
      collect(D){i => weightedpoints(i) / d}
    }
    newMu.data
  }
}*/

/* Manually fused k-Means */
/*object kMeansFused extends PPLCompiler with kMeansFusedApp
object kMeansFusedFunc extends PPLCompiler with kMeansFusedApp {
  registerFunction(kMeans _)
  override def functionName = "kMeans"
}
trait kMeansFusedApp extends kMeansFrame with PPLCompiler {
  def kMeans(xData: Rep[Array1D[Float]], muData: Rep[Array1D[Float]], M: Rep[Int], D: Rep[Int], K: Rep[Int]): Rep[Array1D[Float]] = {
    // ---------- Tiling Hints -----------
    tile(M, tileSize = 40, max = ?)
    tile(D, tileSize = 40, max = ?)
    tile(K, tileSize = 16, max = 16)
    // -----------------------------------
  
    val x = Array2D(xData, M, D)
    val mu = Array2D(muData, K, D)

    // Closest class ID for row i
    def closestLabel(i: Exp[Int]): Exp[Int] = {
      val row = x.slice(i, *) 
      val minC = reduce(K)((unit(0.0),unit(0))){j =>     // MinIndex loop
        val muRow = mu.slice(j, *)
        val dist = reduce(D)(0.0){d => val diff = muRow(d) - row(d); diff*diff}{_+_} // SQUARE distance
        (dist, j)
      }{(d1,d2) => if (tuple2_get1(d1) < tuple2_get1(d2)) d1 else d2}
      tuple2_get2(minC) // Get index of closest class
    }

    // TODO: Change to immutable
    val wp = blockReduce[Float,Array1D[Float],Array2D[Float]](M)(b0 = 1)(Array2D[Float](K,D))({ii => c(ii.start) :@: 1}, {ii => 0 :@: D}){ii => 
      x.bslice(ii.start, *)
    }{(a,b) => collect(D){i => a(i) + b(i)} }

    val p = blockReduce[Int,Array1D[Int],Array1D[Int]](M)(b0 = 1)(Array1D[Int](K)){ii => c(ii.start) :@: 1}{ii => 
      box(unit(1))
    }{(a,b) => box(debox(a) + debox(b)) }

    // Divide by counts
    val newMu = blockAssem[Float,Array1D[Float],Array2D[Float]](K)(b0 = 1)(Array2D[Float](K,D))({ii => ii},{ii => 0 :@: D}){ii =>
      val weightedpoints = wp.slice(ii.start, *)
      val points = p(ii.start) 
      val d = if (points == 0) 1 else points
      collect(D){i => weightedpoints(i) / d}
    }
    newMu.data
  }
}*/

/* Manually blocked k-Means */
/*object kMeansBlocked extends PPLCompiler with kMeansBlockedApp
object kMeansBlockedFunc extends PPLCompiler with kMeansBlockedApp {
  registerFunction(kMeans _)
  override def functionName = "kMeans"
}
trait kMeansBlockedApp extends kMeansFrame {
  def kMeans(xData: Rep[Array1D[Float]], muData: Rep[Array1D[Float]], M: Rep[Int], D: Rep[Int], K: Rep[Int]): Rep[Array1D[Float]] = {
    // ---------- Tiling Hints -----------
    tile(M, tileSize = 40, max = ?)
    tile(D, tileSize = 40, max = ?)
    tile(K, tileSize = 16, max = 16)
    // -----------------------------------
  
    val x = Array2D(xData, M, D)
    val mu = Array2D(muData, K, D)

  }
}*/