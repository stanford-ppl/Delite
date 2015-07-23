import asplos._

trait LogRegFrame extends PPLApp {
  def sigmoid(x: Rep[Double]): Rep[Double] = 1.0 / (1.0 + Math.exp(-1.0 * x))

  def refineTheta(xData: Rep[Array1D[Double]], y: Rep[Array1D[Double]], theta: Rep[Array1D[Double]], alpha: Rep[Double]): Rep[Array1D[Double]]
  def main() {
    val x = read2D(DATA_FOLDER + "/logreg/x1m10.dat") 
    val y = read1D(DATA_FOLDER + "/logreg/y1m.dat")

    val R = x.nRows
    val C = x.nCols
    val alpha = 1.0
    val tol = 0.001
    val MaxIter = 30
    val MinIter = 1

    val thetaOld = collect(C){i => 0.0}.unsafeMutable

    var delta = scala.Double.MaxValue
    var iter = 0

    while ( (Math.abs(delta) > tol && iter < MaxIter) ||  iter < MinIter) {
      val thetaNew = refineTheta(x.data, y, thetaOld.unsafeImmutable, alpha)

      delta = reduce(C)(0.0){i => Math.abs(thetaOld(i) - thetaNew(i)) }{_+_}
      forIndices(C){i => thetaOld(i) = thetaNew(i) }

      iter += 1
    }
    if (iter == MaxIter) { println("Maximum iterations exceeded") }

    val w = thetaOld
    println("w: "); w.pprint
  }
}

/* Logistic regression */
object LogReg extends PPLCompiler with LogRegApp
object LogRegFunc extends PPLCompiler with LogRegApp {
  registerFunction(refineTheta _)
  override def functionName = "refineTheta" 
}
trait LogRegApp extends LogRegFrame {
  def refineTheta(xData: Rep[Array1D[Double]], y: Rep[Array1D[Double]], theta: Rep[Array1D[Double]], alpha: Rep[Double]): Rep[Array1D[Double]] = {
    val R = y.length
    val C = theta.length
    // ---------- Tiling Hints -----------
    tile(R, tileSize = 250, max = 10000000)
    tile(C, tileSize = 10, max = 50)  
    // -----------------------------------

    val x = Array2D(xData, R, C)

    val gradient = reduce(R)(Array1D[Double](C)){i => 
      val row = x.slice(i, *)
      val m = reduce(C)(0.0){j => theta(j) * row(j)}{_+_}
      val mult = y(i) - sigmoid(m)
      collect(C){j => row(j) * mult}        // Row calculationg
    }{(a,b) => collect(C){j => a(j) + b(j)} } // Row summation

    // Calculate new theta
    collect(C){j => gradient(j)*alpha + theta(j)}
  }
}

/* Manually blocked logistic regression */
object LogRegBlocked extends PPLCompiler with LogRegBlockedApp
object LogRegBlockedFunc extends PPLCompiler with LogRegBlockedApp {
  registerFunction(refineTheta _)
  override def functionName = "refineTheta"
}
trait LogRegBlockedApp extends LogRegFrame {
  def refineTheta(xData: Rep[Array1D[Double]], y: Rep[Array1D[Double]], theta: Rep[Array1D[Double]], alpha: Rep[Double]): Rep[Array1D[Double]] = {
    val R = y.length
    val C = theta.length
    // ---------- Tiling Hints -----------
    tile(R, tileSize = 250, max = 10000000) // 10 million (could also just use ? = very large)
    tile(C, tileSize = 10, max = 50)  
    // -----------------------------------

    val x = Array2D(xData, R, C)

    // TBD: Is tiling the outer reduce useful? Seems not..
    // Case 1: C fits - no use in tiling reduce, just makes 2 copies of accumulator
    // Case 2: Duplicated accumulators, always loading chunks of one or the other to reduce
    val gradient = reduce(R)(Array1D[Double](C)){i => 
      val row = x.slice(i, *)
      val mBox = tileReduce[Double,Array1D[Double],Array1D[Double]](C)(box(0.0)){jj => 0 :@: 1}{jj => 
        val thetaChunk = theta.bslice(jj)
        val rowChunk = row.bslice(jj)
        val red = reduce(jj.len)(0.0){j => thetaChunk(j) * rowChunk(j)}{_+_}
        box(red)
      }{(a,b) => box(debox(a) + debox(b)) }

      val m = debox(mBox)
      val mult = y(i) - sigmoid(m)

      // Row calculation
      tileAssemble[Double,Array1D[Double],Array1D[Double]](C)( Array1D[Double](C) ){jj => jj}{jj => 
        val rowChunk = row.bslice(jj)
        collect(jj.len){j => rowChunk(j) * mult}
      }
    }{(a,b) =>
      tileAssemble[Double,Array1D[Double],Array1D[Double]](C)( Array1D[Double](C) ){jj => jj}{jj =>
        val aBlk = a.bslice(jj)
        val bBlk = b.bslice(jj)
        collect(jj.len){j => aBlk(j) + bBlk(j)}
      }
    }

    // Calculate new theta
    tileAssemble[Double,Array1D[Double],Array1D[Double]](C)( Array1D[Double](C) ){jj => jj}{jj => 
      val gradientBlk = gradient.slice(jj)
      val thetaBlk = theta.slice(jj)
      collect(jj.len){j => gradientBlk(j)*alpha + thetaBlk(j) }
    }
  }
}
