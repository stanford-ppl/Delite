import asplos._

trait LogRegFrame extends PPLApp {
  def sigmoid(x: Rep[Float]): Rep[Float] = 1.0f / (1.0f + Math.exp(-1.0f * x).toFloat)

  def refineTheta(xData: Rep[Array1D[Float]], y: Rep[Array1D[Float]], theta: Rep[Array1D[Float]], alpha: Rep[Float]): Rep[Array1D[Float]]
  def main() {
    val x = read2D(DATA_FOLDER + "logreg/x1m10.dat") 
    val y = read1D(DATA_FOLDER + "logreg/y1m.dat")

    val R = x.nRows
    val C = x.nCols

    // ---------- Tiling Hints -----------
    tile(R, tileSize = 250, max = 10000000)
    tile(C, tileSize = 20, max = 20)  
    // -----------------------------------

    val alpha = 1.0f
    //val tol = 0.001
    //val MaxIter = 30
    //val MinIter = 1

    //val thetaOld = collect(C){i => 0.0}.unsafeMutable
    val theta = collect(C){i => 0.0f}

    //var delta = scala.Float.MaxValue
    //var iter = 0

    //while ( (Math.abs(delta) > tol && iter < MaxIter) ||  iter < MinIter) {
    val thetaNew = refineTheta(x.data, y, theta, alpha)

    //  delta = reduce(C)(0.0){i => Math.abs(thetaOld(i) - thetaNew(i)) }{_+_}
    //  forIndices(C){i => thetaOld(i) = thetaNew(i) }

    //  iter += 1
    //}
    //if (iter == MaxIter) { println("Maximum iterations exceeded") }

    //val w = thetaOld
    println("w: "); thetaNew.pprint
  }
}

/* Logistic regression */
object LogReg extends PPLCompiler with LogRegApp
object LogRegFunc extends PPLCompiler with LogRegApp {
  registerFunction(refineTheta _)
  override def functionName = "refineTheta" 
}
trait LogRegApp extends LogRegFrame {
  def refineTheta(xData: Rep[Array1D[Float]], y: Rep[Array1D[Float]], theta: Rep[Array1D[Float]], alpha: Rep[Float]): Rep[Array1D[Float]] = {
    val R = y.length
    val C = theta.length
    // ---------- Tiling Hints -----------
    tile(R, tileSize = 250, max = 10000000)
    tile(C, tileSize = 20, max = 20)  
    // -----------------------------------

    val x = Array2D(xData, R, C)

    val gradient = reduce(R)(Array1D[Float](C)){i => 
      val row = x.slice(i, *)
      val m = reduce(C)(0.0f){j => theta(j) * row(j)}{_+_}
      val mult = y(i) - sigmoid(m)
      collect(C){j => row(j) * mult}        // Row calculating
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
  def refineTheta(xData: Rep[Array1D[Float]], y: Rep[Array1D[Float]], theta: Rep[Array1D[Float]], alpha: Rep[Float]): Rep[Array1D[Float]] = {
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
    val gradient = reduce(R)(Array1D[Float](C)){i => 
      val row = x.slice(i, *)
      val mBox = tileReduce[Float,Array1D[Float],Array1D[Float]](C)(box(0.0f)){jj => 0 :@: 1}{jj => 
        val thetaChunk = theta.bslice(jj)
        val rowChunk = row.bslice(jj)
        val red = reduce(jj.len)(0.0f){j => thetaChunk(j) * rowChunk(j)}{_+_}
        box(red)
      }{(a,b) => box(debox(a) + debox(b)) }

      val m = debox(mBox)
      val mult = y(i) - sigmoid(m)

      // Row calculation
      tileAssemble[Float,Array1D[Float],Array1D[Float]](C)( Array1D[Float](C) ){jj => jj}{jj => 
        val rowChunk = row.bslice(jj)
        collect(jj.len){j => rowChunk(j) * mult}
      }
    }{(a,b) =>
      tileAssemble[Float,Array1D[Float],Array1D[Float]](C)( Array1D[Float](C) ){jj => jj}{jj =>
        val aBlk = a.bslice(jj)
        val bBlk = b.bslice(jj)
        collect(jj.len){j => aBlk(j) + bBlk(j)}
      }
    }

    // Calculate new theta
    tileAssemble[Float,Array1D[Float],Array1D[Float]](C)( Array1D[Float](C) ){jj => jj}{jj => 
      val gradientBlk = gradient.slice(jj)
      val thetaBlk = theta.slice(jj)
      collect(jj.len){j => gradientBlk(j)*alpha + thetaBlk(j) }
    }
  }
}
