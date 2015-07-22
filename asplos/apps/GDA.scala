import asplos._

trait GDAFrame extends PPLApp {
  def gdaSigma(xData: Rep[Array1D[Double]], y: Rep[Array1D[Boolean]], mu0: Rep[Array1D[Double]], mu1: Rep[Array1D[Double]]): Rep[Array1D[Double]]
  def main() {
    val x = read2D(DATA_FOLDER + "gda/1024-1200x.dat")
    val y = read(DATA_FOLDER + "gda/q1y.dat").map{e => e.toDouble > 0.0}

    val M = y.length  // Number of training samples 
    val N = x.nCols   // Dimensionality of training data
    println("N = " + N)
    println("M = " + M)

    val y_zeros = filterReduce(M)(0){  y(_) }{r => 1 }{_+_}
    val y_ones  = filterReduce(M)(0){ !y(_) }{r => 1 }{_+_}

    val zeroVec0 = collect(N){i => 0.0}.asView
    val zeroVec1 = collect(N){i => 0.0}.asView
    val mu0_num = filterReduce(M)(zeroVec0){  y(_) }{r => x.slice(r, *) }{(a,b) => collect(N){v => a(v) + b(v) }.asView }.notePhysViewOnly
    val mu1_num = filterReduce(M)(zeroVec1){ !y(_) }{r => x.slice(r, *) }{(a,b) => collect(N){v => a(v) + b(v) }.asView }.notePhysViewOnly

    val phi = 1.0 / M * y_ones
    val mu0 = collect(N){v => mu0_num(v) / y_zeros }
    val mu1 = collect(N){v => mu1_num(v) / y_ones }

    val sigmaData = gdaSigma(x.data, y, mu0, mu1)
    val sigma = Array2D(sigmaData, N, N)

    println("phi = " + phi)
    println("mu0 = "); mu0.slice(0 :@: 4).pprint
    println("mu1 = "); mu1.slice(0 :@: 4).pprint
    sigma.slice(0 :@: 4, 0 :@: 4).pprint
  } 
}

object GDA extends PPLCompiler with GDAApp
object GDAFunc extends PPLCompiler with GDAApp {
  registerFunction(gdaSigma _)
  override def functionName = "gdaSigma"
}
trait GDAApp extends GDAFrame {
  def gdaSigma(xData: Rep[Array1D[Double]], y: Rep[Array1D[Boolean]], mu0: Rep[Array1D[Double]], mu1: Rep[Array1D[Double]]): Rep[Array1D[Double]] = {
    val M = y.length
    val N = mu0.length
    // ---------- Tiling Hints -----------
    tile(M, tileSize = 40, max = 1024)
    tile(N, tileSize = 40, max = 1200)  
    // ----------------------------------- 

    val x = Array2D(xData, M, N)

    val sigma = reduce(M)( Array2D[Double](N,N) ){r => 
      val mu = if (y(r)) mu1 else mu0
      val row = x.slice(r, *)
      val sub = collect(N){i => row(i) - mu(i)}
      collect(N,N){(i,j) => sub(i) * sub(j) }
    
    }{(a,b) => collect(N,N){(i,j) => a(i,j) = a(i,j) + b(i,j) }; a }

    sigma.data
  }
}

/* Manually blocked GDA */
object GDABlocked extends PPLCompiler with GDABlockedApp
object GDABlockedFunc extends PPLCompiler with GDABlockedApp {
  registerFunction(gdaSigma _)
  override def functionName = "gdaSigma"
}
trait GDABlockedApp extends GDAFrame {
  def gdaSigma(xData: Rep[Array1D[Double]], y: Rep[Array1D[Boolean]], mu0: Rep[Array1D[Double]], mu1: Rep[Array1D[Double]]): Rep[Array1D[Double]] = {
    val M = y.length
    val N = mu0.length
    // ---------- Tiling Hints -----------
    tile(M, tileSize = 40, max = 1024)
    tile(N, tileSize = 40, max = 1200)  
    // ----------------------------------- 

    val x = Array2D(xData, M, N)

    // NOTE: Outer loop is not strip mined
    val sigma = reduce(M)( Array2D[Double](N,N) ){r => 
      val mu = if (y(r)) mu1 else mu0
      val row = x.slice(r, *)
      val sub = tileAssemble[Double,Array1D[Double],Array1D[Double]](N)( Array1D[Double](N) ){ii => ii}{ii => 
        val rowBlk = row.bslice(ii)
        val muBlk = mu.bslice(ii)
        collect(ii.len){i => rowBlk(i) - muBlk(i)}
      }
      tileAssemble[Double,Array2D[Double],Array2D[Double]](N,N)( Array2D[Double](N,N) )({(ii,jj) => ii}, {(ii,jj) => jj}){(ii,jj) => 
        val subBlkI = sub.bslice(ii)
        val subBlkJ = sub.bslice(jj)
        collect(ii.len,jj.len){(i,j) => subBlkI(i) * subBlkJ(j)}
      }
    }{(a,b) => 
      tileAssemble[Double,Array2D[Double],Array2D[Double]](N,N)( Array2D[Double](N,N) )({(ii,jj) => ii}, {(ii,jj) => jj}){(ii,jj) => 
        val aBlk = a.bslice(ii,jj)
        val bBlk = b.bslice(ii,jj)
        collect(ii.len,jj.len){(i,j) => aBlk(i,j) + bBlk(i,j) } 
      }
    }

    sigma.data
  }
}
