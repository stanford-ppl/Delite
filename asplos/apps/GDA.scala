import asplos._

trait GDAFrame extends PPLApp {
  def gdaSigma(xData: Rep[Array1D[Float]], y: Rep[Array1D[Boolean]], mu0: Rep[Array1D[Float]], mu1: Rep[Array1D[Float]]): Rep[Array1D[Float]]
  def main() {
    //val x = read2D(DATA_FOLDER + "gda/1024-1200x.dat")
    //val y = read(DATA_FOLDER + "gda/q1y.dat").map{e => e.toFloat > 0.0f}

    val data = read2D(DATA_FOLDER + "gda/eye_eeg.dat")
    val M = data.nRows
    val DC = data.nCols
    val N = DC - 1

    // Labels are in last column
    val x = data.bslice(*, 0 :@: N); val y = data.bslice(*, N).map{e => e > 0.0f}
    println("N = " + N)
    println("M = " + M)
    // ---------- Tiling Hints -----------
    tile(M, tileSize = 100, max = 15000)
    tile(N, tileSize = 20, max = 20)  
    tile(DC, tileSize = 20, max = 20) // TODO: Shouldn't need this
    // ----------------------------------- 

    val y_zeros = filterReduce(M)(0){  y(_) }{r => 1 }{_+_}
    val y_ones  = filterReduce(M)(0){ !y(_) }{r => 1 }{_+_}

    val zeroVec0 = Array1D[Float](N).asView
    val zeroVec1 = Array1D[Float](N).asView
    val mu0_num = filterReduce(M)(zeroVec0){  y(_) }{r => x.slice(r, *) }{(a,b) => collect(N){v => a(v) + b(v) }.asView }.notePhysViewOnly
    val mu1_num = filterReduce(M)(zeroVec1){ !y(_) }{r => x.slice(r, *) }{(a,b) => collect(N){v => a(v) + b(v) }.asView }.notePhysViewOnly

    val phi = 1.0f / M * y_ones
    val mu0 = collect(N){v => mu0_num(v) / y_zeros }
    val mu1 = collect(N){v => mu1_num(v) / y_ones }

    val sigmaData = gdaSigma(x.data, y, mu0, mu1)
    val sigma = Array2D(sigmaData, N, N)

    println("phi = " + phi)
    println("mu0 = "); mu0.pprint
    println("mu1 = "); mu1.pprint
    println("sigma = ")
    sigma.pprint
  } 
}

object GDA extends PPLCompiler with GDAApp
object GDAFunc extends PPLCompiler with GDAApp {
  registerFunction(gdaSigma _)
  override def functionName = "gdaSigma"
}
trait GDAApp extends GDAFrame {
  def gdaSigma(xData: Rep[Array1D[Float]], y: Rep[Array1D[Boolean]], mu0: Rep[Array1D[Float]], mu1: Rep[Array1D[Float]]): Rep[Array1D[Float]] = {
    val M = y.length
    val N = mu0.length
    // ---------- Tiling Hints -----------
    tile(M, tileSize = 100, max = 15000)
    tile(N, tileSize = 20, max = 20)  
    // ----------------------------------- 

    val x = Array2D(xData, M, N)

    val sigma = reduce(M)( Array2D[Float](N,N) ){r => 
      val mu = if (y(r)) mu1 else mu0
      val row = x.slice(r, *)
      val sub = collect(N){i => row(i) - mu(i)}
      collect(N,N){(i,j) => sub(i) * sub(j) }
    }{(a,b) => collect(N,N){(i,j) => a(i,j) + b(i,j) } }

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
  def gdaSigma(xData: Rep[Array1D[Float]], y: Rep[Array1D[Boolean]], mu0: Rep[Array1D[Float]], mu1: Rep[Array1D[Float]]): Rep[Array1D[Float]] = {
    val M = y.length
    val N = mu0.length
    // ---------- Tiling Hints -----------
    tile(M, tileSize = 100, max = 15000)
    tile(N, tileSize = 20, max = 20)  
    // ----------------------------------- 

    val x = Array2D(xData, M, N)

    // NOTE: Outer loop is not strip mined
    val sigma = reduce(M)( Array2D[Float](N,N) ){r => 
      val mu = if (y(r)) mu1 else mu0
      val row = x.slice(r, *)
      val sub = tileAssemble[Float,Array1D[Float],Array1D[Float]](N)( Array1D[Float](N) ){ii => ii}{ii => 
        val rowBlk = row.bslice(ii)
        val muBlk = mu.bslice(ii)
        collect(ii.len){i => rowBlk(i) - muBlk(i)}
      }
      tileAssemble[Float,Array2D[Float],Array2D[Float]](N,N)( Array2D[Float](N,N) )({(ii,jj) => ii}, {(ii,jj) => jj}){(ii,jj) => 
        val subBlkI = sub.bslice(ii)
        val subBlkJ = sub.bslice(jj)
        collect(ii.len,jj.len){(i,j) => subBlkI(i) * subBlkJ(j)}
      }
    }{(a,b) => 
      tileAssemble[Float,Array2D[Float],Array2D[Float]](N,N)( Array2D[Float](N,N) )({(ii,jj) => ii}, {(ii,jj) => jj}){(ii,jj) => 
        val aBlk = a.bslice(ii,jj)
        val bBlk = b.bslice(ii,jj)
        collect(ii.len,jj.len){(i,j) => aBlk(i,j) + bBlk(i,j) } 
      }
    }

    sigma.data
  }
}
