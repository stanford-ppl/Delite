import asplos._

trait MatmultFrame extends PPLApp {
  def matmult(aData: Rep[Array1D[Double]], bData: Rep[Array1D[Double]], R: Rep[Int], M: Rep[Int], C: Rep[Int]): Rep[Array1D[Double]]
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt}
    val R = dims(0)
    val M = dims(1)
    val C = dims(2)

    // ---------- Tiling Hints -----------
    tile(R, tileSize = 35, max = ?)
    tile(M, tileSize = 30, max = ?)
    tile(C, tileSize = 35, max = ?)  
    // ----------------------------------- 

    val a = collect(R,M){(i,j) => (i + j).toDouble }
    val b = collect(M,C){(i,j) => (i - j).toDouble }

    val cData = matmult(a.data, b.data, R, M, C)
    val c = Array2D(cData, R, C)
    
    c.pprint
  }
}

/* Matrix multiply (using parallel patterns) */
object Matmult extends PPLCompiler with MatmultApp 
object MatmultFunc extends PPLCompiler with MatmultApp {
  registerFunction(matmult _)
  override def functionName = "matmult"
}
trait MatmultApp extends MatmultFrame {
  def matmult(aData: Rep[Array1D[Double]], bData: Rep[Array1D[Double]], R: Rep[Int], M: Rep[Int], C: Rep[Int]): Rep[Array1D[Double]] = {
    // ---------- Tiling Hints -----------
    tile(R, tileSize = 35, max = ?)
    tile(M, tileSize = 30, max = ?)
    tile(C, tileSize = 35, max = ?)  
    // ----------------------------------- 

    val a = Array2D(aData, R, M)
    val b = Array2D(bData, M, C)
    val c = collect(R, C){(i,k) => 
      reduce(M)(0.0){j => a(i,j) * b(j,k)}{_+_}
    }
    c.data
  }
}

/* Manually blocked matrix multiply */
object MatmultBlocked extends PPLCompiler with MatmultBlockedApp
object MatmultBlockedFunc extends PPLCompiler with MatmultBlockedApp {
  registerFunction(matmult _)
  override def functionName = "matmult"
}
trait MatmultBlockedApp extends MatmultFrame {
  def matmult(aData: Rep[Array1D[Double]], bData: Rep[Array1D[Double]], R: Rep[Int], M: Rep[Int], C: Rep[Int]): Rep[Array1D[Double]] = {
    // ---------- Tiling Hints -----------
    tile(R, tileSize = 96, max = ?)
    tile(M, tileSize = 96, max = ?)
    tile(C, tileSize = 96, max = ?)
    // -----------------------------------

    val a = Array2D(aData, R, M)
    val b = Array2D(bData, M, C)
    // Change this 
    val c = tileReduce[Double,Array2D[Double],Array2D[Double]](R,C,M)(Array2D[Double](R,C))({(ii,kk,jj) => ii},{(ii,kk,jj) => kk}){(ii,kk,jj) => 
      val aBlk = a.bslice(ii,jj) // TODO: CPU should see this as a slice, not a copy
      val bBlk = b.bslice(jj,kk) // TODO: CPU should see this as a slice, not a copy
      collect(ii.len,kk.len){(i,k) => 
        reduce(jj.len)(0.0){j => aBlk(i,j) * bBlk(j,k)}{_+_}
      }
    }{(a,b) => collect(b.nRows,b.nCols){(i,k) => a(i,k) + b(i,k)} }

    c.data
  }
}
