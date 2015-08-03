import asplos._

trait SumRowsFrame extends PPLApp {
  def sumRows(mData: Rep[Array1D[Float]], R: Rep[Int], C: Rep[Int]): Rep[Array1D[Float]]
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt}
    val R = dims(0)
    val C = dims(1)

    val mat = collect(R,C){(i,j) => ( C*i + j ).toFloat }

    val col = sumRows(mat.data, R, C)
    col.vprint 
  }
}

/* sumRows */
object SumRows extends PPLCompiler with SumRowsApp
object SumRowsFunc extends PPLCompiler with SumRowsApp {
  registerFunction(sumRows _)
  override def functionName = "sumRows"
}
trait SumRowsApp extends SumRowsFrame {
  def sumRows(mData: Rep[Array1D[Float]], R: Rep[Int], C: Rep[Int]): Rep[Array1D[Float]] = {
    // ---------- Tiling Hints -----------
    tile(R, tileSize = 35, max = ?)
    tile(C, tileSize = 35, max = ?)
    // -----------------------------------

    val mat = Array2D(mData, R, C)

    collect(R){r => 
      reduce(C)(0.0f){c => mat(r,c) }{_+_}
    }
  }  
}

/* Manually blocked sumRows */
object SumRowsBlocked extends PPLCompiler with SumRowsBlockedApp
object SumRowsBlockedFunc extends PPLCompiler with SumRowsBlockedApp {
  registerFunction(sumRows _)
  override def functionName = "sumRows"
}
trait SumRowsBlockedApp extends SumRowsFrame {
  def sumRows(mData: Rep[Array1D[Float]], R: Rep[Int], C: Rep[Int]): Rep[Array1D[Float]] = {
    // ---------- Tiling Hints -----------
    tile(R, tileSize = 35, max = ?)
    tile(C, tileSize = 35, max = ?)
    // -----------------------------------

    val mat = Array2D(mData, R, C)

    // Reduction must be along the smaller of the two, which is always the most recent tile output
    // (The accumulator is of some fixed, maximum size)
    tileReduce[Float,Array1D[Float],Array1D[Float]](R,C)(Array1D[Float](R)){(rr,cc) => rr}{(rr,cc) => 
      val matBlk = mat.bslice(rr, cc)
      // Tile is blockFactor(R)
      collect(rr.len){r => 
        reduce(cc.len)(0.0f){c => matBlk(r,c) }{_+_}
      }
    // Tile is partial column result of size blockFactor(R)
    }{(a,b) => collect(b.length){i => a(i) + b(i) } }
  }  
}
