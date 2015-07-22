import asplos._

trait SumRowsFrame extends PPLApp {
  def sumRows(mData: Rep[Array1D[Double]], R: Rep[Int], C: Rep[Int]): Rep[Array1D[Double]]
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt}
    val R = dims(0)
    val C = dims(1)

    val mat = collect(R,C){(i,j) => ( C*i + j ).toDouble }

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
  def sumRows(mData: Rep[Array1D[Double]], R: Rep[Int], C: Rep[Int]): Rep[Array1D[Double]] = {
    // ---------- Tiling Hints -----------
    tile(R, tileSize = 35, max = ?)
    tile(C, tileSize = 35, max = ?)
    // -----------------------------------

    val mat = Array2D(mData, R, C)

    collect(R){r => 
      reduce(C)(0.0){c => mat(r,c) }{_+_}
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
  def sumRows(mData: Rep[Array1D[Double]], R: Rep[Int], C: Rep[Int]): Rep[Array1D[Double]] = {
    // ---------- Tiling Hints -----------
    tile(R, tileSize = 35, max = ?)
    tile(C, tileSize = 35, max = ?)
    // -----------------------------------

    val mat = Array2D(mData, R, C)

    // Reduction must be along the smaller of the two, which is always the most recent tile output
    // (The accumulator is of some fixed, maximum size)
    tileReduce[Double,Array1D[Double],Array1D[Double]](R,C)(Array1D[Double](R)){(rr,cc) => rr}{(rr,cc) => 
      val matBlk = mat.bslice(rr, cc)
      collect(rr.len){r => 
        reduce(cc.len)(0.0){c => matBlk(r,c) }{_+_}
      }
    }{(a,b) => 
      tileAssemble[Double,Array1D[Double],Array1D[Double]](C)(Array1D[Double](C)){cc => cc}{cc =>
        collect(cc.len){i => a(i) + b(i) }
      }
    }
  }  
}
