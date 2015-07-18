import asplos._

object Matmult extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt}
    val R = dims(0)
    val M = dims(1)
    val C = dims(2)

    val a = collect(R,M){(i,j) => (i + j).toDouble }
    val b = collect(M,C){(i,j) => (i - j).toDouble }

    val c = collect(R,C){(i,k) => 
      reduce(M)(0.0){j => a(i,j) * b(j,k)}{_+_}
    }
    c.slice(0:@:5,0:@:5).pprint
  }
}

object MatmultBlocked extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt}
    val R = dims(0)
    val M = dims(1)
    val C = dims(2)

    // --- Manually Blocked Dimensions ---
    tile(R, tileSize = 50, max = ?)
    tile(M, tileSize = 50, max = ?)
    tile(C, tileSize = 50, max = ?)
    // -----------------------------------

    val a = collect(R,M){(i,j) => (i + j).toDouble }
    val b = collect(M,C){(i,j) => (i - j).toDouble }

    val c = tileMReduce[Double,Array2D[Double],Array2D[Double]](R,C,M)(Array2D[Double](R,C))({(ii,kk,jj) => ii},{(ii,kk,jj) => kk}){(ii,kk,jj) => 
      val aBlk = a.slice(ii,jj)
      val bBlk = b.slice(jj,kk)
      collect(ii.len,kk.len){(i,k) => 
        reduce(jj.len)(0.0){j => aBlk(i,j) * bBlk(j,k)}{_+_}
      }
    }{(a,b) => forIndices(b.nRows,b.nCols){(i,k) => a(i,k) = a(i,k) + b(i,k)}; a }

    c.slice(0:@:5, 0:@:5).pprint
  }
}