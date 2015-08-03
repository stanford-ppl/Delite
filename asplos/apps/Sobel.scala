import asplos._

trait SobelFrame extends PPLApp {
  val sHorz = |(-1, 0, 1)|
              |(-2, 0, 2)|
              |(-1, 0, 1)|
  val sVert = |(-1, -2, -1)|
              |( 0,  0,  0)| 
              |( 1,  2,  1)|

  def sobelFilter(imgData: Rep[Array1D[Int]], H: Rep[Int], W: Rep[Int]): Rep[Array1D[Int]]
  def main() {
    val img = readImg(DATA_FOLDER + "conv/test_img.dat")

    val H = img.nRows
    val W = img.nCols
    val PH = H + 2
    val PW = W + 2

    // ---------- Tiling Hints -----------
    tile(H, tileSize = 40, max = 2000)
    tile(W, tileSize = 40, max = 1500)
    tile(PH, tileSize = 40, max = 2000) // TODO: These should be unnecessary
    tile(PW, tileSize = 40, max = 1500)
    // -----------------------------------

    val imgPadded = collect(PH, PW){(i,j) => 
      if (i > 0 && j > 0 && i <= H && j <= W)  
        img(i - 1, j - 1)
      else
        unit(0)
    }

    val edgeData = sobelFilter(imgPadded.data, H, W)
    val edges = Array2D(edgeData, H, W)
    edges.pprint
  }
}

/* Grayscale sobel filter */
object Sobel extends PPLCompiler with SobelApp
object SobelFunc extends PPLCompiler with SobelApp {
  registerFunction(sobelFilter _)
  override def functionName = "sobelFilter"
}
trait SobelApp extends SobelFrame {
  def sobelFilter(imgData: Rep[Array1D[Int]], H: Rep[Int], W: Rep[Int]): Rep[Array1D[Int]] = {
    // ---------- Tiling Hints -----------
    tile(H, tileSize = 40, max = 2000)
    tile(W, tileSize = 40, max = 1500)  
    // -----------------------------------
    val img = Array2D(imgData, H + 2, W + 2)

    val edges = collect(H,W){(i,j) => 
      val wndw = img.slice(i :@: 3, j :@: 3)
      // These could be fused. Not sure how to do that here
      // NOTE: Should not block these - they're already super tiny
      // (In fact, these should be unrolled without any trouble)
      val dX = reduce(3,3)(0){(y,x) => wndw(y,x) * sHorz(y,x) }{_+_}
      val dY = reduce(3,3)(0){(y,x) => wndw(y,x) * sVert(y,x) }{_+_}
      Math.abs(dX) + Math.abs(dY)
    }
    edges.data
  }
}

/* Manually blocked grayscale sobel filter */
object SobelBlocked extends PPLCompiler with SobelBlockedApp
object SobelBlockedFunc extends PPLCompiler with SobelBlockedApp {
  registerFunction(sobelFilter _)
  override def functionName = "sobelFilter"
}
trait SobelBlockedApp extends SobelFrame {
  def sobelFilter(imgData: Rep[Array1D[Int]], H: Rep[Int], W: Rep[Int]): Rep[Array1D[Int]] = {
    // ---------- Tiling Hints -----------
    tile(H, tileSize = 40, max = 2000)
    tile(W, tileSize = 40, max = 1500)  
    // -----------------------------------
    val img = Array2D(imgData, H + 2, W + 2)

    val edges = tileAssemble[Int,Array2D[Int],Array2D[Int]](H,W)( Array2D[Int](H,W) )({(ii,jj) => ii}, {(ii,jj) => jj}){(ii,jj) => 
      val wndwBlk = img.bslice(ii ++ 2, jj ++ 2).noteReuse(2,2)

      collect(ii.len,jj.len){(i,j) => 
        val wndw = wndwBlk.slice(i :@: 3, j :@: 3)
        val dX = reduce(3,3)(0){(y,x) => wndw(y,x) * sHorz(y,x) }{_+_}
        val dY = reduce(3,3)(0){(y,x) => wndw(y,x) * sVert(y,x) }{_+_}
        Math.abs(dX) + Math.abs(dY)
      }
    }
    edges.data
  }
}
