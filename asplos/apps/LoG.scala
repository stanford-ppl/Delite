import asplos._

trait LoGFrame extends PPLApp {
  def logFilter(imgData: Rep[Array1D[Int]], H: Rep[Int], W: Rep[Int]): Rep[Array1D[Int]]
  def main() {
    val img = readImg(DATA_FOLDER + "conv/test_img.dat")

    // Zero pad input image
    // TODO: What to do for larger input images? What's the tradeoff between
    // branching on every apply and having larger input size?
    val imgPadded = collect(img.nRows + 12, img.nCols + 12){(i,j) => 
      if (i > 5 && j > 5 && i < img.nRows + 6 && j < img.nCols + 6)  
        img(i - 6, j - 6)
      else
        unit(0)
    }

    val logData = logFilter(imgPadded.data, img.nRows, img.nCols)
    val log = Array2D(logData, img.nRows, img.nCols)
    log.slice(0 :@: 10, 0 :@: 10).pprint
  }
}

/* Laplacian of Gaussian approximation */
object LoG extends PPLCompiler with LoGApp
object LoGFunc extends PPLCompiler with LoGApp {
  registerFunction(logFilter _)
  override def functionName = "logFilter"
}
trait LoGApp extends LoGFrame {
  def logFilter(imgData: Rep[Array1D[Int]], H: Rep[Int], W: Rep[Int]): Rep[Array1D[Int]] = {
    // ---------- Tiling Hints -----------
    tile(H, tileSize = 40, max = 2000)
    tile(W, tileSize = 40, max = 1500)  
    // -----------------------------------
    val img = Array2D(imgData, H + 12, W + 12)
    val u = |(1, 1, 1, 1,  1, -1, -1, -1,  1, 1, 1, 1, 1)!
    val v = |(0, 1, 1, 0, -1, -3, -3, -3, -1, 0, 1, 1, 0)!

    // TODO: Requires blurring (kernel diameter - 1) extra empty lines
    // This is a bit awkward at the moment - the difference between H+8 and H means
    // we can't even theoretically fuse the outer collects
    val horz = collect(H+12,W){(i,j) => 
      val wndw = img.slice(i, j :@: 13)
      reduce(13)(0){x => wndw(x) * u(x) }{_+_}
    }
    val log = collect(H,W){(i,j) =>
      val wndw = horz.slice(i :@: 13, j + 6)
      reduce(13)(0){y => wndw(y) * v(y) }{_+_}
    } 
    log.data
  }
}

/* Manually blocked Laplacian of Gaussian */
object LoGBlocked extends PPLCompiler with LoGBlockedApp
object LoGBlockedFunc extends PPLCompiler with LoGBlockedApp {
  registerFunction(logFilter _)
  override def functionName = "logFilter"
}
trait LoGBlockedApp extends LoGFrame {
  def logFilter(imgData: Rep[Array1D[Int]], H: Rep[Int], W: Rep[Int]): Rep[Array1D[Int]] = {
    // ---------- Tiling Hints -----------
    tile(H, tileSize = 40, max = 2000)
    tile(W, tileSize = 40, max = 1500)  
    // -----------------------------------
    val img = Array2D(imgData, H + 12, W + 12)
    val u = |(1, 1, 1, 1,  1, -1, -1, -1,  1, 1, 1, 1, 1)!
    val v = |(0, 1, 1, 0, -1, -3, -3, -3, -1, 0, 1, 1, 0)!

    // TODO: Requires blurring 8 extra empty lines right now
    // FIXME: These bslices should definitely NOT be copies on the CPU!
    val horz = tileAssemble[Int,Array2D[Int],Array2D[Int]](H+12,W)( Array2D[Int](H+12,W) )({(ii,jj) => ii},{(ii,jj) => jj}){(ii,jj) =>
      val wndwBlk = img.bslice(ii, jj ++ 12).noteReuse(0,12)

      collect(ii.len,jj.len){(i,j) => 
        val wndw = wndwBlk.slice(i, j :@: 13)
        reduce(13)(0){x => wndw(x) * u(x) }{_+_}
      }
    }
    val log = tileAssemble[Int,Array2D[Int],Array2D[Int]](H,W)( Array2D[Int](H,W) )({(ii,jj) => ii},{(ii,jj) => jj}){(ii,jj) =>
      val wndwBlk = horz.bslice(ii ++ 12, jj + 6).noteReuse(12,0)
      
      collect(ii.len,jj.len){(i,j) =>
        val wndw = wndwBlk.slice(i :@: 13, j)
        reduce(13)(0){y => wndw(y) * v(y) }{_+_}
      }
    } 
    log.data
  }
}
