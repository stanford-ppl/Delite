import asplos._

trait BlurFrame extends PPLApp {
  def gaussianBlur(imgData: Rep[Array1D[Int]], H: Rep[Int], W: Rep[Int]): Rep[Array1D[Int]]
  def main() {
    val img = readImg(DATA_FOLDER + "conv/test_img.dat")

    // Zero pad input image
    // TODO: What to do for larger input images? What's the tradeoff between
    // branching on every apply and having larger input size?
    val imgPadded = collect(img.nRows + 8, img.nCols + 8){(i,j) => 
      if (i > 3 && j > 3 && i < img.nRows + 4 && j < img.nCols + 4)  
        img(i - 4, j - 4)
      else
        unit(0)
    }

    val blurData = gaussianBlur(imgPadded.data, img.nRows, img.nCols)
    val blur = Array2D(blurData, img.nRows, img.nCols)
    blur.slice(0 :@: 10, 0 :@: 10).pprint
  }
}

/* Approximation of Grayscale Gaussian Blur Filter (9x9, sigma = 2) */
object Blur extends PPLCompiler with BlurApp
object BlurFunc extends PPLCompiler with BlurApp {
  registerFunction(gaussianBlur _)
  override def functionName = "gaussianBlur"
}
trait BlurApp extends BlurFrame {
  def gaussianBlur(imgData: Rep[Array1D[Int]], H: Rep[Int], W: Rep[Int]): Rep[Array1D[Int]] = {
    // ---------- Tiling Hints -----------
    tile(H, tileSize = 40, max = 2000)
    tile(W, tileSize = 40, max = 1500)  
    // -----------------------------------
    val img = Array2D(imgData, H + 8, W + 8)
    val k = |(28, 66, 124, 180, 204, 180, 124, 66, 28)!

    // TODO: Requires blurring (kernel diameter - 1) extra empty lines
    // This is a bit awkward at the moment - the difference between H+8 and H means
    // we can't even theoretically fuse the outer collects
    val hBlur = collect(H+8,W){(i,j) => 
      val wndw = img.slice(i, j :@: 9)
      reduce(9)(0){x => wndw(x) * k(x) }{_+_}
    }
    val blur = collect(H,W){(i,j) =>
      val wndw = hBlur.slice(i :@: 9, j + 4)
      reduce(9)(0){y => wndw(y) * k(y) }{_+_}
    } 
    blur.data
  }
}

/* Manually blocked 9x9 Gaussian blur (sigma = 2) */
object BlurBlocked extends PPLCompiler with BlurBlockedApp
object BlurBlockedFunc extends PPLCompiler with BlurBlockedApp {
  registerFunction(gaussianBlur _)
  override def functionName = "gaussianBlur"
}
trait BlurBlockedApp extends BlurFrame {
  def gaussianBlur(imgData: Rep[Array1D[Int]], H: Rep[Int], W: Rep[Int]): Rep[Array1D[Int]] = {
    // ---------- Tiling Hints -----------
    tile(H, tileSize = 40, max = 2000)
    tile(W, tileSize = 40, max = 1500)  
    // -----------------------------------
    val img = Array2D(imgData, H + 8, W + 8)
    val k = |(28, 66, 124, 180, 204, 180, 124, 66, 28)!

    // TODO: Requires blurring 8 extra empty lines right now
    // FIXME: These bslices should definitely NOT be copies on the CPU!
    val hBlur = tileAssemble[Int,Array2D[Int],Array2D[Int]](H+8,W)( Array2D[Int](H+8,W) )({(ii,jj) => ii},{(ii,jj) => jj}){(ii,jj) =>
      val wndwBlk = img.bslice(ii, jj ++ 8).noteReuse(0, 8)

      collect(ii.len,jj.len){(i,j) => 
        val wndw = wndwBlk.slice(i, j :@: 9)
        reduce(9)(0){x => wndw(x) * k(x) }{_+_}
      }
    }
    val blur = tileAssemble[Int,Array2D[Int],Array2D[Int]](H,W)( Array2D[Int](H,W) )({(ii,jj) => ii},{(ii,jj) => jj}){(ii,jj) =>
      val wndwBlk = hBlur.bslice(ii ++ 8, jj + 4).noteReuse(8,0)
      
      collect(ii.len,jj.len){(i,j) =>
        val wndw = wndwBlk.slice(i :@: 9, j)
        reduce(9)(0){y => wndw(y) * k(y) }{_+_}
      }
    } 
    blur.data
  }
}
