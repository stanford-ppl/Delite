import asplos._

trait OuterProductFrame extends PPLApp {
  def vectorOuterProduct(a: Rep[Array1D[Float]], b: Rep[Array1D[Float]]): Rep[Array1D[Float]]
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt}
    val R = dims(0)
    val C = dims(1)
    // ---------- Tiling Hints -----------
    tile(R, tileSize = 40, max = ?)
    tile(C, tileSize = 40, max = ?)  
    // ----------------------------------- 

    val a = collect(R){i => (i + 3).toFloat }
    val b = collect(C){j => (j * 5).toFloat }

    val cData = vectorOuterProduct(a, b)
    val c = Array2D(cData, R, C)
    c.pprint
  }
}

/* Outer product */
object OuterProduct extends PPLCompiler with OuterProductApp
object OuterProductFunc extends PPLCompiler with OuterProductApp {
  registerFunction(vectorOuterProduct _)
  override def functionName = "vectorOuterProduct"
}
trait OuterProductApp extends OuterProductFrame {
  def vectorOuterProduct(a: Rep[Array1D[Float]], b: Rep[Array1D[Float]]): Rep[Array1D[Float]] = {
    val R = a.length
    val C = b.length
    // ---------- Tiling Hints -----------
    tile(R, tileSize = 40, max = ?)
    tile(C, tileSize = 40, max = ?)  
    // ----------------------------------- 

    val c = collect(R,C){(i,j) => a(i) * b(j)}
    c.data
  }
}

/* Manually blocked outer product */
object OuterProductBlocked extends PPLCompiler with OuterProductBlockedApp
object OuterProductBlockedFunc extends PPLCompiler with OuterProductBlockedApp {
  registerFunction(vectorOuterProduct _)
  override def functionName = "vectorOuterProduct"
}
trait OuterProductBlockedApp extends OuterProductFrame {
  def vectorOuterProduct(a: Rep[Array1D[Float]], b: Rep[Array1D[Float]]): Rep[Array1D[Float]] = {
    val R = a.length
    val C = b.length
    // ---------- Tiling Hints -----------
    tile(R, tileSize = 40, max = ?)
    tile(C, tileSize = 40, max = ?)  
    // ----------------------------------- 

    val c = tileAssemble[Float,Array2D[Float],Array2D[Float]](R,C)(Array2D[Float](R,C))({(ii,jj) => ii}, {(ii,jj) => jj}){(ii,jj) => 
      val aBlk = a.bslice(ii)
      val bBlk = b.bslice(jj)
      collect(ii.len,jj.len){(i,j) => aBlk(i) * bBlk(j) }
    }
    c.data
  }
}