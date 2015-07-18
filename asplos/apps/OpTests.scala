import asplos._

// Simple 1D collect
object Collect1DTest extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt} // Set in PPL.scala
    val d0 = dims(0)
    collect(d0){i => i + 10}.pprint
  }
}

// Simple blocked 1D collect
object Collect1DTestBlocked extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt} // Set in PPL.scala
    val d0 = dims(0)

    // --- Manually Blocked Dimensions ---
    tile(d0, tileSize = 5, max = ?)
    // -----------------------------------
  
    tileAssemble[Int,Array1D[Int],Array1D[Int]](d0)( Array1D[Int](d0) ){ii => ii}{ii => 
      collect(ii.len){i => ii.start + i + 10}
    }.pprint
  }
}

// Simple 2D collect with no inputs
object Collect2DTest extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt} // Set in PPL.scala
    val d0 = dims(0)
    val d1 = dims(1)
    collect(d0,d1){(i,j) => i + j + 10}.pprint
  }
}

// Simple blocked 2D collect
object Collect2DTestBlocked extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt} // Set in PPL.scala
    val d0 = dims(0)
    val d1 = dims(1)

    // --- Manually Blocked Dimensions ---
    tile(d0, tileSize = 5, max = ?)
    tile(d1, tileSize = 5, max = ?)
    // -----------------------------------

    tileAssemble[Int,Array2D[Int],Array2D[Int]](d0,d1)( Array2D[Int](d0,d1) )({(ii,jj) => ii}, {(ii,jj) => jj}){(ii,jj) => 
      collect(ii.len, jj.len){(i,j) => ii.start + jj.start + i + j + 10}
    }.pprint
  }
}

// Simple 1D Reduce
object Reduce1DTest extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt} // Set in PPL.scala
    val d0 = dims(0)
    val x = reduce(d0)(0){i => i}{_+_}
    println("0 + 1 + ... + " + d0 + " = " + x)
  }
}

object Reduce1DTestBlocked extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt} // Set in PPL.scala
    val d0 = dims(0)

    // --- Manually Blocked Dimensions ---
    tile(d0, tileSize = 5, max = ?)
    // -----------------------------------

    val xBoxed = tileReduce[Int,Array1D[Int],Array1D[Int]](d0)( Array1D[Int](1) ){ii => 0 :@: 1}{ii =>
      box( reduce(ii.len)(0){i => ii.start + i}{_+_} )
    }{(a,b) => box( debox(a) + debox(b) ) }

    val x = debox(xBoxed)

    println("0 + 1 + ... + " + d0 + " = " + x)
  }
}


// Simple 2D Reduce 
object Reduce2DTest extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt} // Set in PPL.scala
    val d0 = dims(0)
    val d1 = dims(1)

    val x = reduce(d0,d1)(0){(i,j) => i + j + 10}{_+_}
    println("result = " + x)
  }
}

object Reduce2DTestBlocked extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt} // Set in PPL.scala
    val d0 = dims(0)
    val d1 = dims(1)
    
    // --- Manually Blocked Dimensions ---
    tile(d0, tileSize = 5, max = ?)
    tile(d1, tileSize = 5, max = ?)
    // -----------------------------------

    val xBoxed = tileReduce[Int,Array1D[Int],Array1D[Int]](d0,d1)( Array1D[Int](1) ){(ii,jj) => 0 :@: 1}{(ii,jj) => 
      box( reduce(ii.len,jj.len)(0){(i,j) => ii.start + jj.start + i + j + 10}{_+_} )
    }{(a,b) => box( debox(a) + debox(b) ) }

    val x = debox(xBoxed)
    
    println("result = " + x)
  }
}


// Note: Not a true blocked collectCols operation (true one would have an inner tileAssemble)
object CollectColsBlocked extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt} // Set in PPL.scala
    val d0 = dims(0)
    val d1 = dims(1)

    // --- Manually Blocked Dimensions ---
    tile(d0, tileSize = 5, max = ?)
    tile(d1, tileSize = 1, max = ?)
    // -----------------------------------

    tileAssemble[Int,Array1D[Int],Array2D[Int]](d0,d1)( Array2D[Int](d0,d1) )({(ii,jj) => ii}, {(ii,jj) => jj.start :@: 1}){(ii,jj) => 
      collect(ii.len){i => ii.start + i + 10}
    }.pprint
  }
}

object BlockSliceTest extends PPLCompiler {
  def main() = {
    println("1D")
    val arr = collect(10){i => i + 3}
    arr.pprint
    val arrBlk = arr.bslice(3 :@: 3)
    arrBlk.pprint

    println("\n1D view")
    val arrv = collect(10){i => i + 5}.slice(2 :@: 8)
    arrv.pprint
    val arrvBlk = arrv.bslice(4 :@: 2)
    arrvBlk.pprint

    println("\n2D")
    val mat = collect(10,10){(i,j) => i*10 + j + 100}
    mat.pprint
    println("")
    val matRow = mat.bslice(1, 2 :@: 5)
    matRow.pprint
    println("")
    val matCol = mat.bslice(4 :@: 4, 3)
    matCol.pprint
    println("")
    val matBlk = mat.bslice(3 :@: 4, 5 :@: 2)
    matBlk.pprint

    println("\n2D view")
    val matv = collect(100,100){(i,j) => i*100 + j + 100}.slice(50 :@: 10, 30 :@: 10)
    matv.pprint
    println("")
    val matvRow = matv.bslice(1, 2 :@: 5)
    matvRow.pprint
    println("")
    val matvCol = matv.bslice(4 :@: 4, 3)
    matvCol.pprint
    println("")
    val matvBlk = matv.bslice(3 :@: 4, 5 :@: 2)
    matvBlk.pprint
  }
}

object SliceTest extends PPLCompiler {
  def main() = {
    println("1D")
    val arr = collect(10){i => i + 3}
    arr.pprint
    val arrBlk = arr.slice(3 :@: 3)
    arrBlk.pprint

    println("\n1D view")
    val arrv = collect(10){i => i + 5}.slice(2 :@: 8)
    arrv.pprint
    val arrvBlk = arrv.slice(4 :@: 2)
    arrvBlk.pprint

    println("\n2D")
    val mat = collect(10,10){(i,j) => i*10 + j + 100}
    mat.pprint
    println("")
    val matRow = mat.slice(1, 2 :@: 5)
    matRow.pprint
    println("")
    val matCol = mat.slice(4 :@: 4, 3)
    matCol.pprint
    println("")
    val matBlk = mat.slice(3 :@: 4, 5 :@: 2)
    matBlk.pprint

    println("\n2D view")
    val matv = collect(100,100){(i,j) => i*100 + j + 100}.slice(50 :@: 10, 30 :@: 10)
    matv.pprint
    println("")
    val matvRow = matv.slice(1, 2 :@: 5)
    matvRow.pprint
    println("")
    val matvCol = matv.slice(4 :@: 4, 3)
    matvCol.pprint
    println("")
    val matvBlk = matv.slice(3 :@: 4, 5 :@: 2)
    matvBlk.pprint
  }
}