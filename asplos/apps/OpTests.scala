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

object FilterTest extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{_.toInt}
    val d0 = dims(0)

    val vec = collect(d0){i => i}
    vec.pprint

    val filt = filter(d0){i => vec(i) > 3}{i => vec(i)}
    filt.pprint
    println("Filtered length: " + filt.length)
  }
}

object FilterTestBlocked extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{_.toInt}
    val d0 = dims(0)

    // --- Manually Blocked Dimensions ---
    tile(d0, tileSize = 5, max = ?)
    // -----------------------------------

    val vec = collect(d0){i => i}
    vec.pprint

    val filt = tiledFilter(d0){ii =>
      val vecBlk = vec.bslice(ii)
      filter(ii.len){i => vecBlk(i) > 3}{i => vecBlk(i)}
    }
    filt.pprint
    println("Filtered length: " + filt.length)
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

// Push slice operation into if-statement at same level - doesn't really help anything here
object SlicePushTest1 extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt} // Set in PPL.scala
    val d0 = dims(0)
    val sz = Math.min(d0, 5)
    val c  = dims(1)

    val vec0 = collect(d0){i => i + 5}
    val vec1 = collect(d0){i => 5 - i}

    val vec = if (c > 10) vec1 else vec0

    val vecSlice = vec.bslice(0 :@: sz)
    vecSlice.pprint
  }
}

// Push slice into if statement, resulting in slice getting pulled out of loop
object SlicePushTest2 extends PPLCompiler {
  def main() { 
    val dims = read(CONFIG_FILE).map{d => d.toInt} // Set in PPL.scala
    val N = dims(0)
    val D = Math.min(N, 5)
    val R = dims(1)

    val y = collect(R){i => i}
    val vec0 = collect(N){i => i}
    val vec1 = collect(N){i => 5 - i}

    vec0.pprint
    vec1.pprint

    val sm = blockAssem[Int,Array1D[Int],Array2D[Int]](R)(b0 = 1)(Array2D[Int](R,D))({ii => ii},{ii => 0 :@: D}){ii =>
      val vec = if (y(ii.start) > 10) vec1 else vec0
      vec.bslice(0 :@: D)
    }
    sm.pprint
  }
}

// Same as 2, but if statement is outside of loop to begin with
object SlicePushTest3 extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt} // Set in PPL.scala
    val N = dims(0)
    val D = Math.min(N, 5)
    val R = dims(1)

    val y = collect(R){i => i}
    val vec0 = collect(N){i => i}
    val vec1 = collect(N){i => 5 - i}

    vec0.pprint
    vec1.pprint

    val vec = if (y(0) > 10) vec1 else vec0
    val sm = blockAssem[Int,Array1D[Int],Array2D[Int]](R)(b0 = 1)(Array2D[Int](R,D))({ii => ii},{ii => 0 :@: D}){ii =>
      vec.bslice(0 :@: D)
    }
    sm.pprint
  }
}

object SlicePushTest4 extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt} // Set in PPL.scala
    val N = dims(0)
    val D = Math.min(N, 5)
    val R = dims(1)

    val y = collect(R){i => i}
    val vec0 = collect(N){i => i}
    val vec1 = collect(N){i => 5 - i}

    vec0.pprint
    vec1.pprint

    val vec = if (y(0) > 10) vec1 else vec0
    val sm = blockAssem[Int,Array1D[Int],Array2D[Int]](R,N)(b0 = 1, b1 = D)(Array2D[Int](R,N))({(ii,jj) => ii},{(ii,jj) => jj}){(ii,jj) =>
      vec.bslice(jj)
    }
    sm.pprint
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