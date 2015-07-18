import asplos._

/*object OuterProductApp extends PPLCompiler {
  def main() {
    val dims = read("/home/david/PPL/hyperdsl/delite/asplos/apps/config.txt").map{d => d.toInt}
    val R = dims(0)
    val C = dims(2)

    val a = collect(R){i => i + 3}
    val b = collect(C){j => j * 5}

    collect(R,C){(i,j) => a(i) * b(j)}.pprint
  }
}

object OuterProductBlockedApp extends PPLCompiler {
  def main() {
    val dims = read("/home/david/PPL/hyperdsl/delite/asplos/apps/config.txt").map{d => d.toInt}
    val R = dims(0)
    val C = dims(2)

    val a = collect(R){i => i + 3}
    val b = collect(C){j => j * 5}

    // --- Manually Blocked Dimensions ---
    blockSize(R -> 5)
    blockSize(C -> 5)
    // -----------------------------------

    tileAssemble2D[Int,Array2D[Int],Array2D[Int]](R,C)(Array2D[Int](R,C))({(ii,jj) => ii}, {(ii,jj) => jj}){(ii,jj) => 
      
    }

    collect(R,C){(i,j) => a(i) * b(j)}.pprint
  }
}*/