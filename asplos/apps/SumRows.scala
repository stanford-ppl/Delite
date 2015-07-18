import asplos._

object SumRows extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt}
    val R = dims(0)
    val C = dims(1)

    val mat = collect(R,C){(i,j) => C*i + j}
    mat.pprint

    val col = collect(R){r => 
      reduce(C)(0){c => mat(r,c) }{_+_}
    }
    col.vprint 
  }
}

object SumRowsBlocked extends PPLCompiler {
  def main() {
    val dims = read(CONFIG_FILE).map{d => d.toInt}
    val R = dims(0)
    val C = dims(1)

    // --- Manually Blocked Dimensions ---
    tile(R, tileSize = 5, max = ?)
    tile(C, tileSize = 5, max = ?)
    // -----------------------------------

    // Could also block matrix creation, not done here
    val mat = collect(R,C){(i,j) => C*i + j}
    mat.pprint

    val col = tileReduce[Int,Array1D[Int],Array1D[Int]](R,C)(Array1D[Int](R)){(rr,cc) => rr}{(rr,cc) => 
      val matBlk = mat.bslice(rr, cc)
      collect(rr.len){r => 
        reduce(cc.len)(0){c => matBlk(r,c) }{_+_}
      }
    }{(a,b) => 
      // Reduction must be along the smaller of the two, which is always the most recent tile output
      // (The accumulator is of some fixed, maximum size)
      collect(b.length){i => a(i) + b(i)} 
    }

    col.vprint 
  }
}
