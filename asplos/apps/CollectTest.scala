import asplos._

object CollectTestLiteCompiler extends PPLCompiler with CollectTest
trait CollectTest extends PPLApp {
  def main() {
    val dims = read("/home/david/PPL/data/arr1d.txt").map{d => d.toInt}
    val d0 = dims(0)
    val d1 = dims(1)
    collect(d0,d1){(i,j) => i + j + 10}.pprint
  }
}

object CollectTestBlockedLiteCompiler extends PPLCompiler with CollectTestBlocked
trait CollectTestBlocked extends PPLApp {
  def main() {
    val dims = read("/home/david/PPL/data/arr1d.txt").map{d => d.toInt}
    val d0 = dims(0)
    val d1 = dims(1)

    // Pre-block dimensions since there isn't anything to do that in the compiler yet
    blockSize(d0 -> 5)
    blockSize(d1 -> 5)

    tileAssemble2D[Int,Array2D[Int],Array2D[Int]](d0,d1)(collect(d0,d1){(i,j) => 0})({(ii,jj) => ii}, {(ii,jj) => jj}){(ii,jj) => 
      collect(ii.len, jj.len){(i,j) => ii.start + jj.start + i + j + 10}
    }.pprint
  }
}

