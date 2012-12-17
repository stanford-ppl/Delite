package ppl.dsl.optiml.baseline

import LanguageDefs._

object PrefsTest {
  
  def main(args: Array[String]) = {
    def preferences(user: Int, ratings: Matrix[Int], sims: Matrix[Double]) = {
      sims.mapRowsToVector { testProfile => // each row is a unique profile
        val num = sum(0, ratings.numRows) { i => testProfile(ratings(i,1))*ratings(i,2) }
        val den = sum(0, ratings.numRows) { i => abs(testProfile(ratings(i,1))) }
        num/(den+1)
      }
    }
    
    var runs = 0
    while (runs < 10) {
      println("run: " + runs)
      
    val simsB = SparseMatrix[Double](10000,10000)
    var i = 0
    while (i < simsB.numRows) {
      if (random() < .05) {
        var j = 0
        while (j < simsB.numCols) {
          if (random() < .05) {
            simsB(i,j) = random()*1000
          }
          j += 1
        }
      }
      i += 1
    }
    val sims = simsB//.finish
    println("simz.nnz: " + simsB._nnz)
  
    val ratingsB = DenseMatrix[Int](0,0)
    ratingsB.insertCol(0, Vector.range(0,10000)) // user ids
    ratingsB.insertCol(1, Vector.rand(10000).map(e=>(e*1000).asInstanceOf[Int])) // rating user id    
    ratingsB.insertCol(2, Vector.rand(10000).map(e=>(e*10).asInstanceOf[Int])) // rating, 0 to 10
    val ratings = ratingsB
    // println("ratings: ")// + p.asInstanceOf[SparseVector[Any]]._data.mkString(","))
    // for (i <- ratings.numRows-10 until ratings.numRows) {
    //   println("[" + ratings(i,0) + "," + ratings(i,1) + "," + ratings(i,2) + "]")
    // }    

    tic()
    val p = preferences(0, ratings, sims)
    toc()
    println("p.nnz: " + p.asInstanceOf[SparseVector[Any]]._nnz)    
    // println("p: ")// + p.asInstanceOf[SparseVector[Any]]._data.mkString(","))
    // for (i <- p.length-10 until p.length) {
    //   println("[" + p(i) + "]")
    // }
    
    runs += 1
  }  
  }
}
