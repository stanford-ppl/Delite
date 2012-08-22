package ppl.apps.ml.cf

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object TopNRunner extends OptiMLApplicationRunner with TopN

trait TopN extends OptiMLApplication {
  def printUsage = {
    println("Usage: TopN <input data file> <user> <number of recommendations>")
    exit(-1)
  }
  
  /**
   * Application-specific data structures
   */
  type PairwiseRating = Record{val profileA: Int; val profileB: Int; val scoreA: Int; val scoreB: Int}  
  def NewPairwiseRating(pa: Rep[Int], pb: Rep[Int], sa: Rep[Int], sb: Rep[Int]) = new Record {
    val profileA = pa
    val profileB = pb
    val scoreA = sa
    val scoreB = sb
  }
  
  type Similarity = Record{val a: Int; val b: Int; val value: Double}
  def NewSimilarity(sa: Rep[Int], sb: Rep[Int], sv: Rep[Double]) = new Record {
    val a = sa
    val b = sb
    val value = sv
  }  
  
  /**
   * Pearson correlation 
   */
  def similarity(ratings: Rep[DenseVector[PairwiseRating]]) = {    
    val avgA = (sum(0, ratings.length) { ratings(_).scoreA }).AsInstanceOf[Double] / ratings.length
    val avgB = (sum(0, ratings.length) { ratings(_).scoreB }).AsInstanceOf[Double] / ratings.length
    
    def diffA(i: Rep[Int]) = ratings(i).scoreA - avgA
    def diffB(i: Rep[Int]) = ratings(i).scoreB - avgB
    
    val num = sum(0, ratings.length) { i => diffA(i)*diffB(i) }
    val den = sqrt(sum(0, ratings.length) { i => diffA(i)*diffA(i) }) * sqrt(sum(0, ratings.length) { i => diffB(i)*diffB(i) })
    
    // var i = 0
    // if (ratings.length > 1) {
    // while (i < ratings.length) {
    //   println("  ***" + ratings(i).profileA + ", " + ratings(i).profileB + ", [ " + ratings(i).scoreA + ", " + ratings(i).scoreB + "]")
    //   i += 1
    // }
    // println("num is " + num)
    // }
    
    NewSimilarity(ratings(0).profileA, ratings(0).profileB, num/(den+1))
  }
  
  def preferences(user: Rep[Int], ratings: Rep[DenseMatrix[Int]], sims: Rep[SparseMatrix[Double]]) = {
    // should this be sims.rowIndices { ... } that returns a SparseVector? (might be more expressive, but also slower)
    sims.nzRowIndices { testProfile => // each row is a unique profile
      val num = sum(0, ratings.numRows) { i => sims(testProfile, ratings(i,1))*ratings(i,2) }
      val den = sum(0, ratings.numRows) { i => abs(sims(testProfile, ratings(i,1))) }
      num/(den+1)
    }
  }
  
  def main() = {
    if (args.length < 3) printUsage

    // each row is in the format (userID, profileID, rating)    
    val data = readMatrix(args(0), ",") map { e => e.AsInstanceOf[Int] }     
    val testUser = Integer.parseInt(args(1))
    val N = Integer.parseInt(args(2))
    
    println("..finished loading data, running cf algorithm")
    tic()
            
    /*
     * find all co-occurred ratings and compute n x n similarity matrix,
     * where n is the number of unique elements in the co-occurred rating    
     */
    val userRatings = data groupRowsBy { row => row(0) } // group by user id    
    
    // println("userRatings: ")
    // userRatings.pprint
    // userRatings foreach { _.pprint }
        
    val pairwiseRatings = userRatings flatMap { submat =>
      
      // aggregateIf(0::submat.numRows, 0::submat.numRows) ((i,j) => i < j) { (i,j) =>
      aggregate (utriangle(submat.numRows,false)) { (i,j) => 
        // canonical ordering, smaller profile id always comes first in the pair
        // so (10,20) and (20,10) are both stored at (10,20)
        val a = submat(i,1)
        val b = submat(j,1)
        if (a < b) {
          NewPairwiseRating(a,b,submat(i,2),submat(j,2))
        }
        else {
          NewPairwiseRating(b,a,submat(j,2),submat(i,2))
        }                
      }
    } 
    
    // println("Pairwise ratings:")
    // pairwiseRatings.pprint
        
    /* similarity matrix */
    val uniqueRatings = pairwiseRatings groupBy { rating => (rating.profileA, rating.profileB) } // Vector[Vector[PairwiseRating]]        
    val similarities = uniqueRatings map { similarity } // Vector[Similarity]  
    // var n = 0
    // while (n < similarities.length) {
    //   println("  (" + similarities(n).a + ", " + similarities(n).b + ") = " + similarities(n).value)
    //   n += 1
    // }  
    println("..found " + similarities.length + " similarities")
    val userIds = (uniqueRatings map { grp => grp(0).profileA }) ++ (uniqueRatings map { grp => grp(0).profileB })

    // each similarity represents a unique (i,j) value in the similarity matrix - invert for fast look-up
    
    //val similarityMatrix = SymmetricMatrix[Double](userIds.max+1) // n x n where n is the number of unique users
    val similarityMatrixB = SparseMatrix[Double](userIds.max+1, userIds.max+1)
    // AKS TODO: for loops should check for sparse matrix updates inside and throw an error, as this will never work as expected
    // similarityMatrixB(IndexVector(similarities map { _.a }),IndexVector(similarities map { _.b })) = similarities map { _.value } 
    var s = 0
    while (s < similarities.length) {
      val sim = similarities(s)
      similarityMatrixB(sim.a, sim.b) = sim.value  
      similarityMatrixB(sim.b, sim.a) = sim.value 
      s += 1
    }
    val similarityMatrix = similarityMatrixB.finish
    
    /* compute prediction for desired user */
    val testRatings = userRatings filter { m => m(0,0) == testUser } // pretty inefficient unless this is fused...
    if (testRatings.length != 1) {
      println("CF error: could not find specified user")
      exit
    }
    
    println("..computing user preferences")
    val prefs = preferences(testUser, testRatings(0), similarityMatrix) // Vector[Double] from 0 to nnzRows representing testUser's preference for profile sims.nzRowIndices(i)
    // println("nzRowIndices: ")
    // similarityMatrix.nzRowIndices.pprint    
    // println("preferences: ")
    // prefs.pprint
    
    /* top N */    
    val (sorted, sortedIndices) = prefs.sortWithIndex
    val nzIndices = sortedIndices filter { i => prefs(i) != 0 } // drop entries we don't have data for
    val first = if (nzIndices.length - N > 0) nzIndices.length-N else 0
    val topN = nzIndices drop first
    val topScores = prefs(topN)
    val topUsers = topN map { i => similarityMatrix.nzRowIndices(i) }
        
    // println("sortedIndices: ")
    // sortedIndices.pprint
    // println("nzIndices: ")
    // nzIndices.pprint
    // println("topN: ")
    // topN.pprint
    // println("topScores: ")
    // topScores.pprint
    // println("topUsers: ")
    // topUsers.pprint
    
    toc(topUsers)
    
    println("top N matches for user " + testUser + ": ")
    var i = topUsers.length - 1
    while (i >= 0) {
      println("  match: " + topUsers(i) + ", score: " + topScores(i))
      i -= 1
    }      
  }
}
  
  
  
