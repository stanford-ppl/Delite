package ppl.apps.ml.cf

import ppl.dsl.optiml._
import ppl.dsl.optiml.application.{Similarity, PairwiseRating}
import ppl.delite.framework.DeliteApplication

object TopNRunner extends OptiMLApplicationRunner with TopN

trait TopN extends OptiMLApplication {
  def printUsage = {
    println("Usage: TopN <input data file> <user> <number of recommendations>")
    exit(-1)
  }

  /**
   * Pearson correlation 
   */
  def similarity(ratings: Rep[DenseVector[PairwiseRating]]) = {    
    val avgA = sum(0, ratings.length) { ratings(_).scoreA } / ratings.length
    val avgB = sum(0, ratings.length) { ratings(_).scoreB } / ratings.length

    def diffA(i: Rep[Int]) = ratings(i).scoreA - avgA
    def diffB(i: Rep[Int]) = ratings(i).scoreB - avgB
    
    val num = sum(0, ratings.length) { i => diffA(i)*diffB(i) }
    val den = sqrt(sum(0, ratings.length) { diffA(_) }) * sqrt(sum(0, ratings.length) { diffB(_) })
    
    Similarity(ratings(0).profileA, ratings(0).profileB, num/den)
  }
  
  def preferences(user: Rep[Int], ratings: Rep[DenseMatrix[Int]], sims: Rep[DenseMatrix[Double]]) = {
    (0::sims.numRows) { testProfile => // each row is a unique profile
      val num = sum(0, ratings.numRows) { i => sims(testProfile, ratings(i,1))*ratings(i,2) }
      val den = sum(0, ratings.numRows) { i => abs(sims(testProfile, ratings(i,1))) }
      num/den
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
      
      // this is testing way too many combinations
      // need something equivalent to:
      // for (i <- 0 until submat.numRows) {
      //   for (j <- i+1 until submat.numRows) {
      //}}
      // since we're in the outer flatMap, could do this sequentially, but how do we express it?
      // aggregateIf(i@0::submat.numRows, i::submat.numRows) --> then could still represent as a flattened multiloop internally
      // another option is to try to optimize the multiloop + conditional, but we would need a 2d multiloop representation
      
      // another option is to represent this as a triangular matrix explicitly, and use a bulk operation on that
      aggregateIf(0::submat.numRows, 0::submat.numRows) ((i,j) => i < j) { (i,j) =>
        // canonical ordering, smaller profile id always comes first in the pair
        // so (10,20) and (20,10) are both stored at (10,20)
        val a = submat(i,1)
        val b = submat(j,1)
        if (a < b) {
          PairwiseRating(a,b,submat(i,2),submat(j,2))
        }
        else {
          PairwiseRating(b,a,submat(j,2),submat(i,2))
        }
      }
    } 
    
    // println("Pairwise ratings:")
    // pairwiseRatings.pprint
        
    /* similarity matrix */
    val uniqueRatings = pairwiseRatings groupBy { rating => (rating.profileA, rating.profileB) } // Vector[Vector[PairwiseRating]]        
    val similarities = uniqueRatings map { similarity } // Vector[Similarity]    
    println("..found " + similarities.length + " similarities")
    
    // each similarity represents a unique (i,j) value in the similarity matrix - invert for fast look-up
    // uniqueness guaranteed by previous groupBy, but not statically provable... what should optiml do here? (nothing, because it's a foreach?)
    
    // TODO: this should be a SparseMatrix... only populated where we actually have a similarity value 
    //val similarityMatrix = SymmetricMatrix[Double](userRatings.length) // n x n where n is the number of unique users
    val similarityMatrix = DenseMatrix[Double](userRatings.length, userRatings.length)
    for (sim <- similarities) {
      similarityMatrix(sim.a, sim.b) = sim.value
    }
        
    /* compute prediction for desired user */
    val testRatings = userRatings filter { m => m(0,0) == testUser } // pretty inefficient unless this is fused...
    if (testRatings.length != 1) {
      println("CF error: could not find specified user")
      exit
    }
    
    val prefs = preferences(testUser, testRatings(0), similarityMatrix) // Vector[Double] from 0 to n, representing testUser's preference for profile i
    
    /* top N */
    //val (sorted, indices) = prefs.sortWithIndex 
    val sorted = prefs.sort    
    val topScores = sorted take N
    //val topUsers = indices take N
    // TODO: why is this type annotation required? inferencer ends up with Rep[DenseVector[Nothing]] otherwise
    val topUsers: Rep[DenseVector[Int]] = topScores map { e =>
      val tmp = prefs find { _ == e }
      tmp(0) } // HACK! need to implement sortWithIndex above    
    
    toc(topUsers)
    
    println("top N matches for user " + testUser + ": ")
    var i = 0
    while (i < topUsers.length) {
      println("  match: " + topUsers(i) + ", score: " + topScores(i))
      i += 1
    }      
  }
}
  
  
  
