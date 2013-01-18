package ppl.apps.interop

import scala.collection.parallel.mutable.ParArray
import scala.collection.mutable.ArrayBuffer
import java.io._

object TwitterParallel {

  private var startTime:Long = 0
  private var endTime:Long = 0
  def tic() {
    startTime = System.currentTimeMillis
  }

  def toc(str: String) {
    endTime = System.currentTimeMillis
    println("Time for " + str + ": " + (endTime-startTime) + " [ms]")
  }

  case class Tweet(
    val id: Int,
    val time: Date,
    val hour: Int,
    val fromId: Int,
    val toId: Int,
    val rt: Boolean,
    val language: String,
    val text: String
  )

  def loadTable(fileName: String, sep: Char): ParArray[Tweet] = {
    val table = ArrayBuffer[Tweet]()
    val xfs = new BufferedReader(new FileReader(fileName))
    var line = xfs.readLine()
    while(line != null) {
      val elems = line.split(sep)
      table += Tweet(elems(0).toInt, Date(elems(1)), elems(2).toInt, elems(3).toInt, elems(4).toInt, elems(5).toBoolean, elems(6), elems(7))
      line = xfs.readLine()
    }
    //println("read " + table.length + " tweets")
    //println(table.mkString(","))
    table.toArray.par
  }

  def lcc(G: Graph, LCC: ParProperty[Double], threshold: Int) {
    for(s <- G.nodes) {
      var triangles = 0
      var total = 0

      for(t <- G.InNbrs(s).filter(t => G.HasOutNbr(s,t))) {
          for(u <- G.InNbrs(s).filter(u => G.HasOutNbr(s,u) && t != u)) {
              if (G.HasOutNbr(u,t)) {triangles += 1}
              if (G.HasOutNbr(t,u)) {triangles += 1}
              total += 2
          }
      }
      if (total < threshold) {
        LCC(s) = 0.0
        //println(LCC(s))
        //println("Computed LCC = " + LCC(s))
      } else {
        LCC(s) = triangles.toDouble / total.toDouble
        //println(LCC(s))
        //println("Computed LCC = " + LCC(s) + " = " + triangles + " / " + total)
      }
    }
  }
        
  def retweetCnt(G: Graph, RT: ParProperty[Double]) {
    for(t <- G.nodes) { 
      RT(t) = G.InNbrs(t).length
      //println("inNbrs = " + G.InNbrs(t).length)
      //println(G.InNbrs(t).length)
    }
  }

  private def printUsage() {
    println("Twitter <input file> [numIter]")
    exit(-1)
  }

  def main(args: Array[String]) {

    if(args.length < 1) printUsage()
    val numIter = if(args.length == 1) 1 else args(1).toInt

    // OptiQL
    val tweet = loadTable(args(0),'|')
    //println("Done reading the table")
    //val d = Date("2008-01-01")
    for(i <- 0 until numIter) {
      tic()
      val retweets = tweet.filter(t => t.language == "en" && t.time > Date("2008-01-01") && t.rt)
      val tweets= tweet.filter(t => t.language == "en")  //Add another filter condition
      toc("QL")
      //println("retweets:" + retweets.length)
      //println("tweets:" + tweets.length)

      // OptiGraph
      //val G = Graph.loadGraph(args(0))
      tic()
      val G = Graph.loadGraph(retweets.toArray.map(t => (t.fromId,t.toId)))
      toc("Graph Build")
      tic()
      val LCC = new ParProperty[Double](G, G.numNodes)
      val RT = new ParProperty[Double](G, G.numNodes)
      lcc(G, LCC, 1)
      retweetCnt(G, RT)
      toc("GL")

      // OptiML
      tic()
      val rt = ParVector.fromParArray(RT.data).t
      val scaledRT = (rt + 1.0).log.norm
      val X = ParMatrix.fromParArray(LCC.data,1)
      X.insertCol(0, ParVector.ones[Double](X.numRows).t)
      val theta = (X.t*X)*(X.t*scaledRT)
      
      val tweetHours = ParVector.fromParArray(tweets.map(_.hour.toDouble))
      val mT = tweetHours.mean
      val sdevT = tweetHours.stddev
      val distT = (((tweetHours-mT).square * (-1.0) / (2 * sdevT * sdevT)).exp) / Math.sqrt(2*scala.math.Pi*sdevT*sdevT)
      val retweetHours = ParVector.fromParArray(retweets.map(_.hour.toDouble))
      val mRT = retweetHours.mean
      val sdevRT = retweetHours.stddev
      val distRT = (((retweetHours-mRT).square * (-1.0) / (2 * sdevRT * sdevRT)).exp) / Math.sqrt(2*scala.math.Pi*sdevRT*sdevRT)
      toc("ML")

      println("mean(T):" + mT + ", stddev(T):" + sdevT)
      println("mean(RT):" + mRT + ", stddev(RT):" + sdevRT)
      println(theta.data.mkString(","))
      //println(retweets.map(_.hour).mkString("\n"))
    }
  }
}
