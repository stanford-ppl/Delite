package ppl.apps.interop

import scala.collection.mutable.ArrayBuffer
import java.io._

object Date {
  def apply(str: String) = {
    val elems = str.split('-')
    new Date(elems(0).toInt, elems(1).toInt, elems(2).toInt)
  }
}

class Date(val year: Int, val month: Int, val day: Int) {
  def >(to: Date): Boolean = {
    val thisInt = (year << 9) + (month << 5) + day
    val toInt = (to.year << 9) + (to.month << 5) + to.day
    thisInt > toInt
  }
}

object Twitter {

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
    val fromId: Int,
    val toId: Int,
    val language: String,
    val text: String
  )

  def loadTable(fileName: String, sep: Char): Array[Tweet] = {
    val table = ArrayBuffer[Tweet]()
    val xfs = new BufferedReader(new FileReader(fileName))
    var line = xfs.readLine()
    while(line != null) {
      val elems = line.split(sep)
      table += Tweet(elems(0).toInt, Date(elems(1)), elems(2).toInt, elems(3).toInt, elems(4), elems(5))
      line = xfs.readLine()
    }
    //println("read " + table.length + " tweets")
    //println(table.mkString(","))
    table.toArray
  }

  def lcc(G: Graph, LCC: Property[Double], threshold: Int) {
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
        
  def retweetCnt(G: Graph, RT: Property[Double]) {
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
      val QLresult = tweet.filter(t => t.time > Date("2008-01-01") && t.language == "en")
      toc("QL")
      //for(i <- 0 until QLresult.length) {
      //  println(QLresult(i).fromId + "\t" + QLresult(i).toId)
      //}

      // OptiGraph
      //val G = Graph.loadGraph(args(0))
      tic()
      val G = Graph.loadGraph(QLresult.map(t => (t.fromId,t.toId)))
      toc("Graph Build")
      tic()
      val LCC = new Property[Double](G, G.numNodes)
      val RT = new Property[Double](G, G.numNodes)
      lcc(G, LCC, 1)
      retweetCnt(G, RT)
      toc("GL")

      // OptiML
      tic()
      val rt = Vector.fromArray(RT.data).t
      val scaledRT = (rt+1.0).log.norm
      val X = Matrix.fromArray(LCC.data,1)
      X.insertCol(0, Vector.ones[Double](X.numRows).t)
      val theta = (X.t*X)*(X.t*scaledRT)

      val m = rt.mean
      val sdev = rt.stddev
      val normdist = (((rt-m).square * (-1.0) / (2 * sdev * sdev)).exp) / Math.sqrt(2*scala.math.Pi*sdev*sdev)
      toc("ML")
      println("mean:" + m + ", stddev:" + sdev)
      println(theta.data.mkString(","))
    }
  }
}
