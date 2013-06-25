package ppl.apps.interop

import scala.virtualization.lms.common.Record
import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

// AKS: temporarily comments out during scala-virtualized scope/record testing

object RegressionRunner extends OptiMLApplicationRunner with Regression

trait Regression extends OptiMLApplication {
  /*

  // file format is m lines with n floats per line, each float seperated by 2 spaces
  // (same as matlab .dat)
  def print_usage = {
    println("Usage: Regression <input LCC> <input RT> <input tweets> <input retweets>")
    exit(-1)
  }

  def norm(x: Rep[DenseVector[Double]]) = x / x.sum
  
  def stddev(x: Rep[DenseVector[Double]]) = {
    val m = mean(x)
    val y = square((x-m)).sum
    sqrt(y)/x.length
  }

  def parseTweet(v: Rep[DenseVector[String]]) = {
    new Record {
      val id = v(0).toInt
      val time = v(1)
      val hour = v(2).toInt
      val fromId = v(3).toInt
      val toId = v(4).toInt
      val rt = (v(5) == "true") 
      val language = v(6)
      val text = v(7)
    }
  }
  */
  def main() = {                                                 
    /*
    if (args.length < 4) print_usage


    val inA = readMatrix(args(0))
    val inB = readVector(args(1))
    val inC = readVector(args(2), parseTweet, "\\Q" + "|" + "\\E")
    val inD = readVector(args(3))

    tic(inA,inB,inC,inD)
    
    val rt = inB.t
    val y = norm(log(rt + 1))
    val xm = inA.mutable
    xm.insertCol(0, Vector.ones(xm.numRows).t)
    val X = xm.Clone
    //println("y: " + y.length)
    //println("X: " + X.numRows + " " + X.numCols)
    val theta = (X.t*X)*(X.t*y)

    val retweets = inD.map(e => e * 1.0 / 1.0)
    val mRT = mean(retweets)
    val sdevRT = stddev(retweets)
    val normdistRT = ((square((retweets-mRT)) * (-1.0) / (2 * sdevRT*sdevRT)).exp) / sqrt(2*Pi*sdevRT*sdevRT) 
    //println("theta: " + theta.length)
    
    val tweets = inC.map(_.hour.AsInstanceOf[Double])
    val m = mean(tweets)
    val sdev = stddev(tweets)
    val normdist = ((square((tweets-m)) * (-1.0) / (2 * sdev*sdev)).exp) / sqrt(2*Pi*sdev*sdev)
    
    toc(theta, normdistRT, normdist)
    theta.pprint
    println("mean: " + m)
    println("std dev: " + sdev)
    */
  }
}
