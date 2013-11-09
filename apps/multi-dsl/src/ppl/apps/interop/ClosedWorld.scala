package ppl.apps.interop

import scala.virtualization.lms.common.Record

import ppl.dsl.optiql.OptiQL_
import ppl.dsl.optigraph.OptiGraph_
import ppl.dsl.optiml.OptiML_
import ppl.delite.framework.datastructures.DeliteArray
import ppl.delite.framework.{BeginScopes,EndScopes}
import ppl.delite.framework.ScopeCommunication._

import ppl.dsl.optigraph.NodeProperty

object CloseWorldCompose {
  
  type Tweet = Record { val id: String; val time: String; val hour: Int; val fromId: Int; val toId: Int; val rt: Boolean; val language: String; val text: String }
  
  def main(args: Array[String]) {
    println("scala 1")
        
    BeginScopes() // marker to begin scope file
    
    val q = OptiQL_ {
      val tweets = Table.fromFile[Tweet](args(0), "\\|")    
      //dtic("all", tweets)
      dtic("optiql", tweets)
      val retweets = tweets Where(t => Date(t.time) >= Date("2008-01-01") && t.language == "en" && t.rt) 
      val engtweets = tweets Where(t => t.language == "en") 
      // println(result.toArray)
      dtoc("optiql", retweets.size, engtweets.size)
    
      DRef((retweets.toArray,engtweets.toArray))
    }
    
    // println("tweets: " + tweets.toString)
     
    val g = OptiGraph_ {      
      // val da1 = DeliteArray[Int](12)
      // val da2 = DeliteArray[Int](12)
      // da1(0) = 0; da2(0) = 3
      // da1(1) = 0; da2(1) = 1
      // da1(2) = 0; da2(2) = 2
      // da1(3) = 1; da2(3) = 4
      // da1(4) = 1; da2(4) = 6
      // da1(5) = 2; da2(5) = 0
      // da1(6) = 2; da2(6) = 3
      // da1(7) = 2; da2(7) = 4
      // da1(8) = 3; da2(8) = 0
      // da1(9) = 3; da2(9) = 5
      // da1(10) = 3; da2(10) = 1
      // da1(11) = 3; da2(11) = 2
      // val da = da1.zip(da2)((i,j) => t2(i,j))
      // val G = Graph.fromArray(da)
      val in = q.get
      //dtic("optigraph 1")
      val retweets = in._1
      val inEdges = retweets.map(t => (t.fromId,t.toId))
            
      //dtoc("optigraph 1", inEdges)
      val G = Graph.fromArray(inEdges)  
      
      // LCC
      // TODO: DeliteCodeGenRestage breaks when we have multiple Node Properties of different type
      dtic("optigraph", G)
      val LCC: Rep[NodeProperty[Double]] = NodeProperty[Double](G, 0.0)
      val RT: Rep[NodeProperty[Double]] = NodeProperty[Double](G, 0.0)
      val threshold = 1
      
      Foreach(G.Nodes) { s =>
        var triangles = 0 
        var total = 0
        
        Foreach(G.InNbrs(s).filter(t => G.HasOutNbr(s,t))) { t =>
          Foreach(G.InNbrs(s).filter(u => G.HasOutNbr(s,u) && u != t)) { u =>
            if (G.HasOutNbr(u,t)) {triangles += 1}
            if (G.HasOutNbr(t,u)) {triangles += 1}
            total += 2
          }
        }

        /*
        Foreach(G.InNbrs(s)) { t =>
          if (G.HasOutNbr(s,t)) {
            Foreach(G.InNbrs(s).filter(n => G.HasOutNbr(n,t))) { u =>
              if (G.HasOutNbr(s,u)) {
                if (G.HasOutNbr(u,t)) {triangles += 1}
                if (G.HasOutNbr(t,u)) {triangles += 1}
                total += 2
              }
            }
          }
        }
        */
        if (total < threshold) {
          LCC(s) = 0.0
          //println("Computed LCC = " + LCC(s))
          //println("Total (" + total.value + ") was less than threshold")
        } else {
          LCC(s) = (triangles.floatValueL) / (total.floatValueL)
          //println("Computed LCC = " + LCC(s) + " = " + triangles + " / " + total)
        }
      }
      
      // retweet count
      //TODO: Better to accumulate the number of retweets this node 
      Foreach(G.Nodes) { t =>
        RT(t) = G.InNbrs(t).length
        //println("inNbrs = " + G.InNbrs(t).length)
      }
            
      val RTarray = RT.toArray
      dtoc("optigraph", RTarray)
      DRef((LCC.toArray, RTarray, retweets, in._2))
    }
    
    OptiML_ {
      // unweighted linear regression
      val in = g.get
      val inA = tuple4_get1(in)
      val inB = tuple4_get2(in)
      val retweets = tuple4_get3(in) //Vector.fromArray(tuple4_get3(in))
      val tweets = tuple4_get4(in) //Vector.fromArray(tuple4_get4(in))
      
      dtic("optiml")

      // seems to be a bug in wip-develop with populating the spam vector in the restaged code (TODO: try in wip-restage2)
      
      //val spam = Vector("buy","cheap","sale","free","limited","textbook")
      val spam = Vector("buy","cheap","sale")
      
      val tweetsSpamFactor = Vector.fromArray(tweets.map(_.text)).map{t => 1.0 / (spam.count{s => t; t.contains(s)}.AsInstanceOf[Double] + 1.0)}
      //tweetsSpamFactor.slice(0,5).pprint
      val retweetsSpamFactor = Vector.fromArray(retweets.map(_.text)).map{t => 1.0 / (spam.count{s => t; t.contains(s)}.AsInstanceOf[Double] + 1.0)}

      //println("inA.length: " + inA.length)
      //println("inB.length: " + inB.length)


      //val x = Matrix.fromArray(DeliteArray[Double](200), numFeatures = 1)
      //val y = Vector.fromArray(DeliteArray[Double](200)).t
      //val x = readMatrix(args(0)+"/ml/linreg/q2x.dat")
      //val y = readVector(args(0)+"/ml/linreg/q2y.dat").t

      // -- normal optiml version      
      val x = Matrix.fromArray(inA, numFeatures = 1)/*.mutable*/
      val RT = Vector.fromArray(inB).t
      val xm = x.mutable
      xm.insertCol(0, Vector.ones(x.numRows).t)
      val X = xm.unsafeImmutable
      val RTlog = log(RT+1.0)
      val y = RTlog/sum(RTlog) // norm

      // compute unweighted linear regression on LCC, norm(RT)
      val a = (X.t*X)//.inv
      val b = X.t*y
      val theta = a*b

      // compute other statistics on all tweets
      val tweetHours = Vector.fromArray(tweets.map(_.hour.AsInstanceOf[Double]))*tweetsSpamFactor
      val m = mean(tweetHours)
      val sdev = sqrt(square(tweetHours-m).sum) / tweetHours.length
      val dist = ((square(tweetHours-m) * (-1.0) / (2*sdev*sdev)).exp) / sqrt(2*Pi*sdev*sdev)

      val rtHours = Vector.fromArray(retweets.map(_.hour.AsInstanceOf[Double]))*retweetsSpamFactor 
      val mRt = mean(rtHours)
      val sdevRt = sqrt(square(rtHours-mRt).sum) / rtHours.length
      val distRt = ((square(rtHours-mRt) * (-1.0) / (2*sdevRt*sdevRt)).exp) / sqrt(2*Pi*sdevRt*sdevRt)
      
      dtoc("optiml", dist, distRt, dist)
      //dtoc("all", dist, distRt, dist)

      // -- pre-transposed version
//      val X = DenseMatrix(Vector.onesf(inA.length), Vector.fromArray(inA))
//      val y = Vector.fromArray(inB).t
//      val a = (X*X.t)
//      val b = X*y

      //println("a numCols: " + a.numCols)
//      val theta = a*b
      //val theta = ((X.t*X).inv)*(X.t*y)
      //println("theta(0): " + theta(0))
      //println("theta(1): " + theta(1))
      theta.pprint
      println("mean: " + m)
      println("sdev: " + sdev)
      println("dist(0): " + dist(0))

      println("rt mean: " + mRt)
      println("rt sdev: " + sdevRt)
      println("rt dist(0): " + distRt(0))

      // linreg.weighted(readMatrix(args(0),readVector(args(1)).t))
      // println("optiml 2")
      42
    }    
    
    EndScopes() // marker to complete the scope file
    
    // unstaged blocks get executed immediately like normal
    println("scala 2")
  }
}
