package ppl.apps.interop

import ppl.dsl.optiql.OptiQL_
// import ppl.dsl.optigraph.OptiGraph_
import ppl.dsl.optiml.OptiML_
import ppl.delite.framework.datastructures.DeliteArray
import ppl.delite.framework.EndScopes

object CloseWorldCompose {
  
  def main(args: Array[String]) {
    println("scala 1")
    
    OptiQL_ {
      type Tweet = Record {
        val id: String
        val time: Date
        val fromId: Int
        val toId: Int
        val language: String
        val text: String
      }      
      def Tweet(_id: Rep[String], _time: Rep[Date], _fromId: Rep[Int], _toId: Rep[Int], _language: Rep[String], _text: Rep[String]): Rep[Tweet] = new Record {
        val id = _id;
        val time = _time;
        val fromId = _fromId;
        val toId = _toId;
        val language = _language;
        val text = _text;
      }      
      def emptyTweet(): Rep[Tweet] = Tweet("", Date(""), 0, 0, "", "")
            
      // type Tweet = Record{val fromId: Int; val toId: Int; val text: String}
      // val tweets: Rep[Table[Tweet]] = loadTweets() // elided
      val tweets = TableInputReader(args(0)+"/tweet.tbl", emptyTweet())      
      val result = tweets Where(t => t.time >= Date("2008-01-01") && t.language == "en") 
      // is there something in the desugaring scopes that ignores the block result?
      // without returnScopeResult, getting a block result of (), even the Scope result is of type R
      returnScopeResult(result.toArray)
    }
    
    /*
    OptiGraph_ {      
      type Tweet = Record{val fromId: Int; val toId: Int; val text: String}
      val in = lastScopeResult.asInstanceOf[Rep[DeliteArray[Tweet]]
      val G = Graph.fromArray(in.map(t => (t.fromId,t.toId)))      
      val LCC: Rep[NodeProperty[Float]] = NodeProperty[Float](G, 0.0f)
      val RT: Rep[NodeProperty[Int]] = NodeProperty[Int](G, 0)
      val threshold = 1
      
      // LCC
      Foreach(G.Nodes) { s =>
        var triangles = 0 
        var total = 0
        // TODO: use simpler undirected version
        Foreach(s.InNbrs) { t =>
          if (s.HasOutNbr(t)) {
            Foreach(s.InNbrs.filter(n => n.HasOutNbr(t))) { u =>
              if (s.HasOutNbr(u)) {
                if (u.HasOutNbr(t)) {triangles += 1}
                if (t.HasOutNbr(u)) {triangles += 1}
                total += 2
              }
            }
          }
        }
        if (total < threshold) LCC(s) = 0.0f else LCC(s) = triangles / total
      }

      // retweet cnt
      Foreach(G.Nodes) { t =>
        RT(t) = t.InNbrs.length
        println("inNbrs = " + t.InNbrs.length)
      }  
      
      (LCC.toArray, RT.toArray)
    }
    */
    
    /*
    OptiML_ {
      // unweighted linear regression
      val in = lastScopeResult.AsInstanceOf[(DeliteArray[Double],DeliteArray[Double])]
      val X = Matrix.fromArray(tuple2_get1(in), numFeatures = 1)/*.mutable*/
      val y = Vector.fromArray(tuple2_get2(in)).t
      X.insertCol(0, Vector.ones(X.numRows).t)
      val x2 = X.unsafeImmutable
      ((x2.t*x2).inv)*(x2.t*y)
      // linreg.weighted(readMatrix(args(0),readVector(args(1)).t))
      // println("got input from previous stage: " + previous(0).AsInstanceOf[DeliteArray[Int]]) 
      // println("optiml 2")
    }
    */
    
    EndScopes() // marker to complete the scope file
    
    // unstaged blocks get executed immediately like normal
    println("scala 2")
  }
}