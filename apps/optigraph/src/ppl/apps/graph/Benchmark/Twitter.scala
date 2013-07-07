package ppl.apps.graph.Benchmark

import ppl.dsl.optigraph._
import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.common.Record

object TwitterRunner extends OptiGraphApplicationRunner with Twitter 

trait Twitter extends OptiGraphApplication {

  def lcc(G: Rep[Graph], LCC: Rep[NodeProperty[Float]], threshold: Int) {
    Foreach(G.Nodes) { s =>
      var triangles = 0 
      var total = 0

      Foreach(G.InNbrs(s).filter(t => G.HasOutNbr(s,t))) { t =>
        Foreach(G.InNbrs(s).filter(u => G.HasOutNbr(s,u) && u != t)) { u =>
          //if(G.HasOutNbr(u,t)) {
            if (G.HasOutNbr(u,t)) {triangles += 1}
            if (G.HasOutNbr(t,u)) {triangles += 1}
            total += 2
          //}
        }
      }
      //LCC(s) = if(total < threshold) 0.0f else unit(1.0f) * triangles / total
      if (total < threshold) {
        LCC(s) = 0.0f
        //println("Computed LCC = " + LCC(s))
        //println("Total (" + total.value + ") was less than threshold")
      } else {
        LCC(s) = unit(1.0f) * (triangles) / (total)
        //println("Computed LCC = " + LCC(s) + " = " + triangles + " / " + total)
      }
    }
  }

  //TODO: Better to accumulate the number of retweets this node 
  def retweetCnt(G: Rep[Graph], RT: Rep[NodeProperty[Float]]) {
    Foreach(G.Nodes) { t =>
      RT(t) = G.InNbrs(t).length
      //println("inNbrs = " + G.InNbrs(t).length)
    }
  }

  /*
  def pagerank(G: Rep[Graph], PR: Rep[NodeProperty[Double]], e: Rep[Double], d: Rep[Double], max_iter: Rep[Int]) {
    val diff = Reduceable[Double](0.0)
    var cond = true
    var cnt = 0
    val N = G.NumNodes.asInstanceOf[Rep[Double]]
    PR.setAll(1.0/N)

    while(cond) {
      diff.setValue(0.0)
      Foreach(G.Nodes) { t =>
        val Val: Rep[Double] = ((1.0 - d) / N) + d * Sum(t.InNbrs){
          //w => PR(w) / deg(w.Id)//w.OutDegree
          w => PR(w) / w.OutDegree
        }
        PR <= (t,Val)

        diff += Math.abs(Val - PR(t))
      }
      PR.assignAll()
      cnt += 1
      cond = (diff.value > e) && (cnt < max_iter)
    }
    println("cnt:" + cnt + ",diff:" + diff)
  }
  */
  type EdgeList = Record {
    val fromId: Int
    val toId: Int
  }
  def EdgeList(_fromId: Rep[Int], _toId: Rep[Int]) = new Record{ 
    val fromId = _fromId;
    val toId = _toId;
  }
  def EdgeList(): Rep[EdgeList] = EdgeList(0,0)

  type Tweet = Record {
    val id: String
    val time: String
    val hour: Int
    val fromId: Int
    val toId: Int
    val retweet: Boolean
    val language: String
    val text: String
  }

  def Tweet(_id: Rep[String], _time: Rep[String], _hour: Rep[Int], _fromId: Rep[Int], _toId: Rep[Int], _retweet: Rep[Boolean], _language: Rep[String], _text: Rep[String]): Rep[Tweet] = new Record {
    val id = _id;
    val time = _time;
    val hour = _hour;
    val fromId = _fromId;
    val toId = _toId;
    val retweet = _retweet;
    val language = _language;
    val text = _text;
  }

  def Tweet(): Rep[Tweet] = Tweet("", "", 0, 0, 0, false, "", "")

  def main() {
 
    //val G = graph_load(args(0))
    val GArray = Graph(args(0), Tweet())
    //tic(GArray)
    val G = Graph.fromArray(GArray.map(e => (e.fromId,e.toId)))
    //toc(G)
    val lccprop : Rep[NodeProperty[Float]] = NodeProperty[Float](G, 0.0f)
    val retweet : Rep[NodeProperty[Float]] = NodeProperty[Float](G, 0.0f)
    tic(G)
    lcc(G, lccprop, 1)
    retweetCnt(G, retweet)
    toc(lccprop(node_new(0)),retweet(node_new(0)))
  }
}

