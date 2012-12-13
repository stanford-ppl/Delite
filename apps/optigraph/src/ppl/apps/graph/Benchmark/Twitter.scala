package ppl.apps.graph.Benchmark

import ppl.dsl.optigraph._
import ppl.delite.framework.DeliteApplication

object TwitterRunner extends OptiGraphApplicationRunner with Twitter 

trait Twitter extends OptiGraphApplication {

  def lcc(G: Rep[Graph], LCC: Rep[NodeProperty[Float]], threshold: Int) {
    Foreach(G.Nodes) { s =>
      var triangles = 0 
      var total = 0

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
      if (total < threshold) {
        LCC(s) = 0.0f
        println("Computed LCC = " + LCC(s))
        //println("Total (" + total.value + ") was less than threshold")
      } else {
        LCC(s) = (triangles) / (total)
        println("Computed LCC = " + LCC(s) + " = " + triangles + " / " + total)
      }
    }
  }

  //TODO: Better to accumulate the number of retweets this node 
  def retweetCnt(G: Rep[Graph], RT: Rep[NodeProperty[Int]]) {
    Foreach(G.Nodes) { t =>
      RT(t) = G.InNbrs(t).length
      println("inNbrs = " + G.InNbrs(t).length)
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

  def main() {
    val start_time = wall_time()
    val G = graph_load(args(0))
    val generation_time = wall_time() - start_time
    val lccprop : Rep[NodeProperty[Float]] = NodeProperty[Float](G, 0.0f)
    val retweet : Rep[NodeProperty[Int]] = NodeProperty[Int](G, 0)
    //val rank : Rep[NodeProperty[Double]] = NodeProperty[Double](G, 0.0)
    lcc(G, lccprop, 1)
    //pagerank(G, rank, 0.001, 0.85, 6)
    retweetCnt(G, retweet)
    val lcc_time = wall_time() - (generation_time + start_time)
    val total_time = lcc_time + generation_time
    println("file: " + args(0) + " = generation: " + generation_time + " lcc: " + lcc_time + " total: " + total_time)
  }
}

