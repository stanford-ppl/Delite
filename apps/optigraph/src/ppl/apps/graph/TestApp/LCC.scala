package ppl.apps.graph.TestApp

import ppl.dsl.optigraph._
import ppl.delite.framework.DeliteApplication

object LCCRunner extends OptiGraphApplicationRunner with LCCApp
trait LCCApp extends OptiGraphApplication {

  def lcc(G: Rep[Graph], LCC: Rep[NodeProperty[Float]], threshold: Int) {
    Foreach(G.Nodes) { s =>
      var total = 0
      var triangles = 0
      val sNeighbors = s.OutNbrs.toSet()
      // println("a")
      Foreach(s.InNbrs) { t =>
        // println("b")
        val tNeighbors = t.OutNbrs.toSet()
        if (sNeighbors.Has(t)) {
          Foreach(s.InNbrs.filter(n => n.OutNbrs.toSet().Has(t))) { u =>
            // println("c")
            if (sNeighbors.Has(u)) {
              val uNeighbors = u.OutNbrs.toSet()
              if (uNeighbors.Has(t)) {triangles += 1}
              if (tNeighbors.Has(u)) {triangles += 1}
              // println("got here")
              total += 2
            }
          }
        }
      }
      
      if (total < threshold) {
        LCC(s) = 0.0f
        println("Total (" + total + ") was less than threshold")
      } else {
        LCC(s) = (triangles) / (total)
        println("Computed LCC = " + LCC(s))
      }            
    }
    
  }

  def main() {
    val G = RandUniformGraph(5,15,1997L)
    val start: Float = 0.0f
    val lccprop : Rep[NodeProperty[Float]] = null
    lccprop = NodeProperty[Float](G, start)
    val threshold = 1
    lcc(G, lccprop, threshold)
  }
}