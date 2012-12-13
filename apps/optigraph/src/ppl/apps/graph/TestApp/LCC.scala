package ppl.apps.graph.TestApp

import ppl.dsl.optigraph._
import ppl.delite.framework.DeliteApplication

object LCCRunner extends OptiGraphApplicationRunner with LCCApp
trait LCCApp extends OptiGraphApplication {

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

  def main() {
    val G = RandUniformGraph(5,15,1997L)
    val start: Float = 0.0f
    val lccprop : Rep[NodeProperty[Float]] = null
    lccprop = NodeProperty[Float](G, start)
    val threshold = 1
    lcc(G, lccprop, threshold)
  }
}
