package ppl.apps.graph.Benchmark

import ppl.dsl.optigraph._
import ppl.delite.framework.DeliteApplication
//import scala.virtualization.lms.common.FractionalOps

object GraphAppRunner extends OptiGraphApplicationRunner with OptiGraphBenchmark

/*
 *  -----------------------------------------
 *  OptiGraph benchmark using LCC
 *  -----------------------------------------
*/

trait OptiGraphBenchmark extends OptiGraphApplication {

/*
  // For (node.inNbrs & node.outNbrs) {triangles / (possible)}
Proc localClusterCoefficient(G: Graph, LCC: N_P<Float>(G), threshold:
Int) : Float {
  Foreach (s: G.Nodes) {
    Int triangles = 0;
    Int total = 0;

    Foreach (t:s.InNbrs) {
      If(t.IsNbrFrom(s)) {
        Foreach (u:s.InNbrs) (u > t) {
          If  (u.IsNbrFrom(s)) {
            If  (t.IsNbrFrom(u)) {triangles += 1;}
            If  (u.IsNbrFrom(t)) {triangles += 1;}
            total += 2;
          }
        }
      }
    }
    If (total < threshold) {
      s.LCC = 0.0;
    }
    Else {
      s.LCC = ((Float) triangles) / ((Float) total);
      */

  def lcc(G: Rep[Graph], LCC: Rep[NodeProperty[Float]], threshold: Int) {
    Foreach(G.Nodes) { s =>
      val triangles = Reduceable[Int](0)
      val total = Reduceable[Int](0)
      val sNeighbors = s.OutNbrs.toSet()

      Foreach(s.InNbrs) { t =>
        val tNeighbors = t.OutNbrs.toSet()
        if (sNeighbors.Has(t)) {
          Foreach(s.InNbrs.filter(n => n.OutNbrs.toSet().Has(t))) { u =>
            if (sNeighbors.Has(u)) {
              val uNeighbors = u.OutNbrs.toSet()
              if (uNeighbors.Has(t)) {triangles += 1}
              if (tNeighbors.Has(u)) {triangles += 1}
              total += 2
            }
          }
        }
      }
      if (total.value < threshold) {
        LCC(s) = 0.0f
	println("Computed LCC = " + LCC(s))
        //println("Total (" + total.value + ") was less than threshold")
      } else {
        LCC(s) = (triangles.value.AsInstanceOf[Float]) / (total.value.AsInstanceOf[Float])
        println("Computed LCC = " + LCC(s) + " = " + triangles.value + " / " + total.value)
      }
    }
  }

  def main() {
    //val G = graph_load(args(0))
    val G = RandUniformGraph(30,800,1996L)

    val lccprop : Rep[NodeProperty[Float]] = null
    lccprop = NodeProperty[Float](G, 0.0f)
    val threshold = 10
    lcc(G, lccprop, threshold)
  }
}

