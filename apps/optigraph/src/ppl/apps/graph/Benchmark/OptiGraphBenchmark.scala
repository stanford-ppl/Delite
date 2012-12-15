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
    //for (arg <- args) {
    val arg = args(0)
    val start_time = wall_time()
    val G = graph_load(arg)
    val generation_time = wall_time() - start_time
    val lccprop : Rep[NodeProperty[Float]] = NodeProperty[Float](G, 0.0f)
    lcc(G, lccprop, 10)
    val lcc_time = wall_time() - (generation_time + start_time)
    val total_time = lcc_time + generation_time
    println("file: " + arg + " = generation: " + generation_time + " lcc: " + lcc_time + " total: " + total_time)
    //}
  }
}

