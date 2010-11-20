package ppl.delite.runtime.tests

import ppl.delite.runtime.walktime.graph.DeliteTaskGraph


/**
 * This test creates a graph, equivalent that what we expect the code gen
 * to produce
 */
object SimpleVectorTest {

  def main(args: Array[String]) {
    println("Creating a Delite Task Graph")
    val base = "ppl.delite.runtime.tests.kernels."
    val graph = new DeliteTaskGraph
    // create the zeros ops
    //graph.addOP(base+"MatrixZeros")
    // create vector addition op
    // create matrix addition op
    // create pprint ops
  }
}                              