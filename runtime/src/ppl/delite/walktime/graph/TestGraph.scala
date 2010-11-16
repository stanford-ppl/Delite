package ppl.delite.walktime.graph

import ops._

/**
 * Author: Kevin J. Brown
 * Date: Oct 20, 2010
 * Time: 2:32:43 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class TestGraph extends MapGraph //test choice

//Scheduling & Optimized Execution Test
class SingleGraph extends DeliteTaskGraph {
  val base = "ppl.delite.walktime.graph.TestKernel"
  val node1 = new TestOP(base+"1a")()
  val node2 = new TestOP(base+"1b")(node1)
  val node3 = new TestOP(base+"1c")(node2)
  val node4 = new TestOP(base+"1d")(node3)
  val node5 = new TestOP(base+"2a")()
  val node6 = new TestOP(base+"2b")(node5)
  val node7 = new TestOP(base+"2c")(node6)
  val node8 = new TestOP(base+"2d")(node7)
  val node9 = new TestOP(base+"3")(node4,node8)

  _result = node9
}

//Simple Map Test
class MapGraph extends DeliteTaskGraph {
  val base = "ppl.delite.walktime.graph.TestKernel"
  val node1 = new TestSingle[Array[Int]](base+"Begin")()()
  val node2 = new TestMap(base+"Map")(node1)(node1, node1) //write output to input
  val node3 = new TestSingle[Unit](base+"End")(node1,node2)(node1)

  _result = node3
}

//Simple Reduce Test
class ReduceGraph extends DeliteTaskGraph {
  val base = "ppl.delite.walktime.graph.TestKernel"
  val node1 = new TestSingle[Array[Int]](base+"Begin")()()
  val node2 = new TestReduce[Int](base+"Reduce")(node1)(node1)
  val node3 = new TestSingle[Unit](base+"Print")(node2)(node2)

  _result = node3
}
