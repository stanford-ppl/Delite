package ppl.delite.walktime.graph

import ops.TestOP

/**
 * Author: Kevin J. Brown
 * Date: Oct 20, 2010
 * Time: 2:32:43 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class TestGraph extends DeliteTaskGraph {
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

  def root = node9
}