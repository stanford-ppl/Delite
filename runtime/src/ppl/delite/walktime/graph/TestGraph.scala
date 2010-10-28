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
  val node1 = new TestOP("op1a")()
  val node2 = new TestOP("op1b")(node1)
  val node3 = new TestOP("op1c")(node2)
  val node4 = new TestOP("op1d")(node3)
  val node5 = new TestOP("op2a")()
  val node6 = new TestOP("op2b")(node5)
  val node7 = new TestOP("op2c")(node6)
  val node8 = new TestOP("op2d")(node7)
  val node9 = new TestOP("op3")(node4,node8)

  def root = node9
}