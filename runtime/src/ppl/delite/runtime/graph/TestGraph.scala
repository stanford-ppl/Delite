package ppl.delite.runtime.graph

import ops._
import targets._

/**
 * Author: Kevin J. Brown
 * Date: Oct 20, 2010
 * Time: 2:32:43 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class TestGraph extends SingleGraph { //test choice
  val EOP = new EOP("eop", Map(Targets.Scala->Map("eop" -> "Unit", "functionReturn"->"Unit")), ("",""))
  EOP.addDependency(_result._1)
  _result._1.addConsumer(EOP)
  _result = (EOP, EOP.id)
}

//Scheduling & Optimized Execution Test
abstract class SingleGraph extends DeliteTaskGraph {
  val base = "ppl.delite.runtime.graph.TestKernel"
  val node1 = new TestOP(base+"1a")()
  val node2 = new TestOP(base+"1b")(node1)
  val node3 = new TestOP(base+"1c")(node2)
  val node4 = new TestOP(base+"1d")(node3)
  val node5 = new TestOP(base+"2a")()
  val node6 = new TestOP(base+"2b")(node5)
  val node7 = new TestOP(base+"2c")(node6)
  val node8 = new TestOP(base+"2d")(node7)
  val node9 = new TestOP(base+"3")(node4,node8)

  _ops ++= Map[String,DeliteOP]("node1"->node1, "node2"->node2, "node3"->node3, "node4"->node4, "node5"->node5,
                                "node6"->node6, "node7"->node7, "node8"->node8, "node9"->node9)
  _result = (node9, node9.id)
}

//simple foreach test
abstract class ForeachGraph extends DeliteTaskGraph {
  val base = "ppl.delite.runtime.graph.TestKernel"
  val node1 = new TestSingle[ArrayColl[Int]](base+"Begin")()()
  val node2 = new TestSingle[ArrayColl[Int]](base+"Out")()()
  val node3 = new TestForeach(base+"Foreach")(node1,node2)(node1,node2)
  val node4 = new TestSingle[Unit](base+"Print0")(node3)(node2)

  _ops ++= Map[String,DeliteOP]("node1"->node1, "node2"->node2, "node3"->node3, "node4"->node4)
  _result = (node4, node4.id)

}
