package ppl.delite.walktime.graph.ops

/**
 * Author: Kevin J. Brown
 * Date: Oct 20, 2010
 * Time: 2:23:30 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class TestOP(kernel: String)(deps: DeliteOP*) extends DeliteOP {

  def task = kernel

  def outputType = "Unit"

  //initialize
  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  def nested = null
  def cost = 0
  def size = 0
  def isDataParallel = false

}

class TestMap(kernel: String)(deps: DeliteOP*) extends OP_Map {

  override def task = kernel

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

}
