package ppl.delite.walktime.graph.ops

/**
 * Author: Kevin J. Brown
 * Date: Oct 20, 2010
 * Time: 2:23:30 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class TestOP(str: String)(deps: TestOP*) extends DeliteOP {

  def task = in => println(str)

  def getDependencies = deps

  def getConsumers = consumerList
  private var consumerList: Seq[TestOP] = Seq()
  def addConsumer(c: TestOP) { consumerList ++= Seq(c) }

  //initialize
  for (dep <- deps) {
    dep.addConsumer(this)  
  }

  def nested = null
  def cost = 0
  def size = 0
  def isDataParallel = false

}