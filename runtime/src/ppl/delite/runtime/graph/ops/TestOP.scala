package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets.Targets

/**
 * Author: Kevin J. Brown
 * Date: Oct 20, 2010
 * Time: 2:23:30 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class TestOP(kernel: String)(deps: DeliteOP*) extends OP_Executable {

  def task = kernel

  def id = System.identityHashCode(this).toString

  private[graph] var outputTypesMap = Map(Targets.Scala -> Map(id -> "Unit", "functionReturn" -> "Unit"))
  private[graph] var inputTypesMap = Map(Targets.Scala -> Map[String,String]())

  //initialize
  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  def cost = 0
  def size = 0
  def isDataParallel = false

}

class TestSingle[T: Manifest](kernel: String)(deps: DeliteOP*)(inputs: DeliteOP*)
        extends OP_Single("", kernel, null, null) {

  override val id = System.identityHashCode(this).toString
  outputTypesMap = Map(Targets.Scala -> Map(id -> manifest[T].toString, "functionReturn" -> manifest[T].toString))
  inputTypesMap = Map(Targets.Scala -> inputs.map(in => (in.getOutputs.head, in.outputType)).toMap)

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (input <- inputs.reverse) { //need a reverse to preserve order (addInput prepends)
    this.addInput(input, input.id)
  }

}

class TestForeach(func: String)(deps: DeliteOP*)(input: DeliteOP, free: DeliteOP*)
        extends OP_Foreach("", func, null, null) {

  override val id = System.identityHashCode(this).toString
  outputTypesMap = Map(Targets.Scala -> Map(id -> "Unit", "functionReturn" -> "Unit"))
  inputTypesMap = Map(Targets.Scala -> (Map(input.getOutputs.head->input.outputType) ++ free.map(in => (in.getOutputs.head, in.outputType))))

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (f <- free.reverse) { //need a reverse to preserve order (addInput prepends)
    this.addInput(f, f.id)
  }
  this.addInput(input, input.id)

}
