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

class TestOP(kernel: String)(deps: DeliteOP*)
        extends DeliteOP {

  def task = kernel

  def id = System.identityHashCode(this).toString

  def supportsTarget(target: Targets.Value): Boolean = {
    if (target == Targets.Scala) true
    else false
  }

  def outputType(target: Targets.Value): String = {
    if (target == Targets.Scala) outputType
    else error("EOP does not support targets other than Scala")
  }

  override def outputType = "Unit"

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

class TestMap[T: Manifest](func: String)(deps: DeliteOP*)(output: DeliteOP, input: DeliteOP, free: DeliteOP*)
        extends OP_Map("", func, Map[Targets.Value,String](Targets.Scala -> manifest[T].toString)) {

  override val id = System.identityHashCode(this).toString

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (f <- free.reverse) { //need a reverse to preserve order (addInput prepends)
    this.addInput(f)
  }
  this.addInput(input)
  this.addInput(output)

}

class TestReduce[T: Manifest](func: String)(deps: DeliteOP*)(input: DeliteOP, free: DeliteOP*)
        extends OP_Reduce("", func, Map[Targets.Value,String](Targets.Scala -> manifest[T].toString)) {

  override val id = System.identityHashCode(this).toString

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (f <- free.reverse) {
    this.addInput(f)
  }
  this.addInput(input)

}

class TestMapReduce[T: Manifest](func: String)(deps: DeliteOP*)(input: DeliteOP, free: DeliteOP*)
        extends OP_MapReduce("", func, Map[Targets.Value,String](Targets.Scala -> manifest[T].toString)) {

  override val id = System.identityHashCode(this).toString

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (f <- free.reverse) {
    this.addInput(f)
  }
  this.addInput(input)

}

class TestZip[T: Manifest](func: String)(deps: DeliteOP*)(output: DeliteOP, input1: DeliteOP, input2: DeliteOP, free: DeliteOP*)
        extends OP_Zip("", func, Map[Targets.Value,String](Targets.Scala -> manifest[T].toString)) {

  override val id = System.identityHashCode(this).toString

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (f <- free.reverse) {
    this.addInput(f)
  }
  this.addInput(input2)
  this.addInput(input1)
  this.addInput(output)

}

class TestSingle[T: Manifest](kernel: String)(deps: DeliteOP*)(inputs: DeliteOP*)
        extends OP_Single("", kernel, Map[Targets.Value,String](Targets.Scala -> manifest[T].toString)) {

  override val id = System.identityHashCode(this).toString

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (input <- inputs.reverse) { //need a reverse to preserve order (addInput prepends)
    this.addInput(input)
  }

}

class TestForeach(func: String)(deps: DeliteOP*)(input: DeliteOP, free: DeliteOP*)
        extends OP_Foreach("", func, Map[Targets.Value,String](Targets.Scala -> "Unit")) {

  override val id = System.identityHashCode(this).toString

  for (dep <- deps) {
    this.addDependency(dep)
    dep.addConsumer(this)
  }

  for (f <- free.reverse) { //need a reverse to preserve order (addInput prepends)
    this.addInput(f)
  }
  this.addInput(input)

}
