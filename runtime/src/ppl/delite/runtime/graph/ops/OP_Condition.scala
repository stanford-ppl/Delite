package ppl.delite.runtime.graph.ops

abstract class OP_Condition extends OP_Control

class OP_BeginCondition(val id: String, val predicate: DeliteOP) extends OP_Condition {
  //TODO: this dependency management is overly conservative
  def makeChunk(idx: Int): OP_Control = {
    if (idx == 0) return this
    val chunk = new OP_BeginCondition(id+"_"+idx, predicate)
    chunk.dependencyList = dependencyList
    chunk.consumerList = consumerList
    for (dep <- getDependencies) dep.addConsumer(chunk)
    for (c <- getConsumers) c.addDependency(chunk)
    chunk
  }
}

class OP_BeginElse(val id: String) extends OP_Condition {
  val predicate = null
  def makeChunk(idx: Int): OP_Control = {
    if (idx == 0) return this
    val chunk = new OP_BeginElse(id+"_"+idx)
    chunk.dependencyList = dependencyList
    chunk.consumerList = consumerList
    for (dep <- getDependencies) dep.addConsumer(chunk)
    for (c <- getConsumers) c.addDependency(chunk)
    chunk
  }
}

class OP_EndCondition(val id: String) extends OP_Condition {
  val predicate = null
  def makeChunk(idx: Int): OP_Control = {
    if (idx == 0) return this
    val chunk = new OP_EndCondition(id+"_"+idx)
    chunk.outputList = List(chunk.id)
    chunk.dependencyList = dependencyList
    chunk.consumerList = consumerList
    for (dep <- getDependencies) dep.addConsumer(chunk)
    for (c <- getConsumers) c.addDependency(chunk)
    chunk
  }
}
