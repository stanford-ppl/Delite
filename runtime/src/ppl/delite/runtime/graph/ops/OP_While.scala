package ppl.delite.runtime.graph.ops

abstract class OP_While extends OP_Control

class OP_BeginWhile(val id: String, val predicate: DeliteOP) extends OP_While {
  //TODO: this dependency management is overly conservative
  def makeChunk(idx: Int): OP_Control = {
    if (idx == 0) return this
    val chunk = new OP_BeginWhile(id.dropRight(1)+"_"+idx+"b", predicate)
    chunk.dependencyList = dependencyList
    chunk.consumerList = consumerList
    for (dep <- getDependencies) dep.addConsumer(chunk)
    for (c <- getConsumers) c.addDependency(chunk)
    chunk
  }
}

class OP_EndWhile(val id: String, val predicate: DeliteOP) extends OP_While {
  def makeChunk(idx: Int): OP_Control = {
    if (idx == 0) return this
    val chunk = new OP_EndWhile(id+"_"+idx, predicate)
    chunk.dependencyList = dependencyList
    chunk.consumerList = consumerList
    for (dep <- getDependencies) dep.addConsumer(chunk)
    for (c <- getConsumers) c.addDependency(chunk)
    chunk
  }
}
