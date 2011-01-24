package ppl.dsl.optiml.datastruct.scala

import collection.mutable.ArrayBuffer

class MessageVertexImpl(val graph: Graph[MessageVertex,MessageEdge], val data: MessageData) extends MessageVertex {
  val _tasks = new ArrayBuffer[Vertex]()
  val edges = graph.edgesOf(this)
  val neighbors = graph.neighborsOf(this)

  def addTask(v: Vertex) = {
    tasks += v
  }

  def tasks = new VerticesImpl(_tasks.toArray)

  def clearTasks() = {
    _tasks.clear()
  }
  
  def target(e: MessageEdge) = e.target(this)
}