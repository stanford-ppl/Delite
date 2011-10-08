package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._
import collection.mutable.ArrayBuffer

class MessageVertexImpl(val graph: Graph[MessageVertex,MessageEdge], val data: MessageData) extends MessageVertex {
  val _tasks = new ArrayBuffer[MessageVertex]()
  def edges = graph.edgesOf(this)
  def neighbors = graph.neighborsOf(this)
  def neighborsSelf = graph.neighborsSelfOf(this)

  def addTask(v: MessageVertex) = {
    _tasks += v
  }

  def tasks = new VerticesImpl[MessageVertex](_tasks.toArray)

  def clearTasks() = {
    _tasks.clear()
  }
  
  def target(e: MessageEdge) = e.target(this)
}