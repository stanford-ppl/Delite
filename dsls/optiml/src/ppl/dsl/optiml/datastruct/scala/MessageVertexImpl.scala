package ppl.dsl.optiml.datastruct.scala

import collection.mutable.ArrayBuffer

class MessageVertexImpl(val graph: Graph[MessageVertex,MessageEdge], val data: MessageData) extends MessageVertex {
  //val tasks = new ArrayBuffer(v)
  val edges = graph.edgesOf(this)
  val neighbors = graph.neighborsOf(this)

  //def addTask(v: V) {
  //  tasks += v
  //}
  def target(e: MessageEdge) = e.target(this)
}