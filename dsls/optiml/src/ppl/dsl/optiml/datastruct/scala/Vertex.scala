package ppl.dsl.optiml.datastruct.scala

import ppl.dsl.optila.datastruct.scala._
import collection.mutable.ArrayBuffer

class Vertex(val _graph: Graph, val _data: MessageData) {
  val _tasks = new ArrayBuffer[Vertex]
}